;;; tracker.el --- emacs command/event tracking

;; Copyright 2013 Birdseye Software.

;; Authors: <tavis at birdseye-sw com>, <roman at birdeseye-sw com>
;; Version: 0.0.1
;; Package-version: 0.0.1
;; Package-Requires:
;; Keywords: tooling
;; URL: http://github.com/BirdseyeSoftware/tracker.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;; Comentary:
;;
;; This is a work in progress
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'which-func)

(defcustom tracker-use-logfile t
  "If true, will store events on tracker-ephemeral-dir"
  :group 'tracker)
(defvar *tracker-logfile-path* "~/.emacs_tracker.log")

(defcustom tracker-memory-cache-flush-timeout 4
  "Number of seconds after which to flush the cache.
   The hooks in *tracker-memory-cache-flush-hook* are called at this time"
  :group 'tracker)
(defcustom tracker-logfile-flush-timeout 20
  "Number of seconds after which to flush the log file"
  :group 'tracker)
(defcustom tracker-ui-idle-event-timeout 10
  "Number of seconds after which to record an idle timeout event"
  :group 'tracker)

(defvar *tracker-memory-cache-flush-timer* nil)
(defvar *tracker-memory-cache* nil "The buffer for unflushed tracker events")
(defvar *tracker-memory-cache-flush-hook* '() "")

(defvar *tracker-logfile-flush-timer* nil)
(defvar *tracker-logfile-cache* nil
  "A separate cache for log events that are going to be flushed to a log file")

(defvar *tracker-ui-idle-event-timer* nil)

(defvar *tracker-timers* '() "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Utils

(defun tracker/-get-context ()
  (condition-case nil
      (let ((context
             (cond
              ;; ((member major-mode '(lisp-interaction-mode emacs-lisp-mode))
              ;;  (tracker/-get-defun-name))
              (t (which-function)))))
        (if context
            (set-text-properties 0 (length context) nil context))
        context)
    (error nil)))

(defun tracker/-get-window-number ()
  (when (boundp 'window-numbering-table)
    (let ((window (selected-window))
          (frame (selected-frame)))
      (gethash window
               (cdr (gethash frame window-numbering-table))))))

(defun tracker/-should-add-extra-context (command)
  (not (memq command
             '(right-char
               left-char
               previous-line next-line
               evil-esc
               self-insert-command
               term-send-raw
               org-self-insert-command
               evil-next-line
               evil-previous-line
               coffee-dedent-line-backspace
               term-send-up
               term-send-down
               scroll-down-command
               scroll-up-command
               evil-forward-char
               evil-backward-char
               evil-normal-state
               ido-exit-minibuffer
               org-delete-backward-char
               coffee-newline-and-indent
               move-end-of-line
               move-beginning-of-line
               dss/coffee-electric-pair
               backward-word
               isearch-printing-char
               ))))

(defun tracker/-create-event-record (&optional event)
  (interactive)
  (let* ((event (or event ;; real-last-command
                    (list :command this-command)))
         (base-record (list
                       :time (format-time-string "%Y-%m-%dT%H:%M:%S")
                       :event event
                       :buffer (buffer-name (current-buffer))
                       :line (line-number-at-pos)
                       :column (current-column))))
    (if (tracker/-should-add-extra-context this-command)
        (let ((filename (buffer-file-name (current-buffer))))
          (append base-record (list :context (tracker/-get-context)
                                    :file-path filename
                                    :major-mode major-mode
                                    :vc-status (and filename (vc-state filename))
                                    :vc-revision (and filename
                                                      (vc-working-revision filename))
                                    :frame-name (frame-parameter nil 'name)
                                    :window-number (tracker/-get-window-number))))
      base-record)))

(defun tracker/-register-timer (timer)
  (add-to-list '*tracker-timers* timer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Memory Cache

(defun tracker/-memory-cache-record-event (&optional event)
  (let ((event-record (tracker/-create-event-record event)))
    (setq *tracker-memory-cache*
          (cons event-record *tracker-memory-cache*))))


(defun tracker/-memory-cache-flush-callback ()
  (run-hook-with-args '*tracker-memory-cache-flush-hook* *tracker-memory-cache*)
  (setq *tracker-memory-cache* nil))

(defun tracker/-memory-cache-start ()
  (setq *tracker-memory-cache-flush-timer*
        (run-with-idle-timer tracker-memory-cache-flush-timeout
                             t 'tracker/-memory-cache-flush-callback))
  (tracker/-register-timer *tracker-memory-cache-flush-timer*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Log File

(defun tracker/-logfile-get-events-from-memory-cache (evs)
  (setq *tracker-logfile-cache* (append evs *tracker-logfile-cache*)))

(defun tracker/-logfile-serialize-event-list (event-list)
  (mapconcat 'prin1-to-string event-list "\n"))

(defun tracker/-logfile-flush-events-callback ()
  (setq *tracker-logfile-cache* (nreverse *tracker-logfile-cache*))
  (let ((output (tracker/-logfile-serialize-event-list *tracker-logfile-cache*)))
    (setq *tracker-logfile-cache* nil)
    (append-to-file output nil *tracker-logfile-path*)
    (append-to-file "\n" nil *tracker-logfile-path*)
    output))

(defun tracker/-logfile-start ()
  (add-hook '*tracker-memory-cache-flush-hook*
            'tracker/-logfile-get-events-from-memory-cache)
  (setq *tracker-logfile-flush-timer*
        (run-with-idle-timer tracker-logfile-flush-timeout
                             t 'tracker/-logfile-flush-events-callback))
  (tracker/-register-timer *tracker-logfile-flush-timer*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Idle Event

(defun tracker/-idle-event-callback ()
  (tracker/-memory-cache-record-event 'idle))

(defun tracker/-idle-event-start-listener ()
  ;; When the user interaction has gone idle, track an idle event
  (setq *tracker-ui-idle-event-timer*
        (run-with-idle-timer tracker-ui-idle-event-timeout
                             t 'tracker/-idle-event-callback))

  (tracker/-register-timer *tracker-ui-idle-event-timer*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Org timer events

(defun tracker/-org-clock-in-hook ()
  (tracker/record-event (list :org-clock-in (org-id-get))))

(defun tracker/-org-clock-out-hook ()
  (tracker/record-event (list :org-clock-out (org-id-get))))

(add-hook 'org-clock-in-hook  'tracker/-org-clock-in-hook)
(add-hook 'org-clock-out-hook 'tracker/-org-clock-out-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tracker/-cancel-timers ()
  (dolist (timer *tracker-timers*)
    (when timer
      (cancel-timer timer))))

(defalias 'tracker/-pre-command-hook 'tracker/-memory-cache-record-event)
(defalias 'tracker/record-event 'tracker/-memory-cache-record-event)


;;;###autoload
(defun tracker/enable ()
  (interactive)
  (tracker/-cancel-timers)
  (add-hook 'pre-command-hook 'tracker/-pre-command-hook)
  (tracker/-memory-cache-start)
  (tracker/-idle-event-start-listener)
  (when tracker-use-logfile
    (tracker/-logfile-start)))

;;;###autoload
(defun tracker/disable ()
  (interactive)
  (remove-hook 'pre-command-hook 'tracker/-pre-command-hook)
  (tracker/-cancel-timers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tracker)

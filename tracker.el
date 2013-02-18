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

(defcustom tracker-ephemeral-dir "~/.emacs.d" "")
(defvar *tracker-idle-timeout* 10)
(defvar *tracker-flush-timeout* 20)
(defvar *tracker-idle-timer* nil)
(defvar *tracker-idle-cache-flush-timer* nil)
(defvar *tracker-log-file* (concat tracker-ephemeral-dir "/.tracker.log"))
(defvar *tracker-cache* nil "The buffer for unflushed tracker events")

;; (defun tracker/-get-defun-name ()
;;   "A cheaper alternative to (which-function) in lisps."
;;   (interactive)
;;   (save-excursion
;;     (dss/out-sexp)
;;     (forward-to-word 1)
;;     (forward-sexp)
;;     (skip-chars-forward " ")
;;     (mark-sexp 1)
;;     (let ((defun-name (buffer-substring (region-beginning) (region-end))))
;;       (if defun-name
;;           (set-text-properties 0 (length defun-name) nil defun-name))
;;       defun-name)))

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
        (append base-record (list :context (tracker/-get-context)
                                  :frame-name (frame-parameter nil 'name)
                                  :window-number (tracker/-get-window-number)))
      base-record)))

(defun tracker/-record-event (&optional event)
  (let ((event-record (tracker/-create-event-record event)))
    (setq *tracker-cache*
          (cons event-record *tracker-cache*))))

(defalias 'tracker/-pre-command-hook 'tracker/-record-event)
(defun tracker/-record-idle-time ()
  (tracker/-record-event 'idle))

(defun tracker/-serialize-event-list (event-list)
  (mapconcat 'prin1-to-string event-list "\n"))

(defun tracker/-flush-event-cache ()
  (setq *tracker-cache* (nreverse *tracker-cache*))
  (let ((output (tracker/-serialize-event-list *tracker-cache*)))
    (setq *tracker-cache* nil)
    (append-to-file output nil *tracker-log-file*)
    (append-to-file "\n" nil *tracker-log-file*)
                                        ;(message "flush tracker logs")
    output))

(defun tracker/-idle-timer-hook ()
  (tracker/-record-event 'idle))

(defun tracker/-cancel-timers ()
  (when *tracker-idle-cache-flush-timer*
    (cancel-timer *tracker-idle-cache-flush-timer*))
  (when *tracker-idle-timer*
    (cancel-timer *tracker-idle-timer*)))

;;;###autoload
(defun tracker/enable ()
  (interactive)
  (tracker/-cancel-timers)
  (add-hook 'pre-command-hook 'tracker/-pre-command-hook)
  (setq *tracker-idle-cache-flush-timer*
        (run-with-idle-timer *tracker-flush-timeout*
                             t 'tracker/-flush-event-cache))
  (setq *tracker-idle-timer*
        (run-with-idle-timer *tracker-idle-timeout*
                             t 'tracker/-idle-timer-hook)))
;;;###autoload
(defun tracker/disable ()
  (interactive)
  (remove-hook 'pre-command-hook 'tracker/-pre-command-hook)
  (tracker/-cancel-timers))

(defun tracker/-org-clock-in-hook ()
  (tracker/-record-event (list :org-clock-in (org-id-get))))

(defun tracker/-org-clock-out-hook ()
  (tracker/-record-event (list :org-clock-out (org-id-get))))

(add-hook 'org-clock-in-hook  'tracker/-org-clock-in-hook)
(add-hook 'org-clock-out-hook 'tracker/-org-clock-out-hook)

(provide 'tracker)

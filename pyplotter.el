(require 'elpy)

(defvar datascience-current-session)
(defvar datascience-image-counter)
(defvar datascience-plot-dir)
(defvar datascience-code-name)
(defvar datascience-code-buffer)

(defconst datascience-internal-vars '(datascience-current-session
                                      datascience-image-counter
                                      datascience-plot-dir
                                      datascience-code-buffer))

(dolist (var datascience-internal-vars)
  (make-variable-buffer-local var))


(defun datascience-initialize ()
  "Initialize data science IDE"
  (progn
    (setq datascience-plot-dir (concat (buffer-file-name) "ds-plots"))
    (ignore-errors (make-directory datascience-plot-dir))
    (ignore-errors (image-dired datascience-plot-dir))
    ))


(defun datascience-update-plots ()
  (progn
    (let ((sw (selected-window)))
      (set-buffer (concat datascience-code-name "ds-plots"))
      (revert-buffer)
      (dired-mark-subdir-files)
      (image-dired-display-thumbs)
      (select-window sw)))
    (set-buffer datascience-code-buffer))

(defun datascience-shell-send-region-or-buffer ()
  "Send the active region or the buffer to the Python shell.

Send plots to seperate buffer. Display dataframes in seperate
buffer. 

If there is an active region, send that. Otherwise, send the
whole buffer.

In Emacs 24.3 and later, without prefix argument, this will
escape the Python idiom of if __name__ == '__main__' to be false
to avoid accidental execution of code. With prefix argument, this
code is executed."
  (interactive)
  ;; Ensure process exists
  (elpy-shell-get-or-create-process)
    (let ((if-main-regex "^if +__name__ +== +[\"']__main__[\"'] *:")
          (has-if-main nil)
          (plotting-string "plt.show"))
      (if (use-region-p)
           (let ((selected-region (elpy-shell--region-without-indentation
                                   (region-beginning) (region-end))))
             (if (string-match "plt\.show.+?\)" selected-region)
                 (setq datascience-image-counter (+ datascience-image-counter 1)))
             (let ((region (replace-regexp-in-string
                            "plt\.show.+?\)" (format "plt.savefig('%s/%d')"
                                                     datascience-plot-dir
                                                     datascience-image-counter)
                            selected-region)))
            (setq has-if-main (string-match if-main-regex region))
            (when (string-match "\t" region)
              (message "Region contained tabs, this might cause weird errors"))
            (python-shell-send-string region)))
        (save-excursion
          (goto-char (point-min))
          (setq has-if-main (re-search-forward if-main-regex nil t)))
        (python-shell-send-buffer arg))
      (elpy-shell-display-buffer)
      (datascience-update-plots)
      (datascience-update-plots)
      (when has-if-main
        (message (concat "Removed if __main__ == '__main__' construct, "
                         "use a prefix argument to evaluate.")))))


(define-minor-mode datascience-mode
  "Turn Elpy into a data science IDE"
  :lighter " datascience"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'datascience-shell-send-region-or-buffer)
            map)

  (if datascience-mode
      ;; Enabling.
      (progn
        (setq datascience-image-counter 0)
        (setq datascience-code-buffer (current-buffer))
        (setq datascience-code-name (file-name-nondirectory (buffer-file-name)))
        (ignore-errors datascience-initialize)
        (set-buffer datascience-code-buffer))

    ;; Disabling.
    ))




;;;###autoload
;;;(add-hook 'elpy-mode-hook 'datascience-mode)

(provide 'datascience)

;;; Use this to set variables
;;; https://www.emacswiki.org/emacs/fill-column-indicator.el

          
;;; Commands to think about
;;; dired-mark-subdir-files
;;; image-dired-display-thumbs

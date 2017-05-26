(require 'elpy)
(defvar datascience-current-session)
(defvar datascience-image-counter)
(defvar datascience-plot-dir)

(defconst datascience-internal-vars '(datascience-current-session
                                      datascience-image-counter
                                      datascience-plot-dir))

(dolist (var datascience-internal-vars)
  (make-variable-buffer-local var))

(defun datascience-initialize ()
  "Initialize data science IDE"
  (progn
    (setq datascience-plot-dir (concat (buffer-file-name) "ds-plots"))
    (make-directory datascience-plot-dir)
    (image-dired datascience-plot-dir)
    (message "****TEST****")))


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
        (setq datascience-plot-dir "ds-plots")
        (datascience-initialize))

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

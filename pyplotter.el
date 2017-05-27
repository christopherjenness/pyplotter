(require 'elpy)

(defvar pyplotter-current-session)
(defvar pyplotter-image-counter)
(defvar pyplotter-plot-dir)
(defvar pyplotter-code-name)
(defvar pyplotter-code-buffer)

(defconst pyplotter-internal-vars '(pyplotter-current-session
                                    pyplotter-image-counter
                                    pyplotter-plot-dir
                                    pyplotter-code-buffer))

;(dolist (var pyplotter-internal-vars)
;  (make-variable-buffer-local var))


(defun plyplotter-initialize ()
  "Initialize data science IDE"
  (progn
    (message pyplotter-code-name)
    (message pyplotter-plot-dir)
    (message pyplotter-plot-dir)
    (make-directory pyplotter-plot-dir)
    (image-dired pyplotter-plot-dir)
    ))


(defun pyplotter-update-plots ()
  (progn
    (let ((sw (selected-window))
          (cb (current-buffer)))
      (set-buffer (concat pyplotter-code-name "ds-plots"))
      (revert-buffer)
      (dired-unmark-all-marks)
      (dired-mark-files-regexp "\.png")
      (image-dired-display-thumbs)
      (select-window sw)
      (switch-to-buffer cb)))
    )

(defun pyplotter-shell-send-region-or-buffer ()
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
                 (setq pyplotter-image-counter (+ pyplotter-image-counter 1)))
             (let ((region (replace-regexp-in-string
                            "plt\.show.+?\)" (format "plt.savefig('%s/%d')"
                                                     pyplotter-plot-dir
                                                     pyplotter-image-counter)
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
      (pyplotter-update-plots)
      (pyplotter-update-plots)      
      (when has-if-main
        (message (concat "Removed if __main__ == '__main__' construct, "
                         "use a prefix argument to evaluate.")))
      (pyplotter-update-plots)))


(define-minor-mode pyplotter-mode
  "Turn Elpy into a data science IDE"
  :lighter " pyplotter"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'pyplotter-shell-send-region-or-buffer)
            map)

  (if pyplotter-mode
      ;; Enabling.
      (progn
        (message "***ONE***")
        (setq pyplotter-image-counter 0)
        (setq pyplotter-code-name (file-name-nondirectory (buffer-file-name)))
        (setq pyplotter-plot-dir (concat (buffer-file-name) "ds-plots"))
        (ignore-errors make-directory pyplotter-plot-dir)
        (let ((sw (selected-window))
              (cb (current-buffer)))
          (image-dired pyplotter-plot-dir)
          (select-window sw)
          (switch-to-buffer cb)))

    ;; Disabling.
    ))

;;;###autoload
;;;(add-hook 'elpy-mode-hook 'pyplotter-mode)

(provide 'pyplotter)

;;; Use this to set variables
;;; https://www.emacswiki.org/emacs/fill-column-indicator.el

          
;;; Commands to think about
;;; dired-mark-subdir-files
;;; image-dired-display-thumbs

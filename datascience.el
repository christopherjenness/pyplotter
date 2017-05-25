(make-variable-buffer-local
 (defvar current-session nil
   "Current session for caching purposes"))

(make-variable-buffer-local
 (defvar image-counter 0
   "Current session for caching purposes"))

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
                 (setq image-counter (+ image-counter 1)))
             (let ((region (replace-regexp-in-string
                            "plt\.show.+?\)" (format "plt.savefig('%d')" image-counter)
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
            map))

;;;###autoload
(add-hook 'elpy-mode-hook 'datascience-mode)

(provide 'datascience-mode)

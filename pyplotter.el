;;; pyplotter.el --- Convenient handling of python plots

;; Copyright (c) 2017 Christopher Jenness

;; Author: Alp Aker <christopherjenness@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; By default, python plots popup when using elpy, which is inconvenient
;; This package moves plots to a thumbnail viewer for easy browsing

;;; Code:

(require 'elpy)

(defvar pyplotter-current-session)
(defvar pyplotter-image-counter)
(defvar pyplotter-plot-dir)
(defvar pyplotter-code-name)
(defvar pyplotter-code-buffer)

(defconst pyplotter-internal-vars '(pyplotter-current-session
                                    pyplotter-image-counter
                                    pyplotter-plot-dir
                                    pyplotter-code-buffer
                                    pyplotter-code-name))


(defvar pyplotter-helper
"import pandas as pd
def check_dataframes():
    for variable in globals().keys():
        if isinstance(globals()[variable], pd.DataFrame):
            globals()[variable].to_csv('dfs/{variable}.csv'.format(variable=variable))

check_dataframes()
")

;; Useful org functions:
;; org-table-insert-hline
;; org-table-create-or-convert-from-region


(dolist (var pyplotter-internal-vars)
  (make-variable-buffer-local var))


(defun pyplotter-update-plots ()
  "Update plots in the thumbnail viewer."
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

Send plots to seperate buffer.  Display dataframes in seperate
buffer.

If there is an active region, send that.  Otherwise, send the
whole buffer.

In Emacs 24.3 and later, without prefix argument, this will
escape the Python idiom of if __name__ == '__main__' to be false
to avoid accidental execution of code.  With prefix argument, this
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
                 (setq pyplotter-image-counter (- pyplotter-image-counter 1)))
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
      (when has-if-main
        (message (concat "Removed if __main__ == '__main__' construct, "
                         "use a prefix argument to evaluate.")))
      )
    (pyplotter-update-plots)
    (run-at-time "2 sec" nil #'pyplotter-update-plots))


(define-minor-mode pyplotter-mode
  "Turn Elpy into a data science IDE"
  :lighter " pyplotter"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'pyplotter-shell-send-region-or-buffer)
            map)

  (if pyplotter-mode
      ;; Enabling.
      (progn
        (setq pyplotter-image-counter 1000)
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



(add-hook 'pyplotter--shell-send-region-or-buffer 'pyplotter-update-plots)
(provide 'pyplotter)

;;; pyplotter.el ends here


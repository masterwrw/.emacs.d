;;;; Elisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)


(defun eye/open-init-file ()
  (interactive)
  (find-file-existing (expand-file-name "init.el" user-emacs-directory)))

;; @See http://metasandwich.com/2013/01/19/emacs-config-youre-doing-it-wrong
(defun eye/imenu-init ()
  (interactive)
  (eye/open-init-file)
  (widen)
  (imenu (imenu-choose-buffer-index)))
					

(defun init-narrow-to-section ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^;;;;")
      (re-search-backward "^;;;;" nil t))
    (push-mark)
    (next-line)
    (re-search-forward "^;;;;" nil t)
    (previous-line)
    (narrow-to-region (region-beginning) (region-end))))


(add-hook 'emacs-lisp-mode-hook
          (lambda ()
	    (eldoc-mode 1)
	    (outline-minor-mode 1)
	    (setq outline-regexp ";;;;+")
	    ))


;;;; auto insert
(with-eval-after-load 'emacs-lisp-mode
  (define-auto-insert '(emacs-lisp-mode . "Elisp skeleton")
    '(
      (read-string "Describe:")
      ";;; " (file-name-nondirectory buffer-file-name) " --- " str " -*- lexical-binding: t -*-" \n
      ";;"\n
      \n
      \n
      \n
      \n
      \n
      \n
      ";;; " (file-name-nondirectory buffer-file-name) " ends here"
      ))
  )









(provide 'init-elisp)

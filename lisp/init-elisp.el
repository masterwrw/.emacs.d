;;;; Elisp
(eye--reset-time)
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
	    (outline-minor-mode 1)
	    (setq outline-regexp ";;;;+")
	    ))


(defhydra hydra-elisp ()
  ("x" eval-last-sexp "Eval last" :exit t)
  ("i" imenu "imenu" :exit t)
  ("SPC" keyboard-quit "quit" :exit t))
(eye-define-leader-key emacs-lisp-mode-map "m" 'hydra-elisp/body)
(eye-define-leader-key lisp-interaction-mode-map "m" 'hydra-elisp/body)


(eye-set-leader-key emacs-lisp-mode-map)
(eye-set-leader-key lisp-interaction-mode-map)


(eye--print-time "init-elisp")



(provide 'init-elisp)

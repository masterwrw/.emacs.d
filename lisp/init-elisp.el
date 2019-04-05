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


(eye-reset-mode-leader-key emacs-lisp-mode-map)
(eye-define-mode-basic-keys emacs-lisp-mode-map)
(eye-define-leader-key emacs-lisp-mode-map "h" 'hydra-help/body)
(eye-define-leader-key emacs-lisp-mode-map "r" 'hydra-rect/body)
(eye-define-leader-key emacs-lisp-mode-map "f" 'hydra-file/body)
(eye-define-leader-key emacs-lisp-mode-map "e" 'hydra-select/body)
(eye-define-leader-key emacs-lisp-mode-map "c" 'hydra-jump/body)
(eye-define-leader-key emacs-lisp-mode-map "d" 'hydra-delete/body)
(eye-define-leader-key emacs-lisp-mode-map "w" 'hydra-window/body)
(eye-define-leader-key emacs-lisp-mode-map "s" 'hydra-search/body)
(eye-define-leader-key emacs-lisp-mode-map "i" 'hydra-imenu/body)
(eye-define-leader-key emacs-lisp-mode-map "o" 'hydra-outline/body)
(eye-define-leader-key emacs-lisp-mode-map "x" 'hydra-funcs/body)

(eye-reset-mode-leader-key lisp-interaction-mode-map)
(eye-define-mode-basic-keys lisp-interaction-mode-map)
(eye-define-leader-key lisp-interaction-mode-map "h" 'hydra-help/body)
(eye-define-leader-key lisp-interaction-mode-map "r" 'hydra-rect/body)
(eye-define-leader-key lisp-interaction-mode-map "f" 'hydra-file/body)
(eye-define-leader-key lisp-interaction-mode-map "e" 'hydra-select/body)
(eye-define-leader-key lisp-interaction-mode-map "c" 'hydra-jump/body)
(eye-define-leader-key lisp-interaction-mode-map "d" 'hydra-delete/body)
(eye-define-leader-key lisp-interaction-mode-map "w" 'hydra-window/body)
(eye-define-leader-key lisp-interaction-mode-map "s" 'hydra-search/body)
(eye-define-leader-key lisp-interaction-mode-map "i" 'hydra-imenu/body)
(eye-define-leader-key lisp-interaction-mode-map "o" 'hydra-outline/body)
(eye-define-leader-key lisp-interaction-mode-map "x" 'hydra-funcs/body)

(eye-define-leader-key emacs-lisp-mode-map "m" 'hydra-elisp/body)
(eye-define-leader-key lisp-interaction-mode-map "m" 'hydra-elisp/body)


(eye--print-time "init-elisp")



(provide 'init-elisp)

(defvar insert-mode-input-method current-input-method
  "for mini-wubi input method.")

(require 'xah-fly-keys)

(defun eye/major-mode-key ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode) (call-interactively 'hydra-elisp/body))
   ((eq major-mode 'lisp-interaction-mode) (call-interactively 'hydra-elisp/body))
   ((eq major-mode 'c-mode) (call-interactively 'hydra-cpp/body))
   ((eq major-mode 'c++-mode) (call-interactively 'hydra-cpp/body))
   ((eq major-mode 'python-mode) (call-interactively 'hydra-python/body))
   ((or (eq major-mode 'org-mode)
	(eq major-mode 'org-journal-mode))
    (call-interactively 'hydra-org/body))
   (t nil)))

(fly-dkeys (define-prefix-command 'fly-h-keymap)
	   '((";" . Info-goto-emacs-command-node)
	     ("a" . apropos-command)
	     ("b" . describe-bindings)
	     ("c" . describe-char)
	     ("d" . apropos-documentation)
	     ("e" . view-echo-area-messages)
	     ("f" . describe-face)
	     ("g" . info-lookup-symbol)
	     ("h" . describe-function)
	     ("i" . info)
	     ("j" . man)
	     ("k" . describe-key)
	     ("K" . Info-goto-emacs-key-command-node)
	     ("l" . view-lossage)
	     ("m" . xah-describe-major-mode)
	     ("n" . describe-variable)
	     ("o" . describe-language-environment)
	     ("p" . finder-by-keyword)
	     ("r" . apropos-variable)
	     ("s" . describe-syntax)
	     ("u" . elisp-index-search)
	     ("v" . apropos-value)
	     ("z" . describe-coding-system)))

;; leader keys
(fly-dkeys xah-fly-leader-key-map
	   '(("h" . hydra-help/body)
	     ("s" . switch-to-buffer)
	     ("n" . hydra-numbers/body)
	     ("h" . hydra-help/body)
	     ("r" . hydra-rect/body)
	     ("f" . hydra-file/body)
	     ("e" . hydra-select/body)
	     ("g" . hydra-jump/body)
	     ("d" . hydra-delete/body)
	     ("w" . hydra-window/body)
	     ("s" . hydra-search/body)
	     ("i" . hydra-imenu/body)
	     ("o" . hydra-outline/body)
	     ("x" . hydra-funcs/body)
	     ("m" . eye/major-mode-key)
	     ("a" . counsel-M-x)
	     ("c" . eye/eno-copy)
	     ("p" . counsel-yank-pop)
	     ("TAB" . mode-line-other-buffer)
	     ;; ("/" . xah-comment-dwim)
	     ))


(defun setup-command-fly-key-map ()
  (fly-dkeys xah-fly-key-map
	     '(("DEL" . nil)
	       ("a" . counsel-M-x)
	       ("e" . xah-shrink-whitespaces)
	       ("," . bm-previous)
	       ("." . bm-next)
	       ("t" . bm-toggle)
	       ("g" . counsel-bm)
	       ))
  (setq insert-mode-input-method current-input-method)
  (set-input-method nil)
  (global-hl-line-mode 0)
  )

(defun setup-insert-fly-key-map ()
  (set-input-method insert-mode-input-method)
  (global-hl-line-mode 1)
  )

(add-hook 'xah-fly-command-mode-activate-hook 'setup-command-fly-key-map)
(add-hook 'xah-fly-insert-mode-activate-hook 'setup-insert-fly-key-map)
(add-hook 'org-agenda-mode-hook 'xah-fly-insert-mode-activate)

(xah-fly-keys 1)


;;;; Fn keys
(define-key global-map (kbd "<f1>") (lambda ()
				      (interactive)
				      (if (fboundp 'counsel-ibuffer)
					  (counsel-ibuffer)
					(switch-to-buffer))))
(define-key global-map (kbd "<f2>") 'toggle-input-method)
(define-key global-map (kbd "<f3>") 'xah-next-window-or-frame)
(define-key global-map (kbd "<f4>") 'delete-other-windows)
(define-key xah-fly-key-map (kbd "<f8>") 'org-capture)
(define-key xah-fly-key-map (kbd "<f9>") 'org-agenda)
(define-key global-map (kbd "<f11>") 'fullscreen-toggle)
(define-key global-map (kbd "<f12>") 'counsel-etags-find-tag-at-point)
(define-key global-map (kbd "<C-f12>") 'pop-tag-mark)

;; running on msys2, can't use C-c, it is <pause>
(when is-terminal
  (define-key global-map (kbd "C-x <pause>") 'kill-emacs))


(defalias 'backward-kill-word 'eye/kill-inner-word)
(define-key global-map (kbd "<M-backspace>") 'eye/kill-inner-word)
(define-key global-map (kbd "<C-backspace>") 'eye/kill-inner-word)
(define-key global-map (kbd "<C-wheel-up>") 'eye/increase-font-size)
(define-key global-map (kbd "<C-wheel-down>") 'eye/decrease-font-size)

;; use [ESC] replace [C-g]
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(provide 'init-keys)

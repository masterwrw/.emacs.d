(use-package xah-fly-keys
  :ensure t
  :init
  (setq xah-fly-use-control-key nil) ;; must before enable xah-fly-keys
  (setq xah-fly-user-meta-key nil)
  :config
  (xah-fly-keys-set-layout "qwerty")
  (defun eye/insert-mode-setup ()
    (interactive)
    (global-hl-line-mode 1)
    (define-key xah-fly-key-map (kbd "M-SPC") 'xah-fly-command-mode-activate)
    (define-key xah-fly-key-map (kbd "<f1>") 'xah-fly-command-mode-activate)) ;; must here) ;; must here
  (defun eye/command-mode-setup ()
    (interactive)
    (global-hl-line-mode 0))
  (define-key key-translation-map (kbd "ESC") (kbd "<menu>"))
  (define-key xah-fly-key-map (kbd "a") 'helm-M-x)
  (define-key xah-fly-leader-key-map (kbd "<return>") 'helm-M-x)
  (define-key xah-fly-c-keymap (kbd "e") 'counsel-find-file)
  (define-key xah-fly-dot-keymap (kbd "a") 'org-agenda)
  (define-key xah-fly-dot-keymap (kbd "c") 'org-capture)
  (define-key xah-fly-dot-keymap (kbd "g") 'magit-status)
  (define-key xah-fly-h-keymap (kbd "l") 'helpful-variable)
  (define-key xah-fly-h-keymap (kbd "j") 'helpful-function)
  (define-key xah-fly-h-keymap (kbd "v") 'helpful-key)
  (add-hook 'xah-fly-insert-mode-activate-hook 'eye/insert-mode-setup)
  (add-hook 'xah-fly-command-mode-activate-hook 'eye/command-mode-setup)
  (global-set-key (kbd "<f1>") 'xah-fly-command-mode-activate)
  (xah-fly-keys 1))



(use-package hydra
  :ensure t
  :config
  (defhydra hydra-function (:color red :exit t)
    ("c" org-capture "capture")
    ("d" youdao-dictionary-search "dict")
    ("e" youdao-dictionary-search-from-input "dict input")
    ("m" mpg123 "music")
    ("g" magit-status "magit")
    ("p" package-list-packages "packages")
    ("=" text-scale-increase "scale add")
    ("-" text-scale-decrease "scale sub")
    ("q" nil "quit"))
  (global-set-key (kbd "<f12>") 'hydra-function/body)

  (defhydra hydra-programming (:color red :exit t)
    ("a" helm-imenu "imenu") ;; imenu/helm-imenu/counsel-imenu
    ("b" hs-toggle-hiding "toggle block hs")
    ("h" hs-hide-all "hide all block")
    ("s" hs-show-all "show all block")
    ("f" eye/show-full-path "full path")
    ("c" eye/cpp-help "cpp doc")
    ("g" eye/qt5-help "qt5 doc")
    ("p" eye/python-help "python doc")
    ("q" nil "quit"))
  (global-set-key (kbd "<f6>") 'hydra-programming/body)
  )


(provide 'init-key)

(use-package yasnippet
  :ensure t
  :config
  (set-face-attribute 'yas-field-highlight-face nil :foreground "black" :background nil)
  ;;(add-to-list `yas/root-directory (concat eye-emacs-extension-dir "/yasnippet/snippets"))
  (yas-global-mode 1)
  (use-package yasnippet-snippets
    :ensure t
    :config
    (yas-reload-all))
  )


(provide 'init-yasnippet)

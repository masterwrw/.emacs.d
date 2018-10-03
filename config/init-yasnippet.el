(require 'yasnippet)
;; (require 'yasnippet-snippets) ;; can not found in load-path?? 

;; (set-face-attribute 'yas-field-highlight-face nil :foreground "black" :background nil)
;;(add-to-list `yas/root-directory (concat eye-emacs-extension-dir "/yasnippet/snippets"))
(yas-global-mode 1)
(yas-reload-all)


(provide 'init-yasnippet)

(defun install-lua-env ()
  (use-package lua-mode
    :ensure t
    :config
    (setq lua-indent-level 4))
  (add-hook 'lua-mode-hook 'yas-minor-mode)

  ;; (defun eye/lua-shell ()
    ;; (interactive)
    ;; (setq default-directory "d:/projects/lua")
    ;; (eye/shell-cmd "lua-shell" "c:\\Lua5.1;"))
  )

(provide 'init-lua)

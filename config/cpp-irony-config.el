
(use-package company-irony
  :ensure t
  :hook (c++-mode . (lambda ()
		      (add-to-list 'company-backends 'company-irony))))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(when (eq system-type 'windows-nt)
  ;; Windows performance tweaks
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  ;; Set the buffer size to 64K on Windows (from the original 4K)
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
  ;; irony-server path
  (setq irony--server-executable (expand-file-name (concat user-emacs-directory "irony/bin/irony-server.exe")))
  ;; clang path
  ;;(setenv "PATH"
  ;;        (concat "C:\\msys32\\mingw64\\bin" ";"
  ;;                (getenv "PATH")))
  ;;(setq exec-path (append exec-path '("c:/msys32/mingw64/bin")))
  )


(provide 'cpp-irony-config)

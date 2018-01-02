
;; Switch buffer
;(defalias 'list-buffers 'ibuffer-other-window) ; make ibuffer-other-window default
(defalias 'list-buffers 'ibuffer) ; make ibuffer default
(bind-key "<f1>" 'switch-to-buffer)
(bind-key "C-<f1>" 'list-buffers)

;; Find in file or find in git project
(bind-key "<f2>" 'swiper)
(bind-key "C-<f2>" 'counsel-ag)

;; Close other frame or window
(bind-key "<f3>" 'delete-other-frames)
(bind-key "C-<f3>" 'delete-other-windows)

(bind-key "<f4>" 'kill-buffer)

;; Open file
(bind-key "<f6>" 'find-file)
(bind-key "C-<f6>" 'counsel-find-file)


(bind-key "C-s" 'save-buffer)
(bind-key "C-o" 'ace-window)

(bind-key "M-x" 'counsel-M-x)

(bind-key "M-s" 'avy-goto-char)

(bind-key "C-=" 'cnfonts-increase-fontsize)
(bind-key "C--" 'cnfonts-decrease-fontsize)

; (global-set-key (kbd "M-x") 'my-M-x)
; (global-set-key (kbd "C-x C-m") 'my-M-x)
; (global-set-key (kbd "<f8>") 'helm-do-ag-project-root)
; (bind-key "<f2>" 'helm-do-ag-project-root)
; (bind-key "C-c C-r" 'ivy-resume)
; (bind-key "C-x C-f" 'counsel-find-file)
; (bind-key "<f1> f" 'counsel-describe-function)
; (bind-key "<f1> v" 'counsel-describe-variable)
; (bind-key "<f1> l" 'counsel-load-library)
; (bind-key "<f2> i" 'counsel-info-lookup-symbol)
; (bind-key "<f2> u" 'counsel-unicode-char)
; (bind-key "C-c g" 'counsel-git)
; (bind-key "C-c j" 'counsel-git-grep)
; (bind-key "C-c k" 'counsel-ag)
; (bind-key "C-x l" 'counsel-locate)
; (bind-key "C-S-o" 'counsel-rhythmbox)




(provide 'init-keymap)

;; swiper configuration
;; Using swiper so ido no longer needed
;(require 'init-ido)
(use-package counsel
  :ensure t
  )

(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
 ;   (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))


;; smex configuration
(require-package 'smex)

;; smex or counsel-M-x?
(defvar my-use-smex nil
  "Use `smex' instead of `counsel-M-x' when press M-x.")

(defun my-M-x ()
  (interactive)
  (cond
    (my-use-smex
      (smex))
    ((fboundp 'counsel-M-x)
     ;; `counsel-M-x' will use `smex' to remember history
     (counsel-M-x))
    ((fboundp 'smex)
     (smex))
    (t
      (execute-extended-command))))


;; tabbar
(use-package tabbar
  :ensure t
  :config
  (tabbar-mode 1))


;; beacon, Highlight the cursor whenever the window scrolls
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))


(provide 'init-interface-plus)

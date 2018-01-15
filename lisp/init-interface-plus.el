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
;(use-package tabbar
;  :ensure t
;  :config
;  (tabbar-mode 1))

;; elscreen, Emacs window session manager, like tab bar.
(require-package 'elscreen)
(require 'elscreen)
(elscreen-start)
(require 'setup-elscreen)


;; beacon, Highlight the cursor whenever the window scrolls
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (diminish 'beacon-mode))


;; smooth-scrolling, Make emacs scroll smoothly
(require-package 'smooth-scrolling)
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)



;; diminish, Diminished modes are minor modes with no modeline display
(require-package 'diminish)
(require 'diminish)



;; fullframe, Generalized automatic execution in a single frame
(require-package 'fullframe)
(require 'fullframe)



;; persp-mode, windows/buffers sets shared among frames + save/load.
;(require-package 'persp-mode)
;(with-eval-after-load "persp-mode"
;  ;; .. all settings you want here
;  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
;(require 'persp-mode)




(provide 'init-interface-plus)

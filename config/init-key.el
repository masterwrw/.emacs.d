;;; Custom key binding
(require 'xah-functions)

(defun eye/scroll-up ()
  (interactive)
  (previous-line 5))

(defun eye/scroll-down ()
  (interactive)
  (next-line 5))

(global-set-key (kbd "<M-wheel-up>") 'eye/scroll-up)
(global-set-key (kbd "<M-wheel-down>") 'eye/scroll-down)

(defun ryo-modal-mode-on ()
  (interactive)
  (ryo-modal-mode 1))

(defun ryo-modal-mode-off ()
  (interactive)
  (ryo-modal-mode -1))

(defun setup-ryo-key ()
  (global-set-key (kbd "C-,") 'ryo-modal-mode-on)
  
  (let ((mode-hooks '(prog-mode-hook
		      c++-mode-hook
		      emacs-lisp-mode-hook
		      org-mode-hook
		      helpful-mode-hook
		      css-mode-hook
		      python-mode-hook)))
    (dolist (var mode-hooks)
      (add-hook var 'ryo-modal-mode)))

  (let ((excludes '(magit-status-mode-hook text-mode)))
    (dolist (var excludes)
      (add-hook var 'ryo-modal-mode-off))
    )
  )

;; 会影响 eno copy 的选中
(use-package ryo-modal
  :ensure t
  :config
  (setup-ryo-key)
  (set-cursor-color "#ee44a3")
  (setq ryo-modal-cursor-type 'box)
  (setq ryo-modal-cursor-color "#44aa33")
  (setq ryo-modal-default-cursor-color "#ee44a3")
  
  (ryo-modal-keys
   ("," counsel-M-x)
   ("SPC" ryo-modal-mode-off)
   
   ;; quick move
   ("j" left-char)
   ("l" right-char)
   ("u" left-word)  
   ("o" right-word)
   ("i" previous-line)
   ("k" next-line)
   ("p" eye/scroll-up)
   ("n" eye/scroll-down)
   (";" recenter-top-bottom)
   
   ;; region/select
   ("rr" set-mark-command)
   ("re" xah-extend-selection)
   ("r9" xah-select-text-in-quote)
   ("rl" xah-select-line)

   ;; copy/paste
   ("cc" xah-copy-line-or-region)
   ("cw" eno-word-copy)
   ("cl" eno-line-copy) ;;invalid
   ("cv" popup-kill-ring)
   ("x" xah-cut-line-or-region)
   ("v" yank)

   
   ;; move
   ("ma" eye/beginniing-of-line)
   ("me" move-end-of-line)
   ("mc" avy-goto-char)
   ("ml" avy-goto-line)
   ("mfa" beginning-of-defun)
   ("mfb" beginning-of-defun)
   ("mfe" end-of-defun)
   ("mi" imenu)
   ("m," backward-forward-previous-location)
   ("m." backward-forward-next-location)
   ("mg" dumb-jump-go)
   ("mt" counsel-etags-find-tag-at-point)
   ("md" dired-jump)
   ("mn" eye/new-next-line)
   ("mp" eye/new-previous-line)
   ("mb" bookmark-bmenu-list)

   ;; delete
   ("dc" delete-char)
   ("dw" kill-word)   
   ("dl" kill-line)
   ("dd" eye/kill-inner-word)

   ;; buffer
   ("bb" mode-line-other-buffer) ;;快速切换两个buffer
   ("bl" counsel-ibuffer)
   ("bk" kill-current-buffer)
   ("ba" beginning-of-buffer)
   ("be" end-of-buffer)
   
   ;; window
   ("ww" switch-window)
   ("wd" delete-other-windows)
   ("wc" delete-window)
   ("wr" split-window-right)
   ("wb" split-window-below)

   ;; search
   ;;TODO:quick search current word
   ("ss" save-buffer)
   ("sw" swiper)
   ("sa" counsel-ag)
   ("sr" query-replace)
   ("sg" eye/grep)
   ("sf" counsel-find-file)
   ("so" find-file-other-window)
   ;; find current buffer h/cpp file, use hydra define mode map key

   ("hv" helpful-variable)
   ("hf" helpful-function)
   ("hk" helpful-key)
   ("hm" describe-mode)
   ("hi" info)

   ("ee" eval-last-sexp)

   ("g" magit-status)
   ("a" org-agenda)
   ("t" org-capture)
   
   ))


(provide 'init-key)

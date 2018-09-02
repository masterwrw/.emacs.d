;;; Custom key binding
;; M-x
;; magit status
;; hide block/ show block
;; org-capture, org-agenda
;; save file
;; indent region or buffer
;;;

(require 'xah-functions)

(defun eye/ryo-insert-space ()
  (interactive)
  (insert " "))

(defun eye/ryo-move-up ()
  (interactive)
  (previous-line 3))

(defun eye/ryo-move-down ()
  (interactive)
  (next-line 3))

(global-set-key (kbd "<M-wheel-up>") 'eye/ryo-move-up)
(global-set-key (kbd "<M-wheel-down>") 'eye/ryo-move-down)

(defun ryo-modal-mode-on ()
  (interactive)
  (ryo-modal-mode 1))

(defun ryo-modal-mode-off ()
  (interactive)
  (ryo-modal-mode -1))

(defun setup-ryo-key ()
  ;; use ',' as a leader key
  (global-unset-key (kbd ","))
  (global-set-key (kbd "C-,") (lambda () (interactive) (insert ",")))
  (global-set-key (kbd ",") 'ryo-modal-mode-on)
  (define-key c++-mode-map (kbd ",") 'ryo-modal-mode-on)
  (global-unset-key (kbd "`"))
  (global-set-key (kbd "`") #'ryo-modal-mode)
  (global-set-key (kbd "C-`") (lambda () (interactive) (insert "`")))

  ;;TODO: use dolist
  (let ((mode-hooks '(text-mode-hook
		      prog-mode-hook
		      c++-mode-hook
		      emacs-lisp-mode-hook
		      org-mode-hook
		      helpful-mode-hook
		      css-mode-hook
		      python-mode-hook)))
    (dolist (var mode-hooks)
      (add-hook var 'ryo-modal-mode)))
  
  )

(use-package ryo-modal
  :ensure t
  :config
  (setup-ryo-key)
  (set-cursor-color "#ee44a3")
  (setq ryo-modal-cursor-type 'box)
  (setq ryo-modal-cursor-color "#44aa33")
  (setq ryo-modal-default-cursor-color "#ee44a3")
  
  (ryo-modal-keys
   ("mm" counsel-M-x)
   ("SPC" ryo-modal-mode-off)

   
   ;; quick move
   ("j" left-char)
   ("l" right-char)
   ("u" left-word)  
   ("o" right-word)
   ("i" previous-line)
   ("k" next-line)
   ("p" eye/ryo-move-up)
   ("n" eye/ryo-move-down)
   
   ;; region/select
   ("rr" set-mark-command)
   ("re" xah-extend-selection)
   ("r9" xah-select-text-in-quote)
   ("rl" xah-select-line)

   ;; copy/paste
   ("cc" xah-copy-line-or-region)
   ("cw" eno-word-copy)
   ("cl" eno-line-copy) ;;invalid
   ("cx" xah-cut-line-or-region)   
   ("cv" yank)
   ("vv" popup-kill-ring)

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
   ("dl" kill-line :exit t)
   ("dd" eye/kill-inner-word)

   ;; buffer
   ("bb" mode-line-other-buffer) ;;快速切换两个buffer
   ("bs" save-buffer)
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
   ("ss" swiper)
   ("sa" counsel-ag)
   ("sr" query-replace)
   ("sg" eye/grep)
   ("sf" counsel-find-file)
   ("so" find-file-other-window)
   ;; find current buffer h/cpp file, use hydra define mode map key

   ("hv" helpful-variable)
   ("hf" helpful-function)
   ("hk" helpful-key)

   ("ee" eval-last-sexp)

   ;; if press other key, auto exit ryo-modal-mode
   ("a" self-insert-command :exit t)
   ("f" self-insert-command :exit t)
   ("g" self-insert-command :exit t)
   ("q" self-insert-command :exit t)
   ("t" self-insert-command :exit t)
   ("x" self-insert-command :exit t)
   ("y" self-insert-command :exit t)
   ("z" self-insert-command :exit t)
   (";" self-insert-command :exit t)
   ("'" self-insert-command :exit t)
   ("\\" self-insert-command :exit t)
   ("[" self-insert-command :exit t)
   ("]" self-insert-command :exit t)
   ("." self-insert-command :exit t)
   ("/" self-insert-command :exit t)
   ("-" self-insert-command :exit t)
   ("=" self-insert-command :exit t)
   ("_" self-insert-command :exit t)
   ("+" self-insert-command :exit t)
   ("~" self-insert-command :exit t)
   ("1" self-insert-command :exit t)
   ("2" self-insert-command :exit t)
   ("3" self-insert-command :exit t)
   ("4" self-insert-command :exit t)
   ("5" self-insert-command :exit t)
   ("6" self-insert-command :exit t)
   ("7" self-insert-command :exit t)
   ("8" self-insert-command :exit t)
   ("9" self-insert-command :exit t)
   ("0" self-insert-command :exit t)
   ("!" self-insert-command :exit t)
   ("@" self-insert-command :exit t)
   ("#" self-insert-command :exit t)
   ("$" self-insert-command :exit t)
   ("%" self-insert-command :exit t)
   ("^" self-insert-command :exit t)
   ("&" self-insert-command :exit t)
   ("*" self-insert-command :exit t)
   ("(" self-insert-command :exit t)
   (")" self-insert-command :exit t)
   
   ))


(provide 'init-key)

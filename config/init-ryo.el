(defun ryo-modal-mode-on ()
  (interactive)
  (ryo-modal-mode 1))

(defun ryo-modal-mode-off ()
  (interactive)
  (ryo-modal-mode -1))

(defun setup-ryo-key ()
  (global-set-key (kbd "<M-SPC>") 'ryo-modal-mode-on)
  (global-set-key (kbd "C-,") 'ryo-modal-mode-on)
  (global-set-key (kbd "<home>") 'ryo-modal-mode-on)
  (define-key key-translation-map (kbd "ESC") (kbd "<home>"))
  (global-set-key (kbd "<insert>") 'ryo-modal-mode-off)

  (let ((mode-hooks '(find-file-hook
		      message-mode-hook
		      dired-mode-hook
		      help-mode-hook
		      man-mode-hook
		      prog-mode-hook
		      helpful-mode-hook
		      ;; c++-mode-hook
		      ;; emacs-lisp-mode-hook
		      ;; org-mode-hook
		      ;; css-mode-hook
		      ;; python-mode-hook
		      )))
    (dolist (var mode-hooks)
      (add-hook var 'ryo-modal-mode-on)))

  ;; (let ((excludes '(magit-status-mode-hook text-mode)))
    ;; (dolist (var excludes)
      ;; (add-hook var 'ryo-modal-mode-off))
    ;; )
  )

(require 'ryo-modal)
(setup-ryo-key)
(set-cursor-color "#ee44a3")
(setq ryo-modal-cursor-type 'box)
(setq ryo-modal-cursor-color "#44aa33")
(setq ryo-modal-default-cursor-color "#ee44a3")

(ryo-modal-mode-on)

(ryo-modal-keys
 ("SPC" ryo-modal-mode-off)
 ("q" ryo-modal-mode-off)

 ("j" left-char)
 ("l" right-char)
 ("u" left-word)
 ("o" right-word)
 ("i" previous-line)
 ("k" next-line)
 ("h" eye/beginniing-of-line)
 (";" end-of-line)
 ("p" eye/scroll-up)
 ("n" eye/scroll-down)
 ("'" recenter-top-bottom)
 ("/" xah-comment-dwim)
 ("s" save-buffer)

 ("`" indent-for-tab-command)
 ("t" nil)
 ("8" xah-extend-selection)

 ;; region/select
 ("m" set-mark-command)

 ;; copy/paste
 ("C-. "
  (("b" bookmark-bmenu-list)
   ("3" find-file-other-window)
   ("d" dired-jump)
   ("k" eye/new-next-line)
   ("i" eye/new-previous-line)
   ("w" other-window)
   ))

 ("w"
  (("l" windmove-right)
   ("j" windmove-left)
   ("i" windmove-up)
   ("k" windmove-down)))

 ;; move
 ("gh" beginning-of-defun)
 ("gl" end-of-defun)
 ("gi" imenu)
 ("gb" bookmark-bmenu-list)

 ;; buffer
 ("bb" mode-line-other-buffer) ;;快速切换两个buffer
 ("bk" kill-current-buffer)
 ("ba" beginning-of-buffer)
 ("be" end-of-buffer)
 ("bs" save-buffer)

 ("c" xah-copy-line-or-region)
 ("x" xah-cut-line-or-region)
 ("v" yank-with-indent)
 ("z" undo)

 ;; delete
 ("dd" delete-line-no-copy)
 ("dl" delete-char)
 ("du" delete-inner-word-no-copy)
 ("do" delete-forward-word-no-copy)
 ("d;" delete-end-of-line-no-copy)
 ("dh" delete-beginning-of-line-no-copy)
 ("dj" delete-backward-char)

 ;; find
 ("fr" query-replace)
 ("fg" eye/grep)

 ;; open
 ("1" delete-other-windows)
 ("2" split-window-below)
 ("3" split-window-right)
 )


(global-set-key (kbd "<M-up>") 'scroll-up-defun-or-lines)
(global-set-key (kbd "<M-down>") 'scroll-down-defun-or-lines)
(global-set-key (kbd "<M-left>") 'backward-word)
(global-set-key (kbd "<M-right>") 'forward-word)

(global-set-key (kbd "<C-up>") 'scroll-down-command)
(global-set-key (kbd "<C-down>") 'scroll-up-command)



(global-set-key (kbd "<tab>") 'hippie-expand)
(define-key prog-mode-map (kbd "<C-tab>") 'mode-line-other-buffer)
(global-set-key (kbd "<C-tab>") 'mode-line-other-buffer)
(global-set-key (kbd "<backtab>") 'indent-for-tab-command)


(global-set-key (kbd "<f5>") 'eval-last-sexp)

(global-set-key (kbd "<f9> b") 'bookmark-bmenu-list)

(defalias 'backward-kill-word 'eye/kill-inner-word)
(global-set-key (kbd "<M-backspace>") 'eye/kill-inner-word)
(global-set-key (kbd "<C-backspace>") 'eye/kill-inner-word)

(global-set-key (kbd "M-c") 'eye/capitalize-word)
(global-set-key (kbd "M-u") 'eye/upcase-word)


(provide 'init-ryo)

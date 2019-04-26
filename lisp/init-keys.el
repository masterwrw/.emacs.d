;;; init-leader-key.el --- Basic leader key configuration
(require 'hydra)
(require 'base-toolkit)

(defvar insert-mode-input-method current-input-method
  "for mini-wubi input method.")

(setq xah-fly-use-control-key nil)
(setq xah-fly-use-meta-key nil)
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

;; use [ESC] replace [C-g]
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; 使用自定义函数同时绑定多个按键，不使用xah-fly--define-keys，否则会绑定到dvorak布局的按键上
(defun dkeys (keymap-name key-cmd-alist)
  (interactive)
  (mapc
   (lambda (pair)
     (define-key keymap-name (kbd (car pair)) (cdr pair)))
   key-cmd-alist))
 

;;; more hydra define
(defhydra hydra-help (:exit t :idle 1.0)
  ("v" describe-variable "Desc var")
  ("f" describe-function "Desc fun")
  ("k" describe-key "Desc key")
  ("a" describe-face "Desc face")
  ("b" describe-bindings "desc bindgings")
  ("m" describe-mode "desc mode")
  ("i" info "Info")
  ("c" list-colors-display "List colors")
  ("s" list-faces-display "List faces"))

;;;; rectangle
(defhydra hydra-rect (:idle 1.0)
  ("r" replace-rectangle "Replace rectangle" :exit t)
  ("k" kill-rectangle "Kill rectangle" :exit t))


(defhydra hydra-jump (:exit t :idle 1.0)
  ("SPC" keyboard-quit "quit")
  ("b" pop-to-mark-command "pop local mark" :exit nil)
  ("p" pop-global-mark "pop global mark" :exit nil)
  ("t" pop-tag-mark "pop tag mark")
  ("g" ace-jump-char-mode "Goto char")
  ("l" ace-jump-line-mode "Goto line")
  ("d" hydra-dumb-jump/body "dumb jump")
  )


(defhydra hydra-file (:exit t :idle 1.0)
  ("a" switch-to-buffer "Switch buffer")
  ("s" save-buffer "Save buffer")
  ("o" find-file "Find file")
  ("h" recentf-open-files)
  ("k" kill-this-buffer "Kill buffer")
  ("z" xah-open-last-closed "Open last closed")
  ("b" bookmark-set "Set bookmark")
  ("f" xah-open-file-fast "Jump bookmark")
  ("l" bookmark-bmenu-list "List bookmark")
  ("p" xah-previous-user-buffer "Previous buffer")
  ("n" xah-next-user-buffer "Next buffer")
  )

;;;; select
(defhydra hydra-select (:idle 1.0)
  ("SPC" keyboard-quit "quit" :exit t)
  ("a" mark-whole-buffer "Select all" :exit t)
  ("e" xah-extend-selection "Extend")
  ("q" xah-select-text-in-quote "Select quote" :exit t)
  ("l" xah-select-line "Select line" :exit t)
  ("b" xah-select-block "select block")
  ("n" narrow-to-region "Narrorw" :exit t)
  ("w" widen "widen" :exit t)
  )

;;;; delete
(defhydra hydra-delete (:idle 1.0)
  "delete:"
  ("SPC" nil "quit")
  ("d" delete-line-no-copy :exit t)
  ("l" delete-char)
  ("j" backward-delete-char)
  ("u" delete-inner-word-no-copy "backward word")
  ("o" delete-forward-word-no-copy "forward word")
  ("h" delete-beginning-of-line-no-copy "begin line" :exit t)
  (";" delete-end-of-line-no-copy "end line" :exit t)
  ("b" xah-delete-current-text-block "block" :exit t)
  )


;;;; window
(defhydra hydra-window (:idle 1.0)
  ("SPC" nil "quit")
  ("n" eye/new-frame)
  ("o" xah-next-window-or-frame "Next window/frame")
  ("0" delete-window-or-frame "Delete window/frame" :exit t)
  ("1" delete-other-windows "Delete other window" :exit t)
  ("3" split-window-horizontally "Split hor" :exit t)
  ("4" split-window-vertically "Split ver" :exit t))

;;;; search
(defhydra hydra-search (:idle 1.0)
  ("SPC" nil "quit" :exit t)
  ("s" occur "Occur" :exit t)
  ("f" isearch-forward "isearch-forward" :exit t)
  ("b" isearch-backward "isearch-backward" :exit t)
  ("q" query-replace "query-replace" :exit t)
  ("r" eye/replace-string-buffer "Replace all" :exit t)
  ("o" multi-occur-in-matching-buffers "Occur buffers" :exit t)
  )
(define-key isearch-mode-map (kbd "M-k") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-i") 'isearch-repeat-backward)


;;;; outline
(defhydra hydra-outline ()
  "
    _s_: outline show entry    _a_: outline show all    _n_: outline next heading      _t_: toggle children
    _h_: outline hide entry    _b_: outline hide body   _p_: outline previous heading
"
  ("SPC" nil "quit")
  ("s" outline-show-entry nil)
  ("h" outline-hide-entry nil)
  ("a" outline-show-all nil)
  ("b" outline-hide-body nil)
  ("n" outline-next-heading nil)
  ("p" outline-previous-heading nil)
  ("t" outline-toggle-children nil))

;;;; imenu
(defhydra hydra-imenu (:exit t :idle 1.0)
  ("SPC" nil "quit" :exit t))


;;;; funcs
(defhydra hydra-funcs (:idle 1.0)
  ("SPC" nil "quit" :exit t)
  ("p" pop-global-mark "Pop mark" :exit t)
  ("r" read-only-mode "Read only" :exit t)
  ("l" global-display-line-numbers-mode "Line number")
  ("t" toggle-truncate-lines "Toggle truncate lines")
  ("g" global-company-mode "Company" :exit t)
  )


(defhydra hydra-dired (:exit t)
  ("SPC" nil "quit")
  ("o" dired-w32-browser "open")
  ("e" dired-w32explorer "explorer"))



(defhydra hydra-cpp (:exit t)
  "
_a_: list tags
"
  ("a" counsel-etags-list-tag)
  ("c" counsel-etags-scan-code "create TAGS")
  ("d" counsel-etags-find-tag-at-point "find tag at point")
  ("e" counsel-etags-find-tag "find tag")
  ("r" counsel-etags-recent-tag "recent tag")
  ("t" eye/update-ctags-this-file "update file tags")
  ("f" eye/find-header-or-source-file "find h or cpp")
  ("l" eye/load-project-root-tags "load root tags")
  ("s" eye/search-cpp-doc "cpp doc")
  ("g" eye/auto-compile "compile"))



(dkeys (define-prefix-command 'fly-h-keymap)
       '((";" . Info-goto-emacs-command-node)
	 ("a" . apropos-command)
	 ("b" . describe-bindings)
	 ("c" . describe-char)
	 ("d" . apropos-documentation)
	 ("e" . view-echo-area-messages)
	 ("f" . describe-face)
	 ("g" . info-lookup-symbol)
	 ("h" . describe-function)
	 ("i" . info)
	 ("j" . man)
	 ("k" . describe-key)
	 ("K" . Info-goto-emacs-key-command-node)
	 ("l" . view-lossage)
	 ("m" . xah-describe-major-mode)
	 ("n" . describe-variable)
	 ("o" . describe-language-environment)
	 ("p" . finder-by-keyword)
	 ("r" . apropos-variable)
	 ("s" . describe-syntax)
	 ("u" . elisp-index-search)
	 ("v" . apropos-value)
	 ("z" . describe-coding-system)))


(defhydra hydra-numbers ()
  "numbers:"
  ("SPC" nil "quit")
  ("1" (lambda () (interactive) (insert "1")))
  ("2" (lambda () (interactive) (insert "2")))
  ("3" (lambda () (interactive) (insert "3")))
  ("4" (lambda () (interactive) (insert "4")))
  ("5" (lambda () (interactive) (insert "5")))
  ("6" (lambda () (interactive) (insert "6")))
  ("7" (lambda () (interactive) (insert "7")))
  ("8" (lambda () (interactive) (insert "8")))
  ("9" (lambda () (interactive) (insert "9")))
  ("0" (lambda () (interactive) (insert "0"))))

(defun setup-command-fly-key-map ()
  (dkeys xah-fly-key-map
	 '(("DEL" . nil)
	   ("a" . counsel-M-x)
	   ("b" . mode-line-other-buffer)
	   ("d" . delete-char)
	   ("e" . open-line)
	   ("n" . scroll-up-command)
	   ("m" . set-mark-command)
	   ("p" . scroll-down-command)
	   ("r" . newline)
	   ("s" . backward-delete-char)
	   ("w" . other-window)

	   ("1" . delete-other-windows)
	   ("2" . switch-to-buffer)
	   ("3" . split-window-below)
	   ("4" . split-window-right)
	   ("6" . xah-select-block)
	   ("7" . xah-select-line)
	   ("8" . xah-extend-selection)
	   ("9" . xah-select-text-in-quote)
	   ("0" . delete-window)
	   
	   ("," . xah-fly-leader-key-map)
	   ("/" . xah-comment-dwim)
	   ))
  (setq insert-mode-input-method current-input-method)
  (set-input-method nil)
  (global-hl-line-mode 0)
  )

(defun setup-insert-fly-key-map ()
  (dkeys xah-fly-key-map
	 '(
	   ("C-," . xah-fly-leader-key-map)
	   ("C-k" . xah-fly-leader-key-map)
	   ))
  (set-input-method insert-mode-input-method)
  (global-hl-line-mode 1)
  )
  
(add-hook 'xah-fly-command-mode-activate-hook 'setup-command-fly-key-map)
(add-hook 'xah-fly-insert-mode-activate-hook 'setup-insert-fly-key-map)

(defun eye/major-mode-key ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode) (call-interactively 'hydra-elisp/body))
   ((eq major-mode 'lisp-interaction-mode) (call-interactively 'hydra-elisp/body))
   ((eq major-mode 'c++-mode) (call-interactively 'hydra-cpp/body))
   ((eq major-mode 'python-mode) (call-interactively 'hydra-python/body))
   (t nil)))

;; leader keys
(dkeys xah-fly-leader-key-map
       '(("h" . hydra-help/body)
	 ("s" . switch-to-buffer)
	 ("n" . hydra-numbers/body)
	 ("h" . hydra-help/body)
	 ("r" . hydra-rect/body)
	 ("f" . hydra-file/body)
	 ("e" . hydra-select/body)
	 ("g" . hydra-jump/body)
	 ("d" . hydra-delete/body)
	 ("w" . hydra-window/body)
	 ("s" . hydra-search/body)
	 ("i" . hydra-imenu/body)
	 ("o" . hydra-outline/body)
	 ("x" . hydra-funcs/body)
	 ("m" . eye/major-mode-key)
	 ("a" . counsel-M-x)
	 ("c" . eye/eno-copy)
	 ("p" . counsel-yank-pop)
	 ("TAB" . mode-line-other-buffer)
	 ))




;;;; Fn keys
(define-key global-map (kbd "<f1>") (lambda ()
				      (interactive)
				      (if (fboundp 'counsel-ibuffer)
					  (counsel-ibuffer)
					(switch-to-buffer))))
(define-key global-map (kbd "<f2>") 'toggle-input-method)
(define-key global-map (kbd "<f3>") 'xah-next-window-or-frame)
(define-key global-map (kbd "<f4>") 'delete-other-windows)
(define-key global-map (kbd "<f8>") 'org-capture)
(define-key global-map (kbd "<f9>") 'org-agenda)
(define-key global-map (kbd "<f11>") 'fullscreen-toggle)
(define-key global-map (kbd "<f12>") 'counsel-etags-find-tag-at-point)
(define-key global-map (kbd "<C-f12>") 'pop-tag-mark)

;; running on msys2, can't use C-c, it is <pause>
(when is-terminal
  (eye-define-key global-map "C-x <pause>" 'kill-emacs))


(defalias 'backward-kill-word 'eye/kill-inner-word)
(define-key global-map (kbd "<M-backspace>") 'eye/kill-inner-word)
(define-key global-map (kbd "<C-backspace>") 'eye/kill-inner-word)
(define-key global-map (kbd "<C-wheel-up>") 'eye/increase-font-size)
(define-key global-map (kbd "<C-wheel-down>") 'eye/decrease-font-size)





(provide 'init-keys)

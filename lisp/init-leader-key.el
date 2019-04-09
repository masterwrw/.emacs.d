;;; init-leader-key.el --- Basic leader key configuration

(defun eye-reset-mode-leader-key (modmap)
  (define-key modmap (kbd ",") nil)
  (define-key modmap (kbd "M-,") (lambda () (interactive) (insert ","))))

(eye-reset-mode-leader-key global-map)

(defun eye-define-key (modmap keychar func)
  (define-key modmap (kbd keychar) func))

(defun eye-define-leader-key (modmap keychar func)
  (define-key modmap (kbd (concat "," keychar)) func))



;;;; basic keys
(add-to-list 'load-path "~/packages/hydra")
(add-to-list 'load-path (concat user-emacs-directory "lisp/xah-functions"))
(require 'hydra)
(require 'base-toolkit)
(require 'xah-functions)

;;;; move cursor
(defhydra hydra-move (:column 4 :idle 1.0)
  "
_a_:Begin defun    _w_:Begin buffer   _r_:Forward para    _b_:Brackets match
_s_:End defun      _e_:End buffer     _t_:Backward para   _/_:Comment
"
  ("j" left-char)
  ("M-j" left-char)
  ("l" right-char)
  ("M-l" right-char)
  ("u" left-word)
  ("M-u" left-word)
  ("o" right-word)
  ("M-o" right-word)
  ("i" previous-line)
  ("M-i" previous-line)
  ("k" next-line)
  ("M-k" next-line)
  ("h" eye/beginning-of-line-or-block)
  ("M-h" eye/beginning-of-line-or-block)
  (";" xah-end-of-line-or-block)
  ("M-;" xah-end-of-line-or-block)
  ("n" scroll-up-command)
  ("M-n" scroll-up-command)
  ("p" scroll-down-command)
  ("M-p" scroll-down-command)
  ("m" set-mark-command)
  ("b" xah-goto-matching-bracket nil)
  ("/" xah-comment-dwim nil)
  ("a" beginning-of-defun nil)
  ("s" end-of-defun nil)
  ("w" beginning-of-buffer nil)
  ("e" end-of-buffer nil)
  ("r" forward-paragraph nil)
  ("t" backward-paragraph nil)
  ("SPC" keyboard-quit "quit" :exit t) 		;keyboard-quit to quit mark state
  )

(defun eye-define-mode-basic-keys (modmap)
  (interactive)
  (eye-define-key modmap "M-j" '(lambda () (interactive) (left-char) (hydra-move/body)))
  (eye-define-key modmap "M-l" '(lambda () (interactive) (right-char) (hydra-move/body)))
  (eye-define-key modmap "M-u" '(lambda () (interactive) (left-word) (hydra-move/body)))
  (eye-define-key modmap "M-o" '(lambda () (interactive) (right-word) (hydra-move/body)))
  (eye-define-key modmap "M-i" '(lambda () (interactive) (previous-line) (hydra-move/body)))
  (eye-define-key modmap "M-k" '(lambda () (interactive) (next-line) (hydra-move/body)))
  (eye-define-key modmap "M-h" '(lambda () (interactive) (eye/beginning-of-line-or-block) (hydra-move/body)))
  (eye-define-key modmap "M-;" '(lambda () (interactive) (xah-end-of-line-or-block) (hydra-move/body)))
  (eye-define-key modmap "M-n" '(lambda () (interactive) (scroll-up-command) (hydra-move/body)))
  (eye-define-key modmap "M-p" '(lambda () (interactive) (scroll-down-command) (hydra-move/body)))
  (eye-define-key modmap "M-m" 'set-mark-command)
  (eye-define-key modmap "M-w" 'xah-copy-line-or-region)
  (eye-define-key modmap "M-e" 'xah-copy-line-or-region) ;sometimes M-w not working, then use M-e
  (eye-define-key modmap "M-q" 'xah-cut-line-or-region)
  (eye-define-key modmap "M-a" 'yank)
  (eye-define-key modmap "M-z" 'undo))

(eye-define-mode-basic-keys global-map)


;;; more hydra define
(defhydra hydra-help (:exit t :idle 1.0)
  ("v" describe-variable "Desc var")
  ("f" describe-function "Desc fun")
  ("k" describe-key "Desc key")
  ("a" describe-face "Desc face")
  ("i" info "Info")
  ("c" list-colors-display "List colors")
  ("s" list-faces-display "List faces"))

;;;; rectangle
(defhydra hydra-rect (:idle 1.0)
  ("r" replace-rectangle "Replace rectangle" :exit t)
  ("k" kill-rectangle "Kill rectangle" :exit t))



(defhydra hydra-jump (:exit t :idle 1.0)
  ("SPC" keyboard-quit "quit")
  ("g" goto-line "Goto line"))


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
  ("n" narrow-to-region "Narrorw" :exit t)
  ("w" widen "widen" :exit t)
  )

;;;; delete
(defhydra hydra-delete (:exit t :idle 1.0)
  ("d" delete-line-no-copy)
  ("u" delete-inner-word-no-copy)
  ("o" delete-forward-word-no-copy)
  (";" delete-end-of-line-no-copy)
  ("h" delete-beginning-of-line-no-copy))
(define-key global-map (kbd "M-8") 'backward-delete-char)
(define-key global-map (kbd "M-9") 'delete-char)


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
  ("l" global-display-line-numbers-mode "Line number"))


(defun eye-set-leader-key (modmap)
  (interactive)
  (eye-reset-mode-leader-key modmap)
  (eye-define-mode-basic-keys modmap)
  (eye-define-leader-key modmap "h" 'hydra-help/body)
  (eye-define-leader-key modmap "r" 'hydra-rect/body)
  (eye-define-leader-key modmap "f" 'hydra-file/body)
  (eye-define-leader-key modmap "e" 'hydra-select/body)
  (eye-define-leader-key modmap "c" 'hydra-jump/body)
  (eye-define-leader-key modmap "d" 'hydra-delete/body)
  (eye-define-leader-key modmap "w" 'hydra-window/body)
  (eye-define-leader-key modmap "s" 'hydra-search/body)
  (eye-define-leader-key modmap "i" 'hydra-imenu/body)
  (eye-define-leader-key modmap "o" 'hydra-outline/body)
  (eye-define-leader-key modmap "x" 'hydra-funcs/body))

(eye-set-leader-key global-map)

(if is-terminal
    (eye-define-leader-key global-map " TAB" 'mode-line-other-buffer)
  (eye-define-leader-key global-map " <tab>" 'mode-line-other-buffer))


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


(provide 'init-leader-key)

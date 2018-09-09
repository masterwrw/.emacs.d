(use-package paredit
  :ensure t
  :config
  ;;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  ;;(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  ;;(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  ;;(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  ;;(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  ;;(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  ;;(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  )

;; 安装了 smex 后，counsel-M-x 才会按照使用频率排序
(use-package smex :ensure t)

(use-package ivy
  :ensure t
  :config
  (define-key ivy-minibuffer-map (kbd ",") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd ".") 'ivy-next-line))

;;; 按行滚动
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; Switch window
;; (define-key global-map (kbd "S-<left>") 'windmove-left)
;; (define-key global-map (kbd "S-<right>") 'windmove-right)
;; (define-key global-map (kbd "S-<up>") 'windmove-up)
;; (define-key global-map (kbd "S-<down>") 'windmove-down)
;; (define-key org-mode-map (kbd "S-<left>") 'windmove-left)
;; (define-key org-mode-map (kbd "S-<right>") 'windmove-right)
;; (define-key org-mode-map (kbd "S-<up>") 'windmove-up)
;; (define-key org-mode-map (kbd "S-<down>") 'windmove-down)
(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" "i" "o"))
  :bind
  ([remap other-window] . switch-window)
  ("M-<f3>" . switch-window))

;;https://www.emacswiki.org/emacs/SwitchingBuffers
(defun eye/quick-switch-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Following window splits
;; After split a window, let the focus in the new split window.
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-<f2>") 'kill-current-buffer)
;;; Kill buffers without asking
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-s-k") 'close-all-buffers)
(defun eye/create-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))
(defun eye/show-full-path ()
  "Show the file full path with current buffer."
  (interactive)
  (message (expand-file-name (buffer-file-name))))
;;; Do not ask
(setq ibuffer-expert t)

(use-package helm
  :ensure t
;;  :bind
;;  ("C-x C-f" . 'helm-find-files)
;;  ("M-o" . 'helm-find-files)
;;  ("C-x C-b" . 'ibuffer)
;;  ("<f2>" . 'helm-buffers-list)
;;  ("M-<f2>" . 'ibuffer)
;;  ("M-x" . 'helm-M-x)
  :init
  (helm-mode 1)
  :config
  (defun eye/helm-hide-minibuffer ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook 'eye/helm-hide-minibuffer)
  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 40
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-split-window-in-side-p nil
        helm-move-to-line-cycle-in-source nil
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8 
        helm-echo-input-in-header-line t))

(require 'helm-config)    
(helm-autoresize-mode 1)
(define-key helm-find-files-map (kbd "C-b") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-f") 'helm-execute-persistent-action)

(use-package avy
  :ensure t
  :bind
  ("M-g" . avy-goto-char))

;; 自动保存书签
(add-hook 'kill-emacs-hook
          '(lambda ()
             (bookmark-save)))

;;  (global-set-key (kbd "<f9>") 'list-bookmarks)
;;  (global-set-key (kbd "M-<f9>") 'bookmark-set)

(global-set-key (kbd "<M-left>") 'backward-word)
(global-set-key (kbd "<M-right>") 'forward-word)
(global-set-key (kbd "<M-up>") 'backward-paragraph)
(global-set-key (kbd "<M-down>") 'forward-paragraph)

;; (define-key org-mode-map (kbd "<M-left>") 'backward-word)
;; (define-key org-mode-map (kbd "<M-right>") 'forward-word)
;; (define-key org-mode-map (kbd "<M-up>") 'backward-paragraph)
;; (define-key org-mode-map (kbd "<M-down>") 'forward-paragraph)

;;; Quick insert new line
(defun eye/new-next-line ()
  "在当前行下方快速添加新的一行。"
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun eye/new-previous-line ()
  "在当前行上方快速添加新的一行。"
  (interactive)
  (beginning-of-line)
  (if (eq 1 (point))
      (progn
	(newline)
	(previous-line))
    (progn
      (previous-line)
      (move-end-of-line 1)
      (newline)
      (indent-for-tab-command))))

(global-set-key (kbd "M-n") 'eye/new-next-line)
(global-set-key (kbd "M-p") 'eye/new-previous-line)

(defun eye/beginniing-of-line ()
  "移动到行首加强版，重复按 C-a，在忽略空白的行首和真实行首来回切换。"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(global-set-key (kbd "C-a") 'eye/beginniing-of-line)
(defalias 'org-beginning-of-line 'eye/beginniing-of-line)
(defun eye/goto-line ()
  "Auto enable and disable linum-mode."
  (interactive)
  (unless (bound-and-true-p linum-mode)
    (linum-mode))
  (let ((num (read-string "Goto line: ")))
    (goto-line (string-to-number num))
    (end-of-line))
  (linum-mode -1))

(global-set-key (kbd "M-l") 'eye/goto-line)

(use-package multiple-cursors
  :ensure t
  :config
  (defhydra multiple-cursors-hydra (:color blue :hint nil)
    ("a" mc/mark-all-like-this "all like this")
    ("b" mc/edit-lines "edit lines")
    ("c" mc/mark-next-like-this "next like this")
    ("d" mc/mark-previous-like-this "previous like this")
    ("q" nil "quit")))


;; (use-package edit-at-point
;; :ensure t
;; :bind ("C-c a" . 'edit-at-point-line-copy))

(defun eye/kill-inner-word ()
  "Kills the entire word your cursor is in. Equivalent to 'ciw' in vim."
  (interactive)
  ;;(forward-char 1) 
  (backward-word)
  (kill-word 1))
(defalias 'backward-kill-word 'eye/kill-inner-word)
(global-set-key (kbd "<M-backspace>") 'eye/kill-inner-word)
(global-set-key (kbd "<C-backspace>") 'eye/kill-inner-word)

(defun eye/copy-whole-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (kill-word 1)
    (yank)))
;; (key-chord-define-global "cc" 'eye/copy-whole-word)

(defun eye/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))
;; (key-chord-define-global "cl" 'eye/copy-whole-line)

;; (key-chord-define-global "dd" 'kill-whole-line)

(defun eye/copy-paragraph ()
  "Copy paragraphes at point"
  (interactive)
  (let ((beg (progn (backward-paragraph 1) (point)))
        (end (progn (forward-paragraph 1) (point))))
    (copy-region-as-kill beg end)))

(defun eye/capitalize-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (capitalize-word 1)))
(global-set-key (kbd "M-c") 'eye/capitalize-word)

(defun eye/upcase-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (upcase-word 1)))
(global-set-key (kbd "M-u") 'eye/upcase-word)

(defun eye/downcase-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (downcase-word 1)))

(defun eye/current-word ()
  (interactive)
  (let (p1 p2 w)
    (save-excursion
      (skip-chars-backward "-_A-Za-z0-9")
      (setq p1 (point))
      (skip-chars-forward "-_A-Za-z0-9")
      (setq p2 (point)))
    (copy-region-as-kill p1 p2)
    (substring-no-properties (car kill-ring))))


(delete-selection-mode 1)

;; 快速复制/剪切/移动其它位置的单词/行
(use-package eno
  :ensure t)


(defhydra eno-hydra (:color pink :hint nil)
  "
^Word^          ^Symbol         ^Str^          ^Paren^
^^^^^^^^----------------------------------------------
_a_: jump       _e_: jump       _i_: jump      _m_: jump
_b_: copy       _f_: copy       _j_: copy      _n_: copy
_c_: cut        _g_: cut        _k_: cut       _o_: cut
_d_: paste      _h_: paste      _l_: paste     _p_: paste
"
  ("a" eno-word-jump)
  ("b" eno-word-copy)
  ("c" eno-word-cut)
  ("d" eno-word-paste)
  ("e" eno-symbol-jump)
  ("f" eno-symbol-copy)
  ("g" eno-symbol-cut)
  ("h" eno-symbol-paste)
  ("i" eno-str-jump)
  ("j" eno-str-copy)
  ("k" eno-str-cut)
  ("l" eno-str-paste)
  ("m" eno-paren-jump)
  ("n" eno-paren-copy)
  ("o" eno-paren-cut)
  ("p" eno-paren-paste)
  ("q" nil "quit" :color blue))


;; undo-tree
(require-package 'undo-tree)
(require 'undo-tree)
(global-undo-tree-mode 1)
(diminish 'undo-tree-mode "undo")


;; writeroom

(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 120)

  (defun writeroom-mode-on ()
    (interactive)
    (add-hook 'c++-mode-hook 'writeroom-mode)
    (add-hook 'emacs-lisp-mode-hook 'writeroom-mode)
    (add-hook 'org-mode-hook 'writeroom-mode)
    (add-hook 'css-mode-hook 'writeroom-mode)
    (writeroom-mode))

  (defun writeroom-mode-off ()
    (interactive)
    (remove-hook 'c++-mode-hook 'writeroom-mode)
    (remove-hook 'emacs-lisp-mode-hook 'writeroom-mode)
    (remove-hook 'org-mode-hook 'writeroom-mode)
    (remove-hook 'css-mode-hook 'writeroom-mode)
    (writeroom-mode -1))
  )

(use-package change-inner
  :ensure t)

(use-package vimish-fold
  :ensure t)


(use-package electric-spacing
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'electric-spacing-mode))

(use-package centered-cursor-mode
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'centered-cursor-mode)
  (add-hook 'emacs-lisp-mode-hook 'centered-cursor-mode)
  (add-hook 'org-mode-hook 'centered-cursor-mode)
  (add-hook 'css-mode-hook 'centered-cursor-mode))

(provide 'init-edit)

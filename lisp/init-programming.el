;; imenu-list, Show imenu entries in a separate buffer
(require-package 'imenu-list)
(require 'imenu-list)

;;; Pair configuration
(show-paren-mode 1)
;; autopair
(require-package 'autopair)
(require 'autopair)
(autopair-global-mode)
(diminish 'autopair-mode)


;; change-inner
(require-package 'change-inner)
(require 'change-inner)


;; smartparens
(require-package 'smartparens)
(require 'smartparens)


;; paredit
(require-package 'paredit)
(require 'paredit)


;; Indent configuration
(setq tab-width 4 indent-tabs-mode nil)
(setq c-basic-offset 4 c-default-style "bsd")
;; dtrt-indent, Adapt to foreign indentation offsets
(require-package 'dtrt-indent)
(require 'dtrt-indent)
(dtrt-indent-mode 1)


;; aggressive-indent, Minor mode to aggressively keep your code always indented
(require-package 'aggressive-indent)
(require 'aggressive-indent)


;; multiple-cursors
(require-package 'multiple-cursors)
(require 'multiple-cursors)
(defhydra multiple-cursors-hydra (:color blue)
  ("a" mc/mark-all-like-this "all like this")
  ("b" mc/edit-lines "edit lines")
  ("c" mc/mark-next-like-this "next like this")
  ("d" mc/mark-previous-like-this "previous like this")
  ("q" nil "quit"))

;; auto-highlight-symbol
(require-package 'auto-highlight-symbol)
;(require 'auto-highlight-symbol)
;(global-auto-highlight-symbol-mode t) ;; Conflict with M-<left> for jump back.
(setq global-auto-highlight-symbol-mode nil)
(diminish 'auto-highlight-symbol-mode)


;; undo-tree
(require-package 'undo-tree)
(require 'undo-tree)
(global-undo-tree-mode 1)
(diminish 'undo-tree-mode "undo")


;; ws-butler, Unobtrusively remove trailing whitespace.
(require-package 'ws-butler)
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)
(add-hook 'text-mode 'ws-butler-mode)
(add-hook 'fundamental-mode 'ws-butler-mode)
(diminish 'ws-butler-mode)


;; anzu, Show number of matches in mode-line while searching.
(require-package 'anzu)
(require 'anzu)
(global-anzu-mode)
(diminish 'anzu-mode)


;; clean-aindent-mode, Simple indent and unindent, trims indent white-space.
(require-package 'clean-aindent-mode)
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)


;; volatile-highlights, Minor mode for visual feedback on some operations.
(require-package 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)


;; wgrep-ag, Writable grep buffer and apply the changes to files
(require-package 'wgrep)
(require-package 'wgrep-ag)
(require 'wgrep)


;; iedit, Edit multiple regions in the same way simultaneously.
(require-package 'iedit)


;; company, auto complete
(require-package 'company)
(require 'company)
(setq company-show-numbers t) ; use alt+number to quick select
(setq company-idle-delay 0.2) ; immediately company complete
(setq company-selection-wrap-around t) ; make previous/next selection in the popup cycles

(setq company-backends nil)
(add-to-list 'company-backends 'company-elisp 'company-files 'company-etags)

;; company-dabbrev config, it is for current buffer string auto complete.
(add-to-list 'company-backends 'company-dabbrev)
(add-to-list 'company-backends 'company-dabbrev-code)
(setq company-dabbrev-code-everywhere t)
(setq company-dabbrev-minimum-length 2)
(setq company-dabbrev-other-buffers 'all)
(setq company-dabbrev-downcase nil)


;; company-statistics, Sort candidates using completion history
(require-package 'company-statistics)
(require 'company-statistics)
(company-statistics-mode)


(require-package 'company-c-headers)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(progn
     (add-to-list 'company-backends 'company-c-headers)
     (diminish 'company-mode "com")))


;; yasnippet
(require-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)


;; helm-ag, Need install the_silver_searcher, https://github.com/ggreer/the_silver_searcher
(require-package 'helm-ag)



;; Jump to previous position on current buffer.
(require 'jtpp)


;; eno, Goto/copy/cut any word/symbol/line in view, similar to ace-jump/easymotion
(require-package 'eno)
(require 'eno)

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



(provide 'init-programming)

;;; init-leader-key.el --- Basic leader key configuration
(defun bind-key (keymap keycode function &optional file)
  "绑定全局按键，或者已经存在的keymap，file参数用于autoload"
  (if file
      (autoload function file))
  (define-key keymap (kbd keycode) function))

(defmacro bind-mode-key (mode keymap keycode function &optional file)
  "定义和模式相关的key，由于keymap需要存在才能调用define-key，这里使用defmacro，mode加载后才执行define-key。
示例：(bind-mode-key 'cc-mode c++-mode-map \"M-p\" 'find-file)
"
  `(progn
     (if ,file
	 (autoload ,function ,file))
     (with-eval-after-load ,mode
       (define-key ,keymap (kbd ,keycode) ,function))))


(require 'hydra)
;;;; hydra: help
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

;;;; hydra-move
(defhydra hydra-move ()
  ("SPC" nil :exit t)
  ("q" nil :exit t)
  ("j" left-char)
  ("l" right-char)
  ("i" previous-line)
  ("k" next-line)
  ("h" beginning-of-line)
  (";" end-of-line)
  ("u" left-word)
  ("o" right-word)
  ("c" kill-ring-save)
  ("x" kill-region)
  ("v" yank :exit t)
  ("e" exchange-point-and-mark "exchange")
  ("m" set-mark-command)
  ("w" other-window)
  ("n" scroll-up-command)
  ("p" scroll-down-command)
  ("r" recenter-top-bottom))

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


(defhydra hydra-dired (:exit t)
  ("SPC" nil "quit")
  ("o" dired-w32-browser "open")
  ("e" dired-w32explorer "explorer"))


(defhydra hydra-elisp (:exit t)
  ("x" eval-last-sexp "Eval last")
  ("e" eval-expression "Eval exp")
  ("b" eval-buffer "Eval buffer")
  ("r" eval-region "Eval region")
  ("i" info "info")
  ("c" list-colors-display "colors")
  ("f" list-faces-display "faces")
  ("SPC" keyboard-quit "quit"))


(defhydra hydra-cpp (:exit t)
  "
_a_: list tags
"
  ("a" counsel-etags-list-tag "list tags")
  ("c" eye/create-ctags-file "create TAGS by git") ;;counsel-etags-scan-code "create TAGS"
  ("d" counsel-etags-find-tag-at-point "find tag at point")
  ("e" counsel-etags-find-tag "find tag")
  ("r" counsel-etags-recent-tag "recent tag")
  ("t" eye/update-ctags-this-file "update file tags")
  ("f" eye/find-header-or-source-file "find h or cpp")
  ("l" eye/load-project-root-tags "load root tags")
  ("s" eye/search-cpp-doc "cpp doc")
  ("g" eye/auto-compile "compile"))


(defhydra hydra-org (:exit t)
  "
[_gp_]: previous block

Insert:
[_il_] link    [_s_] src block    [_S_] subheading

Toggle:
[_tl_] display link

Wiki:
[_wc_] insert block
[_wo_] open at point
[_wu_] open url
[_wf_] open from url
[_we_] export page 

"
  ("SPC" nil "quit")
  ("gp" org-previous-block)
  ("il" org-insert-link)
  ("s" eye/org-insert-src-block)
  ("S" org-insert-subheading)
  ("tl" org-toggle-link-display)
  ("we" my-org-wiki-auto-export-hook)
  ("wo" org-open-at-point)
  ("wu" my-org-wiki-open-url)
  ("wf" my-org-wiki-from-url)
  ("wc" org-wiki-insert-block)
  )

(defhydra hydra-highlight ()
  "symbol-overlay"
  ("h" symbol-overlay-put "put")
  ("n" symbol-overlay-jump-next "next")
  ("p" symbol-overlay-jump-prev "prev")
  ("f" symbol-overlay-jump-first "first")
  ("l" symbol-overlay-jump-last "last")
  ("r" symbol-overlay-remove-all "remove all"))


(defhydra hydra-watch-other ()
  ("SPC" nil "quit")
  ("i" watch-other-window-down-line "Down line")
  ("k" watch-other-window-up-line "Up line")
  ("p" watch-other-window-down "Down scroll")
  ("n" watch-other-window-up "Up scroll"))


(defhydra hydra-gtd (:exit t)
  "
Getting Thing Done system:
  [_c_] org-capture [_n_] new note  [_b_] new blog    [_s_] search note keyword    [_f_] search note file
  [_i_] 查看收集蓝（处理）                             
  [_t_] 查看任务（建立清单）                           
  [_o_] 查看TODO项（准备下一步行动） 
  [_x_] 查看下一步行动
  [_T_] org-clock-sum-today-by-tags

  [_a_] agenda    [_j_] new journal    [_d_] notdeft

Clock:
  [_1_] in   [_2_] out   [_3_] report   [_4_] cancel
"
  ("SPC" nil "quit")
  ("a" org-agenda nil)
  ("c" org-capture nil)
  ("d" notdeft nil)
  ("j" org-journal-new-entry nil)
  ("i" (lambda () (interactive) (org-agenda nil "i")) nil)
  ("t" (lambda () (interactive) (org-agenda nil "t")) nil)
  ("o" (lambda () (interactive) (org-agenda nil "o")) nil)
  ("x" (lambda () (interactive) (org-agenda nil "x")) nil)
  ("T" org-clock-sum-today-by-tags)
  ("n" org-note-new)
  ("b" org-blog-new)
  ("s" org-note-search-keywords)
  ("f" org-note-search-title)
  ("1" org-clock-in)
  ("2" org-clock-out)
  ("3" org-clock-report)
  ("4" org-clock-cancel))

(defun eye/major-mode-key ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode) (call-interactively 'hydra-elisp/body))
   ((eq major-mode 'lisp-interaction-mode) (call-interactively 'hydra-elisp/body))
   ((eq major-mode 'c-mode) (call-interactively 'hydra-cpp/body))
   ((eq major-mode 'c++-mode) (call-interactively 'hydra-cpp/body))
   ((eq major-mode 'python-mode) (call-interactively 'hydra-python/body))
   ((or (eq major-mode 'org-mode)
	(eq major-mode 'org-journal-mode))
    (call-interactively 'hydra-org/body))
   (t nil)))


;;;; bind keys
(bind-key global-map "C-v" #'scroll-half-page-up)
(bind-key global-map "M-v" #'scroll-half-page-down)
(bind-key global-map "C-x C-b" #'switch-to-buffer)
(bind-key global-map "<C-wheel-up>" #'eye/increase-font-size)
(bind-key global-map "<C-wheel-down>" #'eye/decrease-font-size)
(bind-key global-map "C-=" #'eye/increase-font-size)
(bind-key global-map "C--" #'eye/decrease-font-size)
(autoload 'eye/kill-inner-word "base-toolkit" "" t)
(bind-key global-map "<M-backspace>" #'eye/kill-inner-word)
(bind-key global-map "<C-backspace>" #'eye/kill-inner-word)
;; running on msys2, can't use C-c, it is <pause>
(when is-terminal (bind-key global-map "C-x <pause>" #'kill-emacs))
(defalias 'backward-kill-word 'eye/kill-inner-word)
;; use [ESC] replace [C-g]
;; 终端下不替换，否则alt+x失效，alt是ESC
(when is-gui (define-key key-translation-map (kbd "ESC") (kbd "C-g")))

;;;; windmove
(bind-key global-map "C-<left>" #'windmove-left)
(bind-key global-map "C-<up>" #'windmove-up)
(bind-key global-map "C-<right>" #'windmove-right)
(bind-key global-map "C-<down>" #'windmove-down)

(bind-key global-map "<f1>" #'helm-buffers-list)
(bind-key global-map "<f2>" #'helm-find-files)
(bind-key global-map "<f3>" #'toggle-input-method)
(bind-key global-map "M-i" #'toggle-input-method)
(bind-key global-map "<f4>" #'snails)

(bind-key global-map "<f6>" #'org-capture)
(bind-key global-map "<f7>" #'org-agenda)
(bind-key global-map "<f8>" #'notdeft)


(bind-key global-map "C-," nil)
(bind-key global-map "M-," nil)
;;(bind-key global-map "M-," #'(lambda () (interactive) (insert ",")))
;;(with-eval-after-load 'org-agenda-mode
;;  (progn
;;    (bind-key org-agenda-mode-map "," nil)
;;    (bind-key org-agenda-mode-map "M-," nil)
;;    (bind-key org-agenda-mode-map "M-," #'(lambda () (interactive) (insert ",")))))



;;(bind-key global-map "M-x" #'helm-M-x)
(bind-key global-map "M-x" #'counsel-M-x "swiper")

(bind-key global-map "C-, 1" #'delete-other-windows)
(bind-key global-map "C-, 2" #'split-window-below)
(bind-key global-map "C-, 3" #'split-window-right)
(bind-key global-map "C-, 4g" #'awesome-tab-switch-group "awesome-tab")
(bind-key global-map "C-, 4f" #'awesome-tab-forward-tab "awesome-tab")
(bind-key global-map "C-, 4b" #'awesome-tab-backward-tab "awesome-tab")
(bind-key global-map "C-, 4n" #'awesome-tab-forward-group "awesome-tab")
(bind-key global-map "C-, 4p" #'awesome-tab-backward-group "awesome-tab")
(bind-key global-map "C-, 8" #'hydra-select/body)
(bind-key global-map "C-, a" #'beginning-of-line)
(bind-key global-map "C-, b" #'switch-to-buffer)
(bind-key global-map "C-, c" #'kill-ring-save)

(bind-key global-map "C-, dd" #'delete-line-no-copy "base-toolkit")
(bind-key global-map "C-, db" #'delete-beginning-of-line-no-copy)
(bind-key global-map "C-, de" #'delete-end-of-line-no-copy)

(bind-key global-map "C-, e" #'end-of-line)

(bind-key global-map "C-, ii" #'counsel-imenu "counsel")
(bind-key global-map "C-, ie" #'eye/imenu-init)
(bind-key global-map "C-, id" #'eye/insert-date)
(bind-key global-map "C-, m" (lambda ()
			     (interactive)
			     (call-interactively 'set-mark-command)
			     (call-interactively 'hydra-move/body)))

(bind-key global-map "C-, tr" #'global-readonly-toggle "global-readonly-mode")
(bind-key global-map "C-, tl" #'global-display-line-numbers-mode)
(bind-key global-map "C-, tt" #'toggle-truncate-lines)
(bind-key global-map "C-, tc" #'global-company-mode)
(bind-key global-map "C-, th" #'highlight-changes-mode)
(bind-key global-map "C-, tn" #'highlight-numbers-mode)
(bind-key global-map "C-, tp" #'rainbow-delimiters-mode)
(bind-key global-map "C-, tP" #'show-paren-mode)
(bind-key global-map "C-, tv" #'global-visual-line-mode)
(bind-key global-map "C-, tR" #'rainbow-mode "rainbow-mode")
(bind-key global-map "C-, tw" #'whitespace-mode)
(bind-key global-map "C-, tf" #'global-font-lock-mode)
(bind-key global-map "C-, tF" #'fullscreen-toggle)
(bind-key global-map "C-, tC" #'centered-cursor-mode "centered-cursor-mode")
(bind-key global-map "C-, tW" #'writeroom-mode)
(bind-key global-map "C-, ty" #'yas-global-mode)

(bind-key global-map "C-, fd" #'dired-jump "dired-x")
(bind-key global-map "C-, ff" #'helm-find-files "helm-files")
(bind-key global-map "C-, fh" #'helm-recentf "helm-for-files")
(bind-key global-map "C-, fo" #'find-file-other-window)
(bind-key global-map "C-, fk" #'kill-this-buffer)
(bind-key global-map "C-, fs" #'save-buffer)
(bind-key global-map "C-, fg" #'counsel-git "counsel") ;;查找在git仓库中的文件，注意最好子目录下没有.git目录，否则可能不会显示出文件列表

(bind-key global-map "C-, sa" #'counsel-ag "counsel")
(bind-key global-map "C-, ss" #'occur)
(bind-key global-map "C-, so" #'multi-occur-in-matching-buffers)
(bind-key global-map "C-, sr" #'color-rg-search-input "color-rg")
(bind-key global-map "C-, sq" #'query-replace)

(bind-key global-map "C-, gg" #'goto-line)
(bind-key global-map "C-, gc" #'ace-jump-char-mode "ace-jump-mode")
(bind-key global-map "C-, gl" #'ace-jump-line-mode "ace-jump-mode")
(bind-key global-map "C-, gi" #'avy-goto-char-in-line "avy")
(bind-key global-map "C-, gt" #'bm-toggle)
(bind-key global-map "C-, gp" #'bm-previous)
(bind-key global-map "C-, gn" #'bm-next)
(bind-key global-map "C-, gs" #'counsel-bm)

(bind-key global-map "C-, h" #'hydra-highlight/body)
(bind-key global-map "C-, o" #'hydra-outline/body)
(bind-key global-map "C-, rr" #'replace-rectangle)
(bind-key global-map "C-, rk" #'kill-rectangle)

(bind-key global-map "C-, v" #'yank)
(bind-key global-map "C-, w" #'other-window)
(bind-key global-map "C-, W" #'hydra-watch-other/body)

(bind-key global-map "C-, x" #'kill-region)
(bind-key global-map "C-, y" #'yankpad-insert)
(bind-key global-map "C-, z" #'undo)

(bind-key global-map "C-, /" #'comment-dwim)
(bind-key global-map "C-, ." #'eye/major-mode-key)

(bind-key global-map "C-, C-," #'hydra-move/body)



(bind-key global-map "M-, aa" #'aweshell-toggle "aweshell")
(bind-key global-map "M-, aN" #'aweshell-new "aweshell")
(bind-key global-map "M-, an" #'aweshell-next "aweshell")
(bind-key global-map "M-, ap" #'aweshell-prev "aweshell")

(bind-key global-map "M-, wg" #'prelude-google "init-web-search")
(bind-key global-map "M-, wb" #'prelude-bing "init-web-search")
(bind-key global-map "M-, wd" #'prelude-duckduckgo "init-web-search")
(bind-key global-map "M-, wG" #'prelude-github "init-web-search")
(bind-key global-map "M-, wy" #'prelude-youtube "init-web-search")

(bind-key global-map "M-, o" #'hydra-watch-other/body "init-watch-other-window")

(bind-key global-map "M-, g" #'hydra-gtd/body)
(bind-key global-map "M-, d" #'bing-dict-brief)
(bind-key global-map "M-, m" #'magit-status)

(bind-key global-map "M-, xr" #'xref-find-references)
(bind-key global-map "M-, xd" #'xref-find-definitions)


(provide 'init-leader-key)

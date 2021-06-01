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


(defun async-shell-command-no-window (command)
  (let ((display-buffer-alist
         (list (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil))))
		(output-buffer (format "*Async Shell Command*.%s" (random 1000)))
		(error-buffer (format "*Async Shell Command*.%s" (random 1000)))
		)
    (async-shell-command command output-buffer error-buffer)))

;; 会影响后续load path?
(when is-windows
  (async-shell-command-no-window (expand-file-name "bin/windows-keys.ahk" user-emacs-directory)))



;;;; one-key
(auto-require
 'one-key
 :load t
 :urls '(("one-key" . "https://github.com/manateelazycat/one-key.git")))

(auto-require
 'hydra
 :load t
 :urls '(("hydra" . "https://github.com/abo-abo/hydra.git")))


(one-key-create-menu
 "WINDOW"
 '(
   (("1" . "Delete other windows") . delete-other-windows)
   (("2" . "Split window below") . split-window-below)
   (("3" . "Split window right") . split-window-right)
   (("w" . "other window") . other-window)
   ) t)

(one-key-create-menu
 "OUTLINE"
 '(
   (("e" . "Enable outline") . outline-minor-mode)
   (("a" . "Show entry") . outline-show-entry)
   (("h" . "Hide entry") . outline-hide-entry)
   (("n" . "Next heading") . outline-next-heading)
   (("p" . "Prev heading") . outline-previous-heading)
   (("a" . "Show all") . outline-show-all)
   ) t)


(one-key-create-menu
 "DIRED"
 '(
   (("o" . "Open win32") . dired-w32-browser)
   (("e" . "Open explorer") . dired-w32explorer)
   ) t)

(one-key-create-menu
 "HELP"
 '(
   (("v" . "Help var") . describe-variable)
   (("f" . "Help function") . describle-function)
   (("k" . "Help key") . describe-key)
   (("a" . "Desc face") . describe-face)
   (("m" . "Desc mode") . describe-mode)
   (("c" . "List colors") . list-colors-display)
   (("s" . "List faces") . list-faces-display)
   ) t)


(one-key-create-menu
 "SEARCH"
 '(
   (("c" . "color rg") . color-rg-search-input)
   ) t)

(one-key-create-menu
 "File"
 '(
   (("r" . "Recent file") . recentf-open-files)
   (("o" . "Open file") . counsel-find-file)
   (("s" . "Save file") . save-buffer)
   (("g" . "Open git file") . counsel-git)
   (("f" . "Switch buffer") . counsel-switch-buffer)
   (("k" . "Kill buffer") . kill-this-buffer)
   (("d" . "dired") . dired-jump)
   (("b" . "Bookmark jump") . bookmark-jump)
   )
 t)


(one-key-create-menu
 "Agenda"
 '(
   (("a" . "agenda") . org-agenda)
   (("c" . "capture") . org-capture)
   (("p" . "group by project") . eye/agenda-by-proj)
   )
 t)

(one-key-create-menu
 "bm"
 '(
   (("t" . "bm toggle") . bm-toggle)
   (("p" . "bm prev") . bm-previous)
   (("n" . "bm next") . bm-next)
   (("s" . "bm switch") . counsel-bm)
   )
 t)


(one-key-create-menu
 "Programming"
 '(
   (("a" . "List tags") . counsel-etags-list-tag)
   (("d" . "Find tag at point") . counsel-etags-find-tag-at-point)
   (("f" . "Find tag") . counsel-etags-find-tag)
   (("r" . "Recent tags") . counsel-etags-recent-tag)
   (("t" . "Create Tags") . eye/create-ctags-file)
   )
 t)


(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("k" . meow-next)
   '("i" . meow-prev))
  (meow-leader-define-key
   ;; SPC k/i will run the original command in MOTION state.
   '("k" . meow-motion-origin-command)
   '("i" . meow-motion-origin-command)
   '("s" . one-key-menu-search)
   '("w" . one-key-menu-window)
   '("?" . one-key-menu-help)
   '("f" . one-key-menu-file)
   '("j" . one-key-menu-bm)
   '("c" . one-key-menu-programming)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("D" . meow-C-d)
   '("DEL" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("s" . meow-find)
   '("S" . meow-find-expand)
   '("g" . meow-cancel)
   '("G" . meow-grab)
   '("h" . beginning-of-line)
   '(";" . end-of-line)
   '("j" . meow-left)
   '("J" . meow-left-expand)
   '("f" . meow-insert)
   '("I" . meow-open-above)
   '("k" . meow-next)
   '("J" . meow-next-expand)
   '("i" . meow-prev)
   '("I" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("o" . meow-block)
   '("O" . meow-block-expand)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("d" . meow-kill)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("V" . meow-kmacro-matches)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-kmacro-lines)
   '("y" . meow-save)
   
   ;;'("Y" . meow-sync-grab)
   ;;'("z" . meow-pop-selection)
   ;;'("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("'" . repeat)
   '("\\" . quoted-insert)
   ))




(auto-require
 'meow
 :load t
 :urls '(("meow" . "https://github.com/DogLooksGood/meow.git"))
 :after
 (progn
   (meow-global-mode 1)
   (meow-setup)
   (meow-setup-indicator)
   ))

(bind-key global-map "M-SPC" 'meow-insert-exit)
;;(bind-key global-map "<home>" 'meow-insert-exit)
(bind-key global-map "<f7>" 'one-key-menu-agenda)

;; 注意，没有加载的mode，会报错，导致后面的设置不生效
(with-eval-after-load 'org
  (bind-key org-mode-map "M-RET" 'org-insert-heading-respect-content)
  (bind-key org-mode-map "C-k" nil)
  (bind-key org-mode-map "C-c '" 'org-edit-src-code))


(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-i") 'company-select-previous)
  (define-key company-active-map (kbd "M-k") 'company-select-next)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map (kbd "<S-tab>") 'company-select-previous))

(provide 'eye-keybindings)

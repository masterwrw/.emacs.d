;;; all key bindings in this file

;; general, More convenient key definitions in emacs
(require-package 'general)
(require 'general)


;; remap escape key to C-g if not enable evil mode
(if (not (bound-and-true-p evil-mode))
    (bind-key "<escape>" 'minibuffer-keyboard-quit))


;(defalias 'list-buffers 'ibuffer-other-window) ; make ibuffer-other-window default
(defalias 'list-buffers 'ibuffer) ; make ibuffer default


(defhydra hydra-code-browser (:color blue)
  "
_a_ dumb go             _v_ xref reference      _e_ hide block
_s_ xref definitions    _c_ helm reference      _r_ show block
_b_ dumb back           _d_ helm find symbol    _w_ goto last change
_z_ semantic jump
_g_ magit status

_q_ quit
"
  ("a" dumb-jump-go)     ;; dumb-jump under samba share directory will be very slow, and not work.
  ("b" dump-jump-back)
  ("z" semantic-ia-fast-jump)
  ("c" helm-gtags-find-rtag)
  ("s" xref-find-definition)
  ("v" xref-find-reference)
  ("d" helm-gtags-find-symbol)
  ("w" goto-last-change)
  ("e" hs-hide-block)
  ("r" hs-show-block)
  ("g" magit-status)
  ("q" nil))
(bind-key "<f8>" 'hydra-code-browser/body)


;;;================== General keys ========================
(setq my-leader-key "<f9>")
(general-define-key
 :prefix my-leader-key
 "a"   'mark-whole-buffer
 "br"  'hydra-org-brain/body
 "bk"  'bookmark-bmenu-list
 "cc"  'xah-copy-line-or-region
 "C-c" 'xah-copy-line-or-region
 "ca"  'org-capture
 "d"   'dumb-jump-go
 "f"   'isearch-forward
 "gg"  'magit-status
 "gl"  'goto-line
 "j"   'hydra-code-browser
 "k"   'xah-cut-line-or-region
 "m"   'set-mark-command
 "n"   'xah-new-empty-buffer
 "o"   'other-window
 "q"   'beginning-of-visual-line
 "r"   'replace-string
 "uu"  'uuidgen
 "v"   'yank
 "C-v" 'yank
 "w"   'ibuffer
 "x"   'xah-cut-line-or-region
 "y"   'redo
 "z"   'undo
 "C-z" 'undo
 "/" 'comment-or-uncomment-region
 "C-/" 'comment-or-uncomment-region)



;;;========================================================
;; Switch buffer
(bind-key "<f1>" 'switch-to-buffer)
(bind-key "C-<f1>" 'helm-buffers-list)
(bind-key "C-1" 'list-buffers)

;; Find in file or find in git project
(bind-key "<f2>" 'swiper)
(if *is-windows*
    (bind-key "C-<f2>" 'helm-ag)
  (bind-key "C-<f2>" 'counsel-ag))
(bind-key "C-2" 'helm-do-ag)

;; Close other frame or window
(bind-key "<f3>" 'delete-other-windows)
(bind-key "C-<f3>" 'delete-other-frames)

;; Kill buffer or kill tab
(bind-key "<f4>" 'kill-buffer)
(bind-key "C-<f4>" 'elscreen-kill)

;; Split window
(bind-key "<f5>" 'split-window-right)
(bind-key "C-<f5>" 'split-window-below)
(bind-key "S-<f5>" 'delete-other-windows)

;; Open file
(bind-key "<f6>" 'counsel-find-file)
(bind-key "C-<f6>" 'ffip)
(bind-key "C-6" 'helm-projectile-find-file)



;(bind-key "<f9>" 'imenu-list-minor-mode)


(if *is-windows*
    (bind-key "<f11>" 'helm-gtags-find-rtag))


;;; Some key may not work.
(bind-key "C-s" 'save-buffer)
(bind-key "C-S-s" 'write-file)
(bind-key "C-f" 'isearch-forward)
(bind-key "C-n" 'xah-new-empty-buffer)
(bind-key "C-S-n" 'make-frame-command)
(bind-key "C-v" 'yank)
(bind-key "C-y" 'redo)
(bind-key "C-z" 'undo)
(bind-key "C-/" 'comment-or-uncomment-region)


;(bind-key "C-o" 'ace-window)
;; map M-c to M-w(copy)
;(define-key key-translation-map [(meta c)] [(meta w)])
;; map M-v to C-y(paste)
;(define-key key-translation-map [(meta v)] [(control y)])


(bind-key "M-x" 'counsel-M-x)

(bind-key "M-s" 'avy-goto-char)


(if *is-windows*
    (bind-key "C-<wheel-up>" 'text-scale-increase)
    (bind-key "C-<mouse-4>" 'text-scale-increase))

(if *is-windows*
    (bind-key "C-<wheel-down>" 'text-scale-decrease)
    (bind-key "C-<mouse-5>" 'text-scale-decrease))

(bind-key "C-=" 'cnfonts-increase-fontsize)
(bind-key "C--" 'cnfonts-decrease-fontsize)

(if *is-windows*
    (bind-key "<wheel-up>" 'scroll-down-lines)
    (bind-key "<mouse-4>" 'scroll-down-lines))
(if *is-windows*
    (bind-key "<wheel-down>" 'scroll-up-lines)
    (bind-key "<mouse-5>" 'scroll-up-lines))

(bind-key "C-t" 'elscreen-create)
;(bind-key "<C-tab>" 'elscreen-next)
(bind-key "<C-iso-lefttab>" 'elscreen-previous) ; Ctrl+Shift+Tab
(bind-key "<C-tab>" 'my-auto-switch-buffer)


(bind-key "C-`" 'set-mark-command)


(bind-key "M-0" '(lambda() (interactive) (elscreen-goto 0)))
(bind-key "M-1" '(lambda() (interactive) (elscreen-goto 1)))
(bind-key "M-2" '(lambda() (interactive) (elscreen-goto 2)))
(bind-key "M-3" '(lambda() (interactive) (elscreen-goto 3)))
(bind-key "M-4" '(lambda() (interactive) (elscreen-goto 4)))
(bind-key "M-5" '(lambda() (interactive) (elscreen-goto 5)))
(bind-key "M-6" '(lambda() (interactive) (elscreen-goto 6)))
(bind-key "M-7" '(lambda() (interactive) (elscreen-goto 7)))
(bind-key "M-8" '(lambda() (interactive) (elscreen-goto 8)))
(bind-key "M-9" '(lambda() (interactive) (elscreen-goto 9)))


(bind-key "C-<left>" 'backward-forward-previous-location)
(bind-key "C-<right>" 'backward-forward-next-location)
;(bind-key "C-<left>" 'jump-to-prev-pos)
;(bind-key "C-<right>" 'jump-to-next-pos)


(bind-key "M-<down>" 'my-fast-step-downward)
(bind-key "M-<up>" 'my-fast-step-upward)
(bind-key "M-<right>" 'forward-word)
(bind-key "M-<left>" 'backward-word)
(bind-key "M-<delete>" 'kill-word)
(bind-key "M-<backspace>" 'backward-kill-word)


;;; isearch configuration
(progn
  ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )

  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)

  (define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
  (define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer))




(defhydra hydra-edit (:color blue)
  "
_i_ change-inner     _o_ chnage-outer      _p_ sp-change-inner
_g_ goto-last-change _r_ goto-last-change-reverse
_l_ goto-line        _q_ quit
"
  ("i" change-inner)
  ("o" change-outer)
  ("p" sp-change-inner)
  ("g" goto-last-change)
  ("r" goto-last-change-reverse)
  ("l" goto-line)
  ("q" nil))
;(bind-key "<f9>" 'hydra-edit/body)


;; dumb-jump key bind
(defhydra hydra-dumb-jump (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")
    ("q" nil "quit"))


; go to line
(defhydra hydra-goto-line (goto-map ""
                           :pre (linum-mode 1)
                           :post (linum-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("q" nil "quit"))

(defhydra hydra-zoom (global-map "<f10>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))


(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(define-key dired-mode-map "." 'hydra-dired/body)


;; ibuffer key map
(require 'ibuffer)
(defhydra hydra-ibuffer-mark (:color teal :columns 5
                              :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                :after-exit
                                (if (eq major-mode 'ibuffer-mode)
                                    (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("s" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" quit-window "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

;(define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)
(add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)


;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

;(bind-key "<f7>" 'hydra-org-agenda/body)



;; command
(defhydra hydra-launcher (:color blue)
  "Launch"
  ("s" (browse-url "http://www.bing.com"))
  ("g" (lambda ()
         (interactive)
         (let ((current-prefix-arg 4))
           (call-interactively #'magit-status)))
       "git")
  ("q" nil "cancel"))

;(bind-key "<f8>" 'hydra-launcher/body)


;; hydra vi
(defhydra hydra-vi (:pre (set-cursor-color "#40e0d0")
                    :post (progn
                            (set-cursor-color "#ffffff")
                            (message
                             "Thank you, come again.")))
  "vi"
  ("l" forward-char)
  ("h" backward-char)
  ("j" next-line)
  ("k" previous-line)
  ("q" nil "quit"))



; (global-set-key (kbd "M-x") 'my-M-x)
; (global-set-key (kbd "C-x C-m") 'my-M-x)
; (global-set-key (kbd "<f8>") 'helm-do-ag-project-root)
; (bind-key "<f2>" 'helm-do-ag-project-root)
; (bind-key "C-c C-r" 'ivy-resume)
; (bind-key "C-x C-f" 'counsel-find-file)
; (bind-key "<f1> f" 'counsel-describe-function)
; (bind-key "<f1> v" 'counsel-describe-variable)
; (bind-key "<f1> l" 'counsel-load-library)
; (bind-key "<f2> i" 'counsel-info-lookup-symbol)
; (bind-key "<f2> u" 'counsel-unicode-char)
; (bind-key "C-c g" 'counsel-git)
; (bind-key "C-c j" 'counsel-git-grep)
; (bind-key "C-c k" 'counsel-ag)
; (bind-key "C-x l" 'counsel-locate)
; (bind-key "C-S-o" 'counsel-rhythmbox)




(provide 'init-keymap)

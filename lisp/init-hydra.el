(require 'hydra)

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
  "
[_c_] capture
[_a_] agenda
[_p_] pop mark

Toggle mode:
[_l_] line number
[_r_] readonly
[_t_] truncate lines
[_g_] company
[_h_] highlight changes

[_n_] Note

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
"
  ("SPC" nil "quit" :exit t)
  ("p" pop-global-mark :exit t)
  ("r" read-only-mode :exit t)
  ("l" global-display-line-numbers-mode)
  ("t" toggle-truncate-lines)
  ("g" global-company-mode :exit t)
  ("c" org-capture :exit t)
  ("a" org-agenda :exit t)
  ("n" hydra-note/body :exit t)
  ("h" highlight-changes-mode :exit t)
  )


(defhydra hydra-dired (:exit t)
  ("SPC" nil "quit")
  ("o" dired-w32-browser "open")
  ("e" dired-w32explorer "explorer"))


(defhydra hydra-elisp (:exit t)
  ("x" eval-last-sexp "Eval last")
  ("e" eval-expression "Eval exp")
  ("i" imenu "imenu")
  ("SPC" keyboard-quit "quit"))



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


(defhydra hydra-note (:exit t :idle 1.0)
  "
[_d_] open note dired
[_n_] new note
[_s_] search by keyword
[_f_] search by file name

[_t_] deft
"
  ("SPC" nil "quit")
  ("d" eye/notes-dired)
  ("n" eye/notes-new)
  ;; ("a" eye/notes-create-attachment "Create attach dir")
  ;; ("o" eye/notes-open-attachment "Open attach")
  ("s" eye/notes-search-keyword)
  ("f" eye/notes-search-file)
  ("t" deft-or-close)
  )


(defhydra hydra-org (:exit t)
  "
[_a_]: attach

Clock:
[_ci_] in
[_co_] out
[_cr_] report
[_cc_] cancel

Insert:
[_l_] link                    [_h_] sub heading
[_t_] attach link
[_s_] src block

Toggle:
[_d_] display link
[_g_] inline image

"
  ("SPC" nil "quit")
  ("ci" org-clock-in)
  ("co" org-clock-out)
  ("cr" org-clock-report)
  ("cc" org-clock-cancel)
  ("a" org-attach)
  ("t" eye/insert-attach-link)
  ("l" org-insert-link)
  ("s" eye/org-insert-src-block)
  ("h" org-insert-subheading)
  ("d" org-toggle-link-display)
  ("g" org-toggle-inline-images))


(defhydra hydra-symbol-overlay ()
  "
Highlight:
[_h_] at point
"
  ("h" symbol-overlay-put))


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


(provide 'init-hydra)

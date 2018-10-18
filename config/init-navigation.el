(require 'counsel-etags)
;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)
;; Setup auto update now
(add-hook 'prog-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook
		      'counsel-etags-virtual-update-tags 'append 'local)))

(with-eval-after-load 'counsel-etags
  ;; counsel-etags-ignore-directories does NOT support wildcast
  (add-to-list 'counsel-etags-ignore-directories ".git")
  (add-to-list 'counsel-etags-ignore-directories ".svn")
  ;; counsel-etags-ignore-filenames supports wildcast
  (add-to-list 'counsel-etags-ignore-filenames "TAGS")
  (add-to-list 'counsel-etags-ignore-filenames "*.json")
  (add-to-list 'counsel-etags-ignore-filenames "ui_*.h")
  (add-to-list 'counsel-etags-ignore-filenames "*.ui")
  (add-to-list 'counsel-etags-ignore-filenames "moc_*.cpp")
  (add-to-list 'counsel-etags-ignore-filenames "*.rc")
  (add-to-list 'counsel-etags-ignore-filenames "*.qrc")
  (add-to-list 'counsel-etags-ignore-filenames "*.user"))

;; You can change callback counsel-etags-update-tags-backend to update tags file using your own solution,
;;;(setq counsel-etags-update-tags-backend (lambda () (shell-command "find . -type f -iname \"*.[ch]\" | etags -")))

(require 'backward-forward)
(advice-add 'counsel-etags-find-tag-at-point :before #'backward-forward-push-mark-wrapper)
(backward-forward-mode t)

(require 'dumb-jump)
(advice-add 'dumb-jump-go :before #'backward-forward-push-mark-wrapper)


;; ibuffer
(setq ibuffer-saved-filter-groups
      '(("Default"
         ("Hidden(g则不显示此分组)"  (name . "^ "))
         ("Helm"  (or (name . "^\\*helm\\|^\\*ac-mode-")))
         ("Help"  (or (name . "^\\*help\\|^\\*ac-mode-")))
         ("Woman"  (name . "^\\*WoMan.*\\*$"))
         ("Compile"  (name . "^*.*compil[ea].*$"))
         ("Gtalk"  (or (name . "^\\*.*jabber") (name . "*fsm-debug*")))
         ("ERC"  (mode . erc-mode))
         ("Custom"  (mode . Custom-mode))
         ("Shell"  (mode . shell-mode))
         ("Mail" (or (mode . mew-summary-mode) (mode . mew-draft-mode)(mode . mew-message-mode)))
         ("VC"  (or (name . "*magit-") (name . "^\\*vc")(mode . diff-mode) (mode . vc-dir-mode)))
         ("Magit "  (name . "*magit:"))
         ("Emacs"  (name . "^\\*.*$"))
         ("Dired"  (mode . dired-mode))
         ("Go"  (mode . go-mode))
         ("Python"  (mode . python-mode))
         ("EL"  (or (mode . emacs-lisp-mode) (mode . lisp-interaction-mode)))
	 ("C++" (mode . c++-mode))
	 ("Text" (name . ".txt"))
         )))

(add-hook 'ibuffer-mode-hook
	    '(lambda ()
		(ibuffer-auto-mode 1)
		(ibuffer-switch-to-saved-filter-groups "EL")))
(setq ibuffer-show-empty-filter-groups nil)




(provide 'init-navigation)

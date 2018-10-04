(require 'awesome-tab)

(awesome-tab-mode t)

(set-face-attribute 'awesome-tab-selected nil :foreground "cyan3")
(set-face-attribute 'awesome-tab-unselected nil :foreground "cyan4")

(ryo-modal-keys
 ("tt" awesome-tab-switch-group)
 ("tj" awesome-tab-backward)
 ("tl" awesome-tab-forward)
 ("th" awesome-tab-select-beg-tab)
 ("t;" awesome-tab-select-end-tab)
 ("tk" awesome-tab-forward-group)
 ("ti" awesome-tab-backward-group)
 ("C-." 
  (("tj" awesome-tab-move-current-tab-to-left)
   ("tl" awesome-tab-move-current-tab-to-right)
   ("tk" awesome-tab-kill-all-buffers-in-current-group)
   ("to" awesome-tab-kill-other-buffers-in-current-group)))
 )

(provide 'tab-init)

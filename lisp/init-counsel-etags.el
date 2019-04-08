
(with-eval-after-load 'counsel-etags
;;;; counsel-etags
  ;;(require 'counsel-etags)
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)

  (when is-linux
    (setq counsel-etags-tags-program "xargs etags --append") ;调用命令类似 find .... -print | xargs etags --append, etags没有递归的参数
    )

  ;; 是否开启输出命令
  (setq counsel-etags-debug nil)

  ;;(append-to-list 'counsel-etags-extra-tags-files locale-system-tags-paths) ;;使counsel-etags能显示系统函数（但无法跳转进入）
  
  ;; Setup auto update now
  (add-hook 'c++-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'eye/update-ctags-this-file)))

  ;; counsel-etags-ignore-directories does NOT support wildcast
  (add-to-list 'counsel-etags-ignore-directories ".git")
  (add-to-list 'counsel-etags-ignore-directories ".svn")
  (add-to-list 'counsel-etags-ignore-directories ".vs")
  (add-to-list 'counsel-etags-ignore-directories "ipch")
  (add-to-list 'counsel-etags-ignore-directories "Debug")
  (add-to-list 'counsel-etags-ignore-directories "Release")
  (add-to-list 'counsel-etags-ignore-directories "Bin")
  (add-to-list 'counsel-etags-ignore-directories "tmp")
  ;; counsel-etags-ignore-filenames supports wildcast
  (add-to-list 'counsel-etags-ignore-filenames "TAGS")
  (add-to-list 'counsel-etags-ignore-filenames "GPATH")
  (add-to-list 'counsel-etags-ignore-filenames "GRTAGS")
  (add-to-list 'counsel-etags-ignore-filenames "GTAGS")
  (add-to-list 'counsel-etags-ignore-filenames "*.json")
  (add-to-list 'counsel-etags-ignore-filenames "ui_*.h")
  (add-to-list 'counsel-etags-ignore-filenames "*.ui")
  (add-to-list 'counsel-etags-ignore-filenames "moc_*.cpp")
  (add-to-list 'counsel-etags-ignore-filenames "*.rc")
  (add-to-list 'counsel-etags-ignore-filenames "*.qrc")
  (add-to-list 'counsel-etags-ignore-filenames "*.tlog")
  (add-to-list 'counsel-etags-ignore-filenames "*.md")
  (add-to-list 'counsel-etags-ignore-filenames "*.bat")
  (add-to-list 'counsel-etags-ignore-filenames "*.txt")
  (add-to-list 'counsel-etags-ignore-filenames "*.pdb")
  (add-to-list 'counsel-etags-ignore-filenames "*.filters")
  (add-to-list 'counsel-etags-ignore-filenames "*.user")
  (add-to-list 'counsel-etags-ignore-filenames "*.vcproj")
  (add-to-list 'counsel-etags-ignore-filenames "*.vcxproj")
  (add-to-list 'counsel-etags-ignore-filenames "*.db")
  (add-to-list 'counsel-etags-ignore-filenames "*.opendb")
  (add-to-list 'counsel-etags-ignore-filenames "*.htm")
  (add-to-list 'counsel-etags-ignore-filenames "*.user")
  (add-to-list 'counsel-etags-ignore-filenames "*.make")
  (add-to-list 'counsel-etags-ignore-filenames "*.sln")
  (add-to-list 'counsel-etags-ignore-filenames "*.exp")
  )


(defhydra+ hydra-ctags (:exit t)
  "
_f_:Find tag at point   _t_:Find other tag   _r_:Open recent tag
_a_:List all tag        _c_:Create tags
"
  ("SPC" nil "quit")
  ("f" counsel-etags-find-tag-at-point)
  ("t" counsel-etags-find-tag)
  ("r" counsel-etags-recent-tag)
  ("a" counsel-etags-list-tag)
  ("c" eye/create-ctags-file))






(provide 'init-counsel-etags)

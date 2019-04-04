(require 'counsel-etags)
;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)

(when is-linux
  (setq counsel-etags-tags-program "xargs etags --append") ;调用命令类似 find .... -print | xargs etags --append, etags没有递归的参数
  )

;; Setup auto update now
(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      'counsel-etags-virtual-update-tags 'append 'local)))

(with-eval-after-load 'counsel-etags
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

(defun eye/create-ctags-file ()
  "Create ctags file"
  (interactive)
  (let ((command (read-string "command: " "ctags -V -R")))
    (async-shell-command command)
    ))

;; You can change callback counsel-etags-update-tags-backend to update tags file using your own solution,
;;(setq counsel-etags-update-tags-backend (lambda () (shell-command "find . -type f -iname \"*.[ch]\" | etags -")))


(provide 'init-tags)

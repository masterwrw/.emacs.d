(eye--reset-time)

(defun eye/create-ctags-file ()
  "Create ctags file"
  (interactive)
  ;; ctags必须加-e选项，否则counsel-xxx-find-tag-xx无法识别其中的tagname
  (let ((tags-dir (ido-read-directory-name "TAGS DIR:"))
	(command "find %s \( -iwholename \"*.h\" -or -iwholename \"*.cpp\" \) -print | ctags -e -f %sTAGS -V -R -L -"))
    (setq command (format command tags-dir tags-dir))
    (message command)
    (async-shell-command command)
    ))

(defun eye/update-ctags-this-file ()
  "Update current file tags"
  (interactive)
  (let ((tags-path (locate-dominating-file default-directory "TAGS"))
	(command)
	(proc))
    (when tags-path
      (setq tags-path (expand-file-name "TAGS" tags-path))
      (setq command (format "ctags -e -a -f %s %s" tags-path (buffer-name))) ;; -a means append
      (message (concat "command:" command))
      (async-shell-command command)
      (delete-other-windows))))


;; Setup auto update now
(add-hook 'c++-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'eye/update-ctags-this-file)))


;;;; counsel-etags
(require 'counsel-etags)
;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)

(when is-linux
  (setq counsel-etags-tags-program "xargs etags --append") ;调用命令类似 find .... -print | xargs etags --append, etags没有递归的参数
  )

;; 是否开启输出命令
(setq counsel-etags-debug nil)


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



(eye--print-time "init-counsel-etags)



(provide 'init-counsel-etags)

(setq eye-org-file-attach-base-dir (concat locale-notebook-dir "/attach"))

(defun eye--get-org-file-attach-id ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (not (search-backward "* attach" nil t))
	(progn
	  (newline)
	  (insert "* attach")
	  ))
    (org-id-get-create)))

(defun eye/get-org-file-attach-path ()
  (interactive)
  (let ((id (eye--get-org-file-attach-id)))
    (when id
      (format "%s/%s" eye-org-file-attach-base-dir id)
    )))

(defun eye/open-org-file-attach-dir ()
  "Open explorer of current buffer directory."
  (interactive)
  (when (eq system-type 'windows-nt)
    (let* ((dir (eye/get-org-file-attach-path))
	   (explorer (replace-regexp-in-string "/" "\\\\" (executable-find "C:/Windows/SysWOW64/explorer")))
	   (command))
      (setq dir (encode-coding-string
		 (replace-regexp-in-string "/" "\\\\" dir) 'gbk-dos))
      (setq command (concat explorer " " dir))
      (shell-command command nil nil)
      (message command))
    ))

(setq eye-match-drop-file-exp
      (concat "\\(jpg\\|png\\|gif\\|"
	      "zip\\|xz\\|gz\\|7z\\|"
	      "jpeg\\|txt\\|doc\\|docx\\|xlsx\\|"
	      "apk\\|rar\\|md\\|json\\|"
	      "html\\|bak\\|db\\|"
	      "pptx\\|pdf\\)$"))

;; see http://that-year.blogspot.com/2008/10/emacs_5377.html?m=1
(defun my-dnd-insert-link (uri action)
  (if (and (eq 'org-mode major-mode)
	   ;;(string-match eye-match-drop-file-exp uri)
	   1 ;; ignore file extension
	   )
      (let* ((file-line (dnd-get-local-file-uri uri))
	     (attach-dir (eye/get-org-file-attach-path))
	     file-path file-name new-file-path)
	;; chinese path
	(setq file-path (replace-regexp-in-string "^file:" "" uri))
	(setq file-path (decode-coding-string (url-unhex-string file-path) 'utf-8))
	(setq file-name (file-name-nondirectory file-path))
	(setq new-file-path (expand-file-name file-name attach-dir))
	
	;; create attach dir
	(unless (f-directory? attach-dir)
	  (f-mkdir attach-dir))

	(if (f-exists-p new-file-path)
	    (message "file already exists!")
	  (progn    
	    ;; copy/move to attach dir
	    (if (>= (f-size file-path) (* 10 1024 1024))
		(if (yes-or-no-p "file size >= 10MB, move it?")
		    (f-move file-path new-file-path)
		  (f-copy file-path new-file-path))
	      (f-copy file-path new-file-path))
	    
	    (if (f-exists-p new-file-path)
		(message "copy/move file ok.")
	      (message "copy/move file failed."))
	    ))

	;; make sure insert with a new line
	(end-of-line)
	(newline)
	(insert (format "[[file:../../attach%s]]"
			(substring new-file-path (length eye-org-file-attach-base-dir))))
	(newline)
	)
    (dnd-open-file uri action)))



(defun eye/org-attach-enable ()
  "Enable my drop event handler."
  (setq dnd-protocol-alist
	`(("^file:" . my-dnd-insert-link)
	  ("^\\(https?\\|ftp\\|file\\|nfs\\)://" . my-dnd-insert-link))))


(eye/org-attach-enable)

(provide 'init-org-note-attach)

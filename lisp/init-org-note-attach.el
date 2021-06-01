(require 'f)

(setq eye-org-file-attach-base-dir (concat locale-notebook-dir "/attach"))
(setq eye-org-file-attach-auto-show-image t)

(defun eye--get-org-file-attach-id ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (not (search-backward "* attach" nil t))
	(progn
	  (newline)
	  (insert "******** attach")
	  ))
    (org-id-get-create)))

(defun eye/get-org-file-attach-path ()
  (interactive)
  (let ((id (eye--get-org-file-attach-id)))
    (when id
      (format "%s/%s" eye-org-file-attach-base-dir id)
    )))

(defun eye/open-org-file-attach-dir ()
  "Open explorer of current buffer directory.
locale-notebook-dir use absolute path for advise.
"
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

(setq eye-match-image-file-exp
      (concat "\\(jpg\\|jpeg\\|png\\|gif\\)$"))

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
	;; 由于图片加了描述时不能显示，所以区分对待
	(if (string-match eye-match-image-file-exp uri)
	    (insert (format "[[file:../../attach%s]]"
			    (substring new-file-path (length eye-org-file-attach-base-dir))))
	  (insert (format "[[file:../../attach%s][%s]]"
			  (substring new-file-path (length eye-org-file-attach-base-dir))
			  file-name)))
	(newline)
	(if (and (string-match eye-match-image-file-exp uri) eye-org-file-attach-auto-show-image)
		(org-redisplay-inline-images))
	)
    (dnd-open-file uri action)))



(defun eye/org-attach-enable ()
  "Enable my drop event handler."
  (setq dnd-protocol-alist
	`(("^file:" . my-dnd-insert-link)
	  ("^\\(https?\\|ftp\\|file\\|nfs\\)://" . my-dnd-insert-link))))


(eye/org-attach-enable)


(defun eye/paste-image-from-clipboard ()
  (interactive)
  (let* ((convert-path (executable-find "convert"))
	 (attach-dir (eye/get-org-file-attach-path))
	 (imagename (concat (format-time-string "%Y-%m-%d_%H-%M-%S") ".png"))
	 (tmppath (concat "d:\\\\" imagename))
	 (fullpath (concat attach-dir "/" imagename))
	command)
    (if convert-path
	(progn
	  ;; create attach dir
	  (unless (f-directory? attach-dir)
	    (f-mkdir attach-dir))
	  (setq command (format "%s clipboard: %s" convert-path tmppath))
	  (message (format "fullpath: %s" fullpath))
	  (shell-command command)
	  (f-move tmppath fullpath)
	  (insert (format "[[file:../../attach%s]]"
			  (substring fullpath (length eye-org-file-attach-base-dir))))
	  (newline)
	  (org-redisplay-inline-images)
	  )
      (message "convert.exe not found.")
      )))


(defun xah-html-encode-percent-encoded-url ()
  "Percent encode URL in current line or selection.
          Example:
          http://example.org/(Dürer)
          becomes
          http://example.org/(D%C3%BCrer)

          Example:
          http://example.org/文本编辑器
          becomes
          http://example.org/%E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8

          URL `http://ergoemacs.org/emacs/emacs_url_percent_decode.html'     
         Version 2018-10-26"
  ;; (interactive)
  (let ($p1 $p2 $input-str $newStr)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (setq $input-str (buffer-substring-no-properties $p1 $p2))
    (require 'url-util)
    (setq $newStr (url-encode-url $input-str))
    (if (string-equal $newStr $input-str)
        (progn (message "no change" ))
      (progn
        (delete-region $p1 $p2)
        (insert $newStr)))))

(defun xah-html-decode-percent-encoded-url ()
  "Decode percent encoded URL of current line or selection.

          Example:
           %28D%C3%BCrer%29
          becomes
           (Dürer)

          Example:
           %E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8
          becomes
           文本编辑器

          URL `http://ergoemacs.org/emacs/emacs_url_percent_decode.html'
          Version 2018-10-26"
  ;; (interactive)     
  (let ( $p1 $p2 $input-str $newStr)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (setq $input-str (buffer-substring-no-properties $p1 $p2))
    (require 'url-util)
    (setq $newStr (url-unhex-string $input-str))
    (if (string-equal $newStr $input-str)
        (progn (message "no change" ))
      (progn
        (delete-region $p1 $p2)
        (insert (decode-coding-string $newStr 'utf-8))))))

;; @see https://emacs-china.org/t/org-mode-link/17059/4
;; 把原函数的interactive特性去掉，自己包装了一下
;; 如果想直接对整个buffer转码，就把mark-whole-buffer前面的注释去掉
(defun buffer-url-decode()
  (interactive)
  ;; (mark-whole-buffer)
  (xah-html-decode-percent-encoded-url))

(defun buffer-url-encode()
  (interactive)
  ;; (mark-whole-buffer)
  (xah-html-encode-percent-encoded-url))

;; 设置全局快捷键
(global-set-key (kbd "C-x RET d") 'buffer-url-decode)
(global-set-key (kbd "C-x RET e") 'buffer-url-encode)


(provide 'init-org-note-attach)

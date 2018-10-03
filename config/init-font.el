;; cnfont generated
(set-face-attribute
 'default nil
 :font (font-spec :name "-outline-Source Code Pro-normal-italic-normal-mono-*-*-*-*-c-*-iso10646-1"
		  :weight 'normal
		  :slant 'normal
		  :size 14))
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset
   (font-spec :family "Microsoft YaHei"
	      :weight 'normal
	      :slant 'normal
	      :size 12.0)))


(provide 'init-font)

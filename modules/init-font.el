(when is-linux
  (setq en-font-name "DejaVu Sans Mono")
  (setq cn-font-name "YaHei Consolas Hybrid")
  (setq en-font-size 14)
  (setq cn-font-size 14)
  )
(when is-windows
  (setq en-font-name "Liberation Mono")
  (setq cn-font-name "Microsoft YaHei")
  (setq en-font-size 14)
  (setq cn-font-size 11)
  )

(defun eye-update-font-size ()
  ;; English font
  (set-face-attribute
   'default nil
   :font (font-spec :family en-font-name
                    :weight 'normal
                    :slant 'normal
                    :size en-font-size))
  ;; Chinese font
  (if (display-graphic-p)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font
	 (frame-parameter nil 'font)
	 charset
	 (font-spec :family cn-font-name
                    :weight 'normal
                    :slant 'normal
                    :size cn-font-size))))
  )


(defun eye/increase-font-size ()
  "Increase font size of english and chinese."
  (interactive)
  (setq en-font-size (+ en-font-size 1))
  (setq cn-font-size (+ cn-font-size 1))
  (eye-update-font-size)
  )

(defun eye/decrease-font-size ()
  "Decrease font size of english and chinese."
  (interactive)
  (setq en-font-size (- en-font-size 1))
  (setq cn-font-size (- cn-font-size 1))
  (eye-update-font-size)
  )


(eye-update-font-size)

(provide 'init-font)

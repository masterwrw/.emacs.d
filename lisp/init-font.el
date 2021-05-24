;;;; font
(if is-windows (setq cn-font-name "Microsoft YaHei") (setq cn-font-name "Noto Sans CJK SC Regular"))
(setq en-font-name "Source Code Pro")
(setq en-font-size 14 cn-font-size 14)

;; 获取屏幕分辨率自动增大字体
(when (and is-gui
	   (> (x-display-pixel-width) 1366)
	   (> (x-display-pixel-height) 768))
  (setq en-font-size (+ en-font-size 2))
  (setq cn-font-size (+ cn-font-size 2)))


(defun eye-update-font-size ()
  ;; English font
  (set-face-attribute 'default nil :font
		      (font-spec :family en-font-name :weight 'normal :slant 'normal :size en-font-size))
  ;; Chinese font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
		      charset
		      (font-spec :family cn-font-name
				 :weight 'normal
				 :slant 'normal
				 ;;:size cn-font-size
				 ))))

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
  (if (equal (frame-parameter nil 'fullscreen) 'maximize)
      (maximize-frame))
  )

(when is-gui
  (eye-update-font-size)
  ;;(setq face-font-rescale-alist '(("微软雅黑" . 1.2) ("Microsoft Yahei" . 1.2) ("Noto Sans CJK SC Regular" . 1.2)))
  )


(provide 'init-font)

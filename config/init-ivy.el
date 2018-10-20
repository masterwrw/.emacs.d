(require 'swiper)

(defun swiper-dwim ()
  "Search input word or current select string"
  (interactive)
  (if (region-active-p)
	  (let ((str (buffer-substring (region-beginning) (region-end))))
		(pop-mark)
		(swiper str))
	(swiper)))


;; 安装了 smex 后，counsel-M-x 才会按照使用频率排序
(require 'smex)

(require 'counsel)
(defun counsel-ag-dwim ()
  "Search input word or current select string"
  (interactive)
  (if (region-active-p)
	  (let ((str (buffer-substring (region-beginning) (region-end))))
		(pop-mark)
		(counsel-ag str))
	(counsel-ag)))


(let ((command
       (cond
	((executable-find "rg")
	 "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
	((executable-find "ag")
	 "ag -i --noheading --nocolor --nofilename --numbers '%s' %s"))))
  (setq counsel-grep-base-command command))

(require 'ivy)
(setq ivy-initial-inputs-alist nil) ;;不需要自动添加^符号
;; 在当前光标处弹出ivy
;; (setq ivy-completion-beg 0)
;; (setq ivy-display-function 'ivy-display-function-overlay)
(define-key ivy-minibuffer-map (kbd "C-i") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur)


;;; 不想让分割左右窗口后还是在左下角弹出ivy @see https://emacs-china.org/t/topic/5754/9
(setq ivy-count-format "")
(defvar maple/ivy-format-padding nil)

(defun maple/ivy-read-around (-ivy-read &rest args)
  "Advice ivy-read `-IVY-READ` `ARGS`."
  (let ((maple/ivy-format-padding (make-string (window-left-column) ?\s)))
    (setcar args (concat maple/ivy-format-padding (car args)))
    (apply -ivy-read args)))

(advice-add 'ivy-read :around #'maple/ivy-read-around)

(defun maple/ivy-format-function (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat maple/ivy-format-padding (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat maple/ivy-format-padding str))
   cands "\n"))

(setq ivy-format-function 'maple/ivy-format-function)



(require 'find-file-in-project)




(provide 'init-ivy)

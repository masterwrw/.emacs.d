;; 快速复制/剪切/移动其它位置的单词/行
(require 'eno)
(defun eye/eno-copy ()
  (interactive)
  (cond
   ((equal major-mode 'c++-mode)
    (eno-word-copy))
   ((or (equal major-mode 'emacs-lisp-mode) (equal major-mode 'lisp-interaction-mode))
    (eno-symbol-copy))
   (t (eno-word-copy))))


(provide 'init-eno)

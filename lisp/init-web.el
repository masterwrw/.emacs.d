;;;; web
(require 'web-mode)
(defun eye/html-char-to-ltgt()
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (replace-string "<" "&lt;" nil start end)
        (goto-char start)
        (replace-string ">" "&gt;" nil start end)
        )))

(defun eye/html-ltgt-to-char()
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (replace-string "&lt;" "<" nil start end)
        (goto-char start)
        (replace-string "&gt;" ">" nil start end)
        )))


(provide 'init-web)

;; ~/.emacs.d/snippets is yas--default-user-snippets-dir
(require-maybe 'yasnippet)
(require-maybe 'ivy-yasnippet)
(require-maybe 'yankpad)
;;如果手动更换orgmode9后，这句执行后出现Not a face: nil的奇怪问题，终端下ivy无法弹出来，如果是赋值为不带/的字符串，又不会出现问题
(eval-after-load 'yankpad
  (setq yankpad-file (expand-file-name "tecs/yankpad.org" locale-notebook-dir))
  ;; (add-to-list 'hippie-expand-try-functions-list #'yankpad-expand)
  )



(provide 'init-yasnippet)


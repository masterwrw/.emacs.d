
;; 自动保存书签
(add-hook 'kill-emacs-hook
          '(lambda ()
             (bookmark-save)))


(provide 'init-bookmark)

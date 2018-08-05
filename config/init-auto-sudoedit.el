(require-package 'auto-sudoedit)
(require 'auto-sudoedit)

;; Just hook on `find-file-hook', don't hook `dired-mode-hook', it's unnecessary.
(add-hook 'find-file-hook  'auto-sudoedit)

(provide 'init-auto-sudoedit)

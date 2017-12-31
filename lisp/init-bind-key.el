(require-package 'bind-key)

(require 'bind-key)

(bind-key "C-f" 'swiper)
(bind-key "<f2>" 'helm-do-ag-project-root)

(provide 'init-bind-key)

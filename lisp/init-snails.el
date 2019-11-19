;; 使用fuz.el，emacs需要支持动态模块，编译时以--with-modules编译，用(fboundp 'module-load)
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html

(require 'fuz)
(require 'snails)
(require 'snails-backend-rg)

(define-key snails-mode-map (kbd "<down>") #'snails-select-next-item)
(define-key snails-mode-map (kbd "<up>") #'snails-select-prev-item)

(define-key snails-mode-map (kbd "<M-down>") #'snails-select-next-backend)
(define-key snails-mode-map (kbd "<M-up>") #'snails-select-prev-backend)

(global-set-key (kbd "<f4>") 'snails)



(provide 'init-snails)

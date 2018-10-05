;;; Auto generate c++ class implement, function implement, functipn prototype
;;
;; 1.Generate class implement:
;; Move cursor to class name, call srefactor-refactor-at-point,
;; if selecte Other file and cpp file already has content, must open it first,
;; otherwise will be overwritten cpp file.
;;
;; 2.Generate function implement:
;; Move cursor to function name, call srefactor-refactor-at-point, Generate Function Implement
;;
;; 3.Generate function prototype:
;; Move cursor in function, call srefactor-refactor-at-point, Generate Function Prototype
;;
;; 4.Convert function to function pointer
;; Move cursor to function name, call srefactor-refactor-at-point, Generate Function Pointer
;;
;; 5.Extract region to a function:
;; Select a region, call srefactor-refactor-at-point.
;;
;; 6.Rename local variable name:
;; Move cursor on variable name, call srefactor-refactor-at-point
;;

(require 'srefactor)
;; (require 'srefactor-lisp)
;; (setq srefactor-ui-menu-show-help nil)

(add-hook 'c++-mode-hook
		  (lambda ()
			(semantic-mode 1)))

(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)



(provide 'semantic-refactor-init)

;;; C++ programming environment

;; cedet
(require 'semantic) ; code jump, ref...
(require 'ede) ; project manage
;(require 'srecode) ; template system
(require 'cedet)

(require 'semantic/ia)

(add-hook 'c-mode-hook 'semantic-mode)
(add-hook 'c++-mode-hook 'semantic-mode)

;; System header path
(require 'semantic/bovine/gcc nil 'noerror)

(setq qt5-core-dir "d:/Qt/Qt5.8.0/5.8/mingw53_32/include/QtCore")
(setq qt5-gui-dir "d:/Qt/Qt5.8.0/5.8/mingw53_32/include/QtGui")
(setq qt5-network-dir "d:/Qt/Qt5.8.0/5.8/mingw53_32/include/QtNetwork")
(semantic-add-system-include qt5-core-dir 'c++-mode)
(semantic-add-system-include qt5-gui-dir 'c++-mode)
(semantic-add-system-include qt5-network-dir 'c++-mode)
(add-to-list 'auto-mode-alist (cons qt5-core-dir 'c++-mode))
(add-to-list 'auto-mode-alist (cons qt5-gui-dir 'c++-mode))
(add-to-list 'auto-mode-alist (cons qt5-network-dir 'c++-mode))
;; fix void variable error, https://lists.gnu.org/archive/html/emacs-devel/2011-12/msg00683.html
(defvar semantic-lex-c-preprocessor-symbol-file '())
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt5-core-dir "/qconfig.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt5-core-dir "/qglobal.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt5-core-dir "/qsettings.h"))



(provide 'init-cpp-env)

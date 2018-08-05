(use-package qt-pro-mode
  :ensure t
  :mode ("\\.pro\\'" "\\.pri\\'")
  :config
  (add-hook 'qt-pro-mode 'yas-minor-mode))

(use-package qml-mode
  :ensure t
  :init
  (autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
  (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
  )

(use-package company-qml
  :ensure t
  :config
  (add-hook 'qml-mode
		 '(lambda ()
		    (require 'company-qml)
		    (eye/push-to-list 'company-qml company-backends)))
  )



(defun eye/qt5-help ()
  "Find Qt5 document."
  (interactive)
  (let ((url "http://doc.qt.io/qt-5/search-results.html?q="))
    (setq url (concat url (read-string "Query Qt5 document: " (eye/current-word))))
    (browse-url-firefox url)))



(provide 'init-qt)
  

(require-maybe 'qt-pro-mode)
(add-to-list 'auto-mode-alist '("\\.pro$" . qt-pro-mode))
(add-to-list 'auto-mode-alist '("\\.pri$" . qt-pro-mode))

(require-maybe 'css-mode)
(add-to-list 'auto-mode-alist '("\\.qss$" . css-mode))

(require-maybe 'qml-mode)
(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

(defun eye/qt5-help ()
  "Find Qt5 document."
  (interactive)
  (let ((url "http://doc.qt.io/qt-5/search-results.html?q="))
    (setq url (concat url (read-string "Query Qt5 document: " (eye/current-word))))
    (browse-url-firefox url)))



;;(add-hook 'c++-mode-hook 'qt-mode-style-setup)



(provide 'init-qt)

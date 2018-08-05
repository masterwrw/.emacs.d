  (defun config-visit ()
    (interactive)
    (find-file "~/.emacs.d/config.org"))
  (global-set-key (kbd "C-c e") 'config-visit)

  (defun config-reload ()
    "Reloads ~/.emacs.d/config.org at runtime"
    (interactive)
    (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
  (global-set-key (kbd "C-c r") 'config-reload)

  (setq electric-pair-pairs '(
                              (?\{ . ?\})
                              (?\( . ?\))
                              (?\[ . ?\])
                              (?\" . ?\")
                              ))
  (electric-pair-mode t)

;; Show color of #hex format string.
  (use-package rainbow-mode
    :ensure t
    :init
      (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))


(show-paren-mode 1)

(use-package expand-region
  :ensure t
  :bind ("C-q" . er/expand-region)
  :config
  ;; (key-chord-define-global "ee" 'er/expand-region)
  )

(defun eye/indent-region-or-buffer ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-region (point-min) (point-max))
        (message "Indent buffer.")))
    )
  )

(global-set-key (kbd "C-M-\\") 'eye/indent-region-or-buffer)
(define-key prog-mode-map (kbd "C-<tab>") 'eye/indent-region-or-buffer)

  (use-package hungry-delete
    :ensure t
    :config
      (global-hungry-delete-mode))

;; save clipboard contents into kill-ring before replace theme
(setq save-interprogram-paste-before-kill t)

(if (display-graphic-p)
    (progn
      (use-package popup-kill-ring
	:ensure t
	:bind
	("M-y" . popup-kill-ring))))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;; Quick ediff files from dired
;; Mark 2 files in dired, and press "e" into ediff. if only marked one file, then ask second file in prompt.
  (defun ora-ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      (lambda ()
                        (setq ediff-after-quit-hook-internal nil)
                        (set-window-configuration wnd))))
        (error "no more than 2 files should be marked"))))

  (define-key dired-mode-map "e" 'ora-ediff-files)

  (use-package indent-guide
    :ensure t
    :config
    (indent-guide-global-mode))

  (defun xah-comment-dwim ()
    "Like `comment-dwim', but toggle comment if cursor is not at end of line.

  URL `http://ergoemacs.org/emacs/emacs_toggle_comment_by_line.html'
  Version 2016-10-25"
    (interactive)
    (if (region-active-p)
        (comment-dwim nil)
      (let (($lbp (line-beginning-position))
            ($lep (line-end-position)))
        (if (eq $lbp $lep)
            (progn
              (comment-dwim nil))
          (if (eq (point) $lep)
              (progn
                (comment-dwim nil))
            (progn
              (comment-or-uncomment-region $lbp $lep)
              (forward-line )))))))

  (global-set-key (kbd "M-;") 'xah-comment-dwim)

(global-set-key (kbd "C-o") 'dired)
(use-package wdired
  :ensure t)

;; 打开 .dired 后缀文件时，自动进入 dired-virtual-mode 模式。
(require 'dired-x)
(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
                            auto-mode-alist))

;; quick generate virtual.dired file and open it
(defun eye/virtual-dir ()
  "Create and open a virtual directory file.
use command: ls -lR > virtual.dired
"
  (interactive)
  ;; Check ls can use
  (unless (executable-find "ls")
    (error "Unkown command 'ls'"))
  (let (dir path cmd)
    ;; get directory path
    (setq dir (read-directory-name "Directory: "))
    (unless (equal "/" (s-right 1 dir)) ;; check last / charactor
      (setq dir (concat dir "/")))
    (setq path (concat dir "virtual.dired"))
    (setq cmd (concat "ls -lR " dir " > " path))
    (message cmd)
    (when (or (y-or-n-p "Create or update?") (not (file-exists-p path)))
      (setq cmd (read-string "Command:" cmd))
      (eshell-command cmd))
    (if (file-exists-p path)
        (find-file path)
      (message "Can not create virtual.dired file."))))

;; 如果开启了全局 global-auto-revert，则 dired-virtual-mode 模式下经常会弹出提示，所以只在编程模式下开启。
  (add-hook 'prog-mode-hook
            '(lambda ()
               (auto-revert-mode 1)))

(global-set-key (kbd "C-s") 'save-buffer)

(defun eye/sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun eye/delete-file-and-buffer ()
  "Kill the current buffer and delete the file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (delete-file filename)
      (message"Deleted file %s" filename)
      (kill-buffer))))

(require 'nerdtab)
(setq nerdtab-tab-width 30)
(add-to-list 'nerdtab-regex-blacklist "org-src-fontification")
(add-to-list 'nerdtab-regex-blacklist "TAGS")
(global-set-key (kbd "M-0") 'nerdtab-jump-0)
(global-set-key (kbd "M-1") 'nerdtab-jump-1)
(global-set-key (kbd "M-2") 'nerdtab-jump-2)
(global-set-key (kbd "M-3") 'nerdtab-jump-3)
(global-set-key (kbd "M-4") 'nerdtab-jump-4)
(global-set-key (kbd "M-5") 'nerdtab-jump-5)
(global-set-key (kbd "M-6") 'nerdtab-jump-6)
(global-set-key (kbd "M-7") 'nerdtab-jump-7)
(global-set-key (kbd "M-8") 'nerdtab-jump-8)
(global-set-key (kbd "M-9") 'nerdtab-jump-9)
;; (global-set-key (kbd "<M-SPC>") 'nerdtab-jump)

;; 默认的线段不会占满屏幕，设置语言环境为 utf-8 的线段才会占满 (set-language-environment 'utf-8)
;; M-x quoted-insert C-l 输入 ，不能直接输入 l
(use-package page-break-lines
  :ensure t
  :config
  (turn-on-page-break-lines-mode))

(global-subword-mode)

(provide 'init-minor)

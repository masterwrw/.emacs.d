;;; Visual Bookmarks configuration
(require 'bm)

;; (setq bm-in-lifo-order t)		;; 先入先出
(setq bm-cycle-all-buffers t)		;; 在所有buffer中循环
(setq bm-restore-repository-on-load t)

;; where to store persistant files
(setq bm-repository-file "~/.emacs.d/bm-repository")

;; save bookmarks
(setq-default bm-buffer-persistence t)

;; Loading the repository from file when on start up.
(add-hook 'after-init-hook 'bm-repository-load)

;; Saving bookmarks
(add-hook 'kill-buffer-hook #'bm-buffer-save)

;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when Emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook #'(lambda nil
                               (bm-buffer-save-all)
                               (bm-repository-save)))

;; The `after-save-hook' is not necessary to use to achieve persistence,
;; but it makes the bookmark data in repository more in sync with the file
;; state.
(add-hook 'after-save-hook #'bm-buffer-save)

;; Restoring bookmarks
(add-hook 'find-file-hooks   #'bm-buffer-restore)
(add-hook 'after-revert-hook #'bm-buffer-restore)

;; The `after-revert-hook' is not necessary to use to achieve persistence,
;; but it makes the bookmark data in repository more in sync with the file
;; state. This hook might cause trouble when using packages
;; that automatically reverts the buffer (like vc after a check-in).
;; This can easily be avoided if the package provides a hook that is
;; called before the buffer is reverted (like `vc-before-checkin-hook').
;; Then new bookmarks can be saved before the buffer is reverted.
;; Make sure bookmarks is saved before check-in (and revert-buffer)
(add-hook 'vc-before-checkin-hook #'bm-buffer-save)

;; 结合ivy来使用bm-bookmarks
;; @see https://pengpengxp.github.io/emacs/counsel-bm.html
(defun bm-counsel-get-list (bookmark-overlays)
  (-map (lambda (bm)
          (with-current-buffer (overlay-buffer bm)
            (let* ((line (replace-regexp-in-string "\n$" "" (buffer-substring (overlay-start bm)
                                                                              (overlay-end bm))))
                   ;; line numbers start on 1
                   (line-num (+ 1 (count-lines (point-min) (overlay-start bm))))
                   (name (format "%s:%d - %s" (buffer-name) line-num line)))

              `(,name . ,bm))))
        bookmark-overlays))

;; 使即时预览生效
(defun counsel-bm-update-input ()
  "Update fn for counsel-bm."
  (with-ivy-window
    (when (> (length (ivy-state-current ivy-last)) 0)
      (let* ((chosen (ivy-state-current ivy-last))
             (bookmark (gethash chosen bm-hash-table)))
        (if chosen
            (save-restriction
              (with-ivy-window
                (switch-to-buffer (overlay-buffer bookmark))
                (bm-goto bookmark)))
          nil)))))

(defun counsel-bm (&optional initial-input)
  "Use ivy to select bm bookmarks.
It has the ability to preview the bookmarks like `swiper-all'."
  (interactive)
  (let* ((bm-list (bm-counsel-get-list (bm-overlays-lifo-order t)))
         (bm-hash-table (make-hash-table :test 'equal))
         (search-list (-map (lambda (bm) (car bm)) bm-list)))

    (-each bm-list (lambda (bm)
                     (puthash (car bm) (cdr bm) bm-hash-table)))

    (if search-list
        (ivy-read "Find bookmark: "
                  search-list
                  :keymap counsel-describe-map

                  :action (lambda (chosen)
                            (let ((bookmark (gethash chosen bm-hash-table)))
                              (switch-to-buffer (overlay-buffer bookmark))
                              (bm-goto bookmark)))

                  :update-fn #'counsel-bm-update-input

                  :initial-input initial-input
                  :caller 'counsel-bm
                  )
      (message "%s" "No bookmark now."))))

(defun counsel-bm-from-isearch ()
  "Invoke `counsel-bmr' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-exit)
    (counsel-bm query)))


(provide 'init-bm)

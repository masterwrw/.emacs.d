;; Modify from watch-other-window

(defun watch-other-frame-up ()
  (interactive)
  (watch-other-frame-internal "up"))

(defun watch-other-frame-down ()
  (interactive)
  (watch-other-frame-internal "down"))

(defun watch-other-frame-up-line ()
  (interactive)
  (watch-other-frame-internal "up" 1))

(defun watch-other-frame-down-line ()
  (interactive)
  (watch-other-frame-internal "down" 1))

(defun watch-other-frame-internal (direction &optional line)
  (save-excursion
    ;; Switch to other frame.
    (other-frame 1)
    ;; Do scroll operation.
    (ignore-errors
      (if (string-equal direction "up")
          (if line
              (scroll-up line)
            (scroll-up))
        (if line
            (scroll-down line)
          (scroll-down))))
    ;; Switch back to current frame.
    (other-frame 1)
    ))

(provide 'watch-other-frame)


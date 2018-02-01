;;; Basic key bindings

(bind-key "C-o" 'ace-window)
(bind-key "M-x" 'counsel-M-x)

(bind-key "C-<left>" 'backward-forward-previous-location)
(bind-key "C-<right>" 'backward-forward-next-location)

(bind-key "M-<down>" 'my-fast-step-downward)
(bind-key "M-<up>" 'my-fast-step-upward)
(bind-key "M-<right>" 'forward-word)
(bind-key "M-<left>" 'backward-word)

;(bind-key "M-<delete>" 'kill-word)
;(bind-key "M-<backspace>" 'backward-kill-word)


(if *is-windows*
    (bind-key "C-<wheel-up>" 'text-scale-increase)
    (bind-key "C-<mouse-4>" 'text-scale-increase))

(if *is-windows*
    (bind-key "C-<wheel-down>" 'text-scale-decrease)
    (bind-key "C-<mouse-5>" 'text-scale-decrease))

(bind-key "C-=" 'cnfonts-increase-fontsize)
(bind-key "C--" 'cnfonts-decrease-fontsize)

(if *is-windows*
    (bind-key "<wheel-up>" 'scroll-down-lines)
    (bind-key "<mouse-4>" 'scroll-down-lines))
(if *is-windows*
    (bind-key "<wheel-down>" 'scroll-up-lines)
    (bind-key "<mouse-5>" 'scroll-up-lines))


(provide 'init-key-binding)

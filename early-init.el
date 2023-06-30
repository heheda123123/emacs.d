(setq inhibit-startup-message t)
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)))
(defun xiaozhu/reset-frame-size (&optional frame)
    (interactive)
    (when frame
      (select-frame frame))
    (set-frame-width (selected-frame) 200)
    (set-frame-height (selected-frame) 50))
(xiaozhu/reset-frame-size)
(add-hook 'after-make-frame-functions 'xiaozhu/reset-frame-size)

(global-display-line-numbers-mode)

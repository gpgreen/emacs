;;; init-font-switching.el --- Switch fonts in a buffer

;; Copyright (C) 2018 Greg Green

;; Author: Greg Green <gpgreen@bit-builder.com>
;; Version: 0.1

(defvar font-switching-list nil "A list of fonts for `font-switching-cycle-font' to cycle from.")
(setq font-switching-list
      (cond
       ((string-equal system-type "windows-nt")
        '(
          "Courier-10"
          "Lucida Console-10"
          "Segoe UI Symbol-12"
          "Lucida Sans Unicode-10"
          ))
       ((string-equal system-type "gnu/linux")
        '(
          "Ubuntu Mono"
	  "Source Code Pro Medium"
	  "DejaVu Sans Mono"
          "Garuda"
          ))
       ((string-equal system-type "darwin") ; Mac
        '("Courier-14"
          "Menlo-14"))))

;; switch fonts from list font-switching-list
(defun font-switching-cycle-font (@n)
  "Change font in current frame.
Each time this is called, font cycles thru a predefined list of fonts in the variable `font-switching-list' .
If @n is 1, cycle forward.
If @n is -1, cycle backward.
See also `font-switching-cycle-font-next', `font-switching-cycle-font-previous'.

URL `http://ergoemacs.org/emacs/emacs_switching_fonts.html'
Version 2015-09-21"
  (interactive "p")
  ;; this function sets a property “state”. It is a integer. Possible values are any index to the fontList.
  (let ($fontToUse $stateBefore $stateAfter )
    (setq $stateBefore (if (get 'font-switching-cycle-font 'state) (get 'font-switching-cycle-font 'state) 0))
    (setq $stateAfter (% (+ $stateBefore (length font-switching-list) @n) (length font-switching-list)))
    (setq $fontToUse (nth $stateAfter font-switching-list))
    (set-frame-font $fontToUse t)
    ;; (set-frame-parameter nil 'font $fontToUse)
    (message "Current font is: %s" $fontToUse )
    (put 'font-switching-cycle-font 'state $stateAfter)))

;; to next font
(defun font-switching-cycle-font-next ()
  "Switch to the next font, in current window.
See `font-switching-cycle-font'."
  (interactive)
  (font-switching-cycle-font 1))

;; to previous font
(defun font-switching-cycle-font-previous ()
  "Switch to the previous font, in current window.
See `font-switching-cycle-font'."
  (interactive)
  (font-switching-cycle-font -1))

;; now provide some keys
(global-set-key (kbd "C-x <f8>") 'font-switching-cycle-font-next)
(global-set-key (kbd "C-x <f7>") 'font-switching-cycle-font-previous)

(provide 'init-font-switching)

;;; init-font-switching.el ends here

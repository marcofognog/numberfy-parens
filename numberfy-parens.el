(defvar numberfy-parens-positions)

(defun numberfy-parens-buffer ()
  (interactive)
  (progn
    (setq content (buffer-substring-no-properties (point-min) (point-max)))
    (setq new-content (nth 0 (numberfy-parens-replace content)))
    (setq numberfy-parens-positions (nth 1 (numberfy-parens-replace content)))
    (setq saved-pos (point))
    (erase-buffer)
    (insert new-content)
    (numberfy-parens-highlight)
    (goto-char saved-pos)
    )
  )

(defun numberfy-parens-defun ()
  (interactive)
  (if (numberfy-parens-in-defun)
      (save-restriction
        (widen)
        (narrow-to-defun)
        (numberfy-parens-buffer))
    (numberfy-parens-buffer))
  )

(defun numberfy-parens-in-defun ()
  (bounds-of-thing-at-point 'defun))

(defun numberfy-parens-replace(original)
  (progn
    (setq new-list (list))
    (setq positions (list))
    (setq char-list (split-string original ""))
    (setq current-level -1)
    (loop for i from 0 to (length char-list) collect
          (progn
            (setq current-char(nth (+ i 1) char-list))
            (if (string-equal current-char "(")
                (progn
                  (setq current-level (+ current-level 1))
                  (setq to-be-added (substring (number-to-string current-level) -1 nil))
                  (setq positions (cons i positions))
                  )
              (if (string-equal current-char ")")
                  (progn
                    (setq to-be-added (substring (number-to-string current-level) -1 nil))
                    (setq current-level (- current-level 1))
                    (setq positions (cons i positions))
                    )
                (setq to-be-added current-char)
                )
              )
            (setq new-list (cons to-be-added new-list))
            ))
    (list (mapconcat 'identity (reverse new-list) "") positions)))

(defun numberfy-parens-highlight ()
  (setq content (buffer-substring-no-properties (point-min) (point-max)))
  (setq char-list (split-string content ""))
  (loop for pos in numberfy-parens-positions
        collect
        (progn
        (setq number (string-to-number (nth (+ pos 1) char-list)))
        (numberfy-parens-highlight-char (+ pos 1) number))
        )
  )

(defun numberfy-parens-highlight-char(position number)
  (when (or (equal number 0) (equal number 5))
    (put-text-property position (+ position 1) 'font-lock-face '(:foreground "yellow") (current-buffer))
    )
  (when (or (equal number 1) (equal number 6))
    (put-text-property position (+ position 1) 'font-lock-face '(:foreground "red") (current-buffer))
    )
  (when (or (equal number 2) (equal number 7))
    (put-text-property position (+ position 1) 'font-lock-face '(:foreground "green") (current-buffer))
    )
  (when (or (equal number 3) (equal number 8))
    (put-text-property position (+ position 1) 'font-lock-face '(:foreground "lightblue") (current-buffer))
    )
  (when (or (equal number 4) (equal number 9))
    (put-text-property position (+ position 1) 'font-lock-face '(:foreground "white") (current-buffer))
    )
  )

(provide 'numberfy-parens)

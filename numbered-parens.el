(defun numbered-parens-convert (original)
  (progn
    (setq new-list (list))
    (setq char-list (split-string original ""))
    (loop for char in char-list collect
          (progn
            (if (string-equal char "(")
                (setq to-be-added "0")
              (if (string-equal char ")")
                  (setq to-be-added "0")
                (setq to-be-added char)
                )
              )
            (setq new-list (cons to-be-added new-list))
            ))
    (mapconcat 'identity (reverse new-list) "")))


(ert-deftest numbered-parens-convert-test ()
  (setq original "(+ 1 2)")
  (setq expected "0+ 1 20")
  (should (string-equal expected (numbered-parens-convert original )))
  )

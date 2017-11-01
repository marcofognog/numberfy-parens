(defun numbered-parens-convert (original)
  (progn
    (setq new-list (list))
    (setq char-list (split-string original ""))
    (setq current-level -1)
    (loop for char in char-list collect
          (progn
            (if (string-equal char "(")
                (progn
                  (setq current-level (+ current-level 1))
                  (setq to-be-added (number-to-string current-level))
                  )
              (if (string-equal char ")")
                  (progn
                    (setq to-be-added (number-to-string current-level))
                    (setq current-level (- current-level 1))
                    )
                (setq to-be-added char)
                )
              )
            (setq new-list (cons to-be-added new-list))
            ))
    (mapconcat 'identity (reverse new-list) "")))


(ert-deftest numbered-parens-convert-0-test ()
  (setq original "(+ 1 2)")
  (setq expected "0+ 1 20")
  (should (string-equal expected (numbered-parens-convert original )))
  )

(ert-deftest numbered-parens-convert-1-test ()
  (setq original "(= (+ 1 2) 5)")
  (setq expected "0= 1+ 1 21 50")
  (should (string-equal expected (numbered-parens-convert original )))
  )

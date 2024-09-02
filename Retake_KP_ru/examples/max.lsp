(let ((max (lambda (a b)
              (if (eq (sub a b) (abs (sub a b)))
                  a
                  b))))
  (max 8 12))

(let ((fact (lambda (n)
               (if (eq n 0)
                   1
                   (mul n (fact (sub n 1)))))))
  (fact 4))

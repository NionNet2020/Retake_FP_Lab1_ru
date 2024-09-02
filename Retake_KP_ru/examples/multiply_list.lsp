(let ((map (lambda (f lst)
              (if (eq (length lst) 0)
                  lst
                  (cons (f (car lst)) (map f (cdr lst)))))))
      (mul_by_k (lambda (x) (mul x 3)))
      (lst '(1 2 3 4 5)))
  (map mul_by_k lst))

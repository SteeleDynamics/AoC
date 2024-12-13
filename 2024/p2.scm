;; read-datum procedure
(define (read-datum p)
  (let ((res (get-datum p)))
    (if (eof-object? res)
        '()
        (cons res (read-datum p)))))

;; read-line procedure
(define (read-line p)
  (let ((res (get-line p)))
    (if (eof-object? res)
        res
        (read-datum (open-string-input-port res)))))

;; read-file procedure
(define (read-file p)
  (let ((res (read-line p)))
    (if (eof-object? res)
        '()
        (cons res (read-file p)))))

;; read-input procedure
(define (read-input filename)
  (let* ((p (open-input-file filename))
         (res (read-file p)))
    (close-input-port p)
    res))

;; input-col procedure
(define (input-col input k)
  (fold-right (lambda (elt acc) (cons (list-ref elt k) acc)) '() input))

;; solving p2
(define p2-input (read-input "p2-input.txt"))
(define l (input-col p2-input 0))
(define r (input-col p2-input 1))
(define p2-ans (fold-left + 0 (map (lambda (x) (* x (length (filter (lambda (y) (eq? y x)) r)))) l)))
p2-ans

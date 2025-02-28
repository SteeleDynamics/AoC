;; read-file procedure
(define (read-file p)
  (let ((res (get-line p)))
    (if (eof-object? res)
        '()
        (cons res (read-file p)))))

;; read-input procedure
(define (read-input filename)
  (let* ((p (open-input-file filename))
         (res (read-file p)))
    (close-input-port p)
    res))

;; char->digit procedure
(define (char->digit c) (- (char->integer c) (char->integer #\0)))

;; last? predicate
(define (last? lis) (null? (cdr lis)))

;; last selector
(define (last lis)
  (cond ((null? lis) (error #f "Empty list -- LAST"))
        ((last? lis) (car lis))
        (else (last (cdr lis)))))

;; next-to-last? predicate
(define (next-to-last? lis) (null? (cddr lis)))

;; next-to-last selector
(define (next-to-last lis)
  (cond ((null? lis) (error #f "Empty list -- NEXT-TO-LAST"))
        ((next-to-last? lis) (car lis))
        (else (next-to-last (cdr lis)))))

;; snoc procedure
(define (snoc d a) (append d (list a)))

;; uniq procedure
(define (uniq x acc)
  (cond ((null? x) acc)
        ((member (car x) acc) (uniq (cdr x) acc))
        (else (uniq (cdr x) (snoc acc (car x))))))

;; make-strides procedure
(define (make-strides dims)
  (if (last? dims)
      '(1)
      (cons (fold-left * 1 (cdr dims))
            (make-strides (cdr dims)))))

;; make-array constructor
(define (make-array dims)
  (list dims
        (make-strides dims)
        (make-vector (fold-left * 1 dims))))

;; vector->array procedure
(define (vector->array vec dims)
  (list dims
        (make-strides dims)
        (vector-copy vec)))

;; array-dims selector
(define (array-dims arr) (car arr))

;; array-strides selector
(define (array-strides arr) (cadr arr))

;; array-vector selector
(define (array-vector arr) (caddr arr))

;; array-copy procedure
(define (array-copy arr)
  (list (list-copy (array-dims arr))
        (list-copy (array-strides arr))
        (vector-copy (array-vector arr))))

;; inner-prod procedure
(define (inner-prod x y acc)
  (if (or (null? x) (null? y))
      acc
      (inner-prod (cdr x) (cdr y) (+ (* (car x) (car y)) acc))))

;; array-ref procedure
(define (array-ref arr ind)
  (vector-ref (array-vector arr)
              (inner-prod (array-strides arr) ind 0)))

;; array-set! procedure
(define (array-set! arr ind val)
  (vector-set! (array-vector arr)
               (inner-prod (array-strides arr) ind 0)
               val))

;; ind-incr-cmb procedure
(define (ind-incr-cmb coord dim acc)
  (let ((carry (if (car acc) 1 0))
        (sum (cdr acc)))
    (if (>= (+ coord carry) dim)
        (cons #t (cons 0 sum))
        (cons #f (cons (+ coord carry) sum)))))

;; ind-incr procedure
(define (ind-incr ind dims)
  (let ((res (fold-right ind-incr-cmb '(#t) ind dims)))
    (if (car res)
        #f
        (cdr res))))

;; ind-decr-cmb procedure
(define (ind-decr-cmb coord dim acc)
  (let ((borrow (if (car acc) 1 0))
        (diff (cdr acc)))
    (if (negative? (- coord borrow))
        (cons #t (cons (- dim 1) diff))
        (cons #f (cons (- coord borrow) diff)))))

;; ind-decr procedure
(define (ind-decr ind dims)
  (let ((res (fold-right ind-decr-cmb '(#t) ind dims)))
    (if (car res)
        #f
        (cdr res))))

;; array-member-iter procedure
(define (array-member-iter obj arr ind)
    (cond ((and ind (equal? (array-ref arr ind) obj)) ind)
          (ind (array-member-iter obj arr (ind-incr ind (array-dims arr))))
          (else #f)))

;; array-member procedure
(define (array-member obj arr)
  (array-member-iter obj arr (map (lambda (x) 0) (array-dims arr))))

;; ind-valid procedure
(define (ind-valid ind dims)
  (let* ((nat (map (lambda (x) (>= x 0)) ind))
         (less-than (map (lambda (x y) (< x y)) ind dims))
         (within (map (lambda (p1 p2) (and p1 p2)) nat less-than))
         (valid (fold-left (lambda (acc elt) (and acc elt)) #t within)))
    (if valid ind #f)))

;; TODO: rewrite using ind-incr procedure
;; subvector-disp procedure
(define (subvector-disp vec beg end w)
  (do ((i beg (+ i 1)))
      ((>= i end))
    (display (format "~vd" w (vector-ref vec i))))
  (newline))

;; TODO: rewrite using ind-incr procedure
;; vector-disp-iter procedure
(define (vector-disp-iter vec len pos m n w)
  (cond ((zero? m)
         (subvector-disp vec pos n w))
        ((zero? pos)
         (subvector-disp vec pos (+ pos n) w)
         (vector-disp-iter vec len (+ pos n) m n w))
        (else
         (cond ((= pos (- len n))
                (subvector-disp vec pos (+ pos n) w))
               ((zero? (remainder (+ pos n) (* m n)))
                (subvector-disp vec pos (+ pos n) w)
                (newline)
                (vector-disp-iter vec len (+ pos n) m n w))
               (else
                (subvector-disp vec pos (+ pos n) w)
                (vector-disp-iter vec len (+ pos n) m n w))))))

;; TODO: rewrite using ind-incr procedure
;; array-disp procedure
(define (array-disp arr w)
  (let* ((vec (array-vector arr))
         (dims (array-dims arr))
         (len (vector-length vec))
         (rank (length dims))
         (m (if (> rank 1) (next-to-last dims) 0))
         (n (last dims)))
    (vector-disp-iter vec len 0 m n w)))

;;; problem-specific procedures

;; adj-list procedure
(define (adj-list arr ind)
  (let ((i (car ind))
        (j (cadr ind))
        (pred (lambda (ind) (ind-valid ind (array-dims arr))))
        (v (array-ref arr ind)))
    (let ((down (list (+ i 1) j))
          (incr? (lambda (ind) (and ind (= (+ v 1) (array-ref arr ind)))))
          (left (list i (- j 1)))
          (right (list i (+ j 1)))
          (up (list (- i 1) j)))
      (filter incr? (map pred (list right up left down))))))

;; find-one procedure (using CPS)
(define (find-one p? a xs sc fc)
  (cond ((null? xs) (fc))
        ((p? a (car xs)) (sc (car xs)))
        (else (find-one p? a (adj-list a (car xs)) sc
                        (lambda () (find-one p? a (cdr xs) sc fc))))))

;; dfs procedure (using CPS)
(define (dfs p? a xs k)
  (let* ((neq? (lambda (x y) (not (equal? x y))))
         (q? (lambda (y) (lambda (a x) (and (p? a x) (neq? x y)))))
         (sc (lambda (x) (dfs (q? x) a xs (lambda (r) (k (cons x r))))))
         (fc (lambda () (k '()))))
    (find-one p? a xs sc fc)))

;; CPS predicate and continuation procedures 
(define p? (lambda (a x) (= (array-ref a x) 9)))
(define sc (lambda (x) x))
(define fc (lambda () #f))
(define k (lambda (x) x))

;; example
(define ex-input (read-input "p19-ex.txt"))
(define ex-dims (list (length ex-input) (string-length (car ex-input))))
(define ex-chars
  (fold-left (lambda (acc elt) (append acc (string->list elt))) '() ex-input))
(define ex-list (map char->digit ex-chars))
(define ex-arr (vector->array (list->vector ex-list) ex-dims))

;; solving p19
(define p19-input (read-input "p19-input.txt"))
(define p19-dims (list (length p19-input) (string-length (car p19-input))))
(define p19-chars
  (fold-left (lambda (acc elt) (append acc (string->list elt))) '() p19-input))
(define p19-list (map char->digit p19-chars))
(define p19-arr (vector->array (list->vector p19-list) p19-dims))

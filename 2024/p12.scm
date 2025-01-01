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

;; snoc procedure
(define (snoc d a)
  (append d (list a)))

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

;; subvector-disp procedure
(define (subvector-disp vec beg end w)
  (do ((i beg (+ i 1)))
      ((>= i end))
    (display (format "~vd" w (vector-ref vec i))))
  (newline))

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

;; array-disp procedure
(define (array-disp arr w)
  (let* ((vec (array-vector arr))
         (dims (array-dims arr))
         (len (vector-length vec))
         (rank (length dims))
         (m (if (> rank 1) (next-to-last dims) 0))
         (n (last dims)))
    (vector-disp-iter vec len 0 m n w)))

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

;;; problem-specific procedures

;; next-ind procedure
(define (next-ind ind dir)
  (let ((i (car ind))
        (j (cadr ind)))
    (cond ((eq? dir 'E) (list i (+ j 1)))
          ((eq? dir 'N) (list (- i 1) j))
          ((eq? dir 'W) (list i (- j 1)))
          (else (list (+ i 1) j)))))

;; turn-right procedure
(define (turn-right dir)
  (cond ((eq? dir 'E) 'S)
        ((eq? dir 'N) 'E)
        ((eq? dir 'W) 'N)
        (else 'W)))

;; walk procedure
(define (walk arr ind dir acc)
  (let ((next (ind-valid (next-ind ind dir) (array-dims arr))))
    (cond ((and next (eq? (array-ref arr next) #\#))
           (walk arr ind (turn-right dir) acc))
          (next
           (walk arr next dir (snoc acc (cons next dir))))
          (else acc))))

;; uniq procedure
(define (uniq x acc)
  (cond ((null? x) acc)
        ((member (car x) acc) (uniq (cdr x) acc))
        (else (uniq (cdr x) (snoc acc (car x))))))

;; walk-cycle? predicate procedure
(define (walk-cycle? arr ind dir acc)
  (let ((next (ind-valid (next-ind ind dir) (array-dims arr))))
    (cond ((and next (member (cons next dir) acc)) #t)
          ((and next (or (eq? (array-ref arr next) #\#)
                         (eq? (array-ref arr next) #\O)))
           (walk-cycle? arr ind (turn-right dir) acc))
          (next
           (walk-cycle? arr next dir (snoc acc (cons next dir))))
          (else #f))))

;; obst-cycle? predicate procedure
(define (obst-cycle? arr0 ind0 dir0 acc0)
  (lambda (ind)
    (let* ((arr1 (array-copy arr0))
           (t0 (array-set! arr1 ind #\O)))
      (walk-cycle? arr1 ind0 dir0 acc0))))

;; example
(define ex-input (read-input "p12-ex.txt"))
(define ex-dims (list (length ex-input) (string-length (car ex-input))))
(define ex-list
  (fold-left (lambda (acc elt) (append acc (string->list elt))) '() ex-input))
(define ex-arr (vector->array (list->vector ex-list) ex-dims))
(define ex-ind0 (array-member #\^ ex-arr))
(define ex-acc0 (list (cons ex-ind0 'N)))
(define ex-uniq (cdr (uniq (map car (walk ex-arr ex-ind0 'N ex-acc0)) '())))
(define ex-cycl (map (obst-cycle? ex-arr ex-ind0 'N ex-acc0) ex-uniq))
(define ex-ans
  (fold-left (lambda (acc e1 e2) (if e1 (snoc acc e2) acc)) '() ex-cycl ex-uniq))
(begin (length ex-ans))

;; solving p12
(define p12-input (read-input "p12-input.txt"))
(define p12-dims (list (length p12-input) (string-length (car p12-input))))
(define p12-list
  (fold-left (lambda (acc elt) (append acc (string->list elt))) '() p12-input))
(define p12-arr (vector->array (list->vector p12-list) p12-dims))
(define p12-ind0 (array-member #\^ p12-arr))
(define p12-acc0 (list (cons p12-ind0 'N)))
(define p12-uniq (cdr (uniq (map car (walk p12-arr p12-ind0 'N p12-acc0)) '())))
(define p12-cycl (map (obst-cycle? p12-arr p12-ind0 'N p12-acc0) p12-uniq))
(define p12-ans
  (fold-left (lambda (acc e1 e2) (if e1 (snoc acc e2) acc)) '() p12-cycl p12-uniq))
(begin (length p12-ans))

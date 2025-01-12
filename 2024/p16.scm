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

;; ind-add procedure
(define (ind-add ind1 ind2)
  (let ((d1 (length ind1))
        (d2 (length ind2)))
    (if (zero? (- d2 d1))
        (map + ind1 ind2)
        (error #f "dimension mismatch -- IND-ADD"))))

;; ind-sub procedure
(define (ind-sub ind1 ind2)
  (let ((d1 (length ind1))
        (d2 (length ind2)))
    (if (zero? (- d2 d1))
        (map - ind1 ind2)
        (error #f "dimension mismatch -- IND-SUB"))))

;; ind-valid procedure
(define (ind-valid ind dims)
  (let* ((nat (map (lambda (x) (>= x 0)) ind))
         (less-than (map (lambda (x y) (< x y)) ind dims))
         (within (map (lambda (p1 p2) (and p1 p2)) nat less-than))
         (valid (fold-left (lambda (acc elt) (and acc elt)) #t within)))
    (if valid ind #f)))

;; snoc procedure
(define (snoc d a) (append d (list a)))

;; uniq procedure
(define (uniq x acc)
  (cond ((null? x) acc)
        ((member (car x) acc) (uniq (cdr x) acc))
        (else (uniq (cdr x) (snoc acc (car x))))))

;;; problem-specific procedures

;; tbl-snoc! procedure
(define (tbl-snoc! tbl key value)
  (hashtable-set! tbl key (snoc (hashtable-ref tbl key '()) value)))

;; pop-tbl! procedure
(define (pop-tbl! tbl arr ind)
  (cond ((and ind (eq? (array-ref arr ind) #\.))
         (pop-tbl! tbl arr (ind-incr ind (array-dims arr))))
        (ind
         (tbl-snoc! tbl (array-ref arr ind) ind)
         (pop-tbl! tbl arr (ind-incr ind (array-dims arr))))
        (else
         (void))))

;; cmb-add-iter procedure
(define (cmb-add-iter ind diff dims acc)
  (if ind
      (cmb-add-iter (ind-valid (ind-add ind diff) dims)
                    diff dims (snoc acc ind))
      acc))

;; cmb-sub-iter procedure
(define (cmb-sub-iter ind diff dims acc)
  (if ind
      (cmb-sub-iter (ind-valid (ind-sub ind diff) dims)
                    diff dims (snoc acc ind))
      acc))

(define (make-nodes-cmb ind1 dims)
  (lambda (acc ind2)
    (let* ((diff (ind-sub ind2 ind1))
           (node1 (ind-valid (ind-add ind2 diff) dims))
           (node2 (ind-valid (ind-sub ind1 diff) dims))
           (nodes1 (cmb-add-iter node1 diff dims '()))
           (nodes2 (cmb-sub-iter node2 diff dims '())))
      (append acc nodes1 nodes2 (list ind1 ind2)))))

;; make-nodes constructor procedure
(define (make-nodes ants dims)
  (cond ((<= (length ants) 1) '())
        (else
         (let* ((ind1 (car ants))
                (nodes-cmb (make-nodes-cmb ind1 dims)))
           (append (fold-left nodes-cmb '() (cdr ants))
                 (make-nodes (cdr ants) dims))))))

;; example
(define ex-input (read-input "p16-ex.txt"))
(define ex-dims (list (length ex-input) (string-length (car ex-input))))
(define ex-list
  (fold-left (lambda (acc elt) (append acc (string->list elt))) '() ex-input))
(define ex-arr (vector->array (list->vector ex-list) ex-dims))
(define ex-tbl (make-eq-hashtable))
(pop-tbl! ex-tbl ex-arr '(0 0))
(define-values (ex-keys-vec ex-vals-vec) (hashtable-entries ex-tbl))
(define ex-keys (vector->list ex-keys-vec))
(define ex-vals (vector->list ex-vals-vec))
(define ex-tmp0 (map (lambda (x) (make-nodes x ex-dims)) ex-vals))
(define ex-tmp1 (fold-left append '() ex-tmp0))
(define ex-tmp2 (filter (lambda (x) x) ex-tmp1))
(define ex-uniq (uniq ex-tmp2 '()))
(begin (length ex-uniq))

;; solving p16
(define p16-input (read-input "p16-input.txt"))
(define p16-dims (list (length p16-input) (string-length (car p16-input))))
(define p16-list
  (fold-left (lambda (acc elt) (append acc (string->list elt))) '() p16-input))
(define p16-arr (vector->array (list->vector p16-list) p16-dims))
(define p16-tbl (make-eq-hashtable))
(pop-tbl! p16-tbl p16-arr '(0 0))
(define-values (p16-keys-vec p16-vals-vec) (hashtable-entries p16-tbl))
(define p16-keys (vector->list p16-keys-vec))
(define p16-vals (vector->list p16-vals-vec))
(define p16-tmp0 (map (lambda (x) (make-nodes x p16-dims)) p16-vals))
(define p16-tmp1 (fold-left append '() p16-tmp0))
(define p16-tmp2 (filter (lambda (x) x) p16-tmp1))
(define p16-uniq (uniq p16-tmp2 '()))
(begin (length p16-uniq))

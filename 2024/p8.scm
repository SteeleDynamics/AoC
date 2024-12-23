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

;; x-mas-cfgs list (ccw starting at top-right)
(define x-mas-cfgs '((#\M #\M #\S #\S) (#\S #\M #\M #\S)
                     (#\S #\S #\M #\M) (#\M #\S #\S #\M)))

;; x-mas-inds procedure (ccw starting at top-right)
(define (x-mas-inds ind)
  (list (list (- (car ind) 1) (+ (cadr ind) 1))
        (list (- (car ind) 1) (- (cadr ind) 1))
        (list (+ (car ind) 1) (- (cadr ind) 1))
        (list (+ (car ind) 1) (+ (cadr ind) 1))))

;; x-mas-word procedure
(define (x-mas-word ind corners)
  (append (list (list ind #\A))
          (map list (x-mas-inds ind) corners)))

;; ind-corners procedure
(define (ind-corners arr ind)
  (map (lambda (elt) (array-ref arr elt)) (x-mas-inds ind)))

;; x-mas-search procedure
(define (x-mas-search arr)
  (do ((res '())
       (i 1 (+ i 1)))
      ((>= i (- (car (array-dims arr)) 1)) res)
    (do ((j 1 (+ j 1)))
        ((>= j (- (cadr (array-dims arr)) 1)) res)
      (let* ((ind (list i j))
             (corners (ind-corners arr ind)))
        (if (and (eq? #\A (array-ref arr ind))
                 (pair? (member corners x-mas-cfgs)))
            (set! res (append res (list (x-mas-word ind corners)))))))))

;; x-mas-mark! mutator procedure
(define (x-mas-mark! arr words)
  (fold-left
   (lambda (acc word)
     (fold-left
      (lambda (acc elt) (array-set! arr (car elt) (cadr elt)))
      (void)
      word
      ))
   (void)
   words))

;; example
(define ex-input "MMMSXXMASMMSAMXMSMSAAMXSXMAAMMMSAMASMSMXXMASAMXAMMXXAMMXXAMASMSMSASXSSSAXAMASAAAMAMMMXMMMMMXMXAXMASX")
(define ex-ans ".M.S........A..MSMS..M.S.MAA....A.ASMSM..M.S.M..............S.S.S.S.S..A.A.A.A..M.M.M.M.M...........")
(define ex-arr0 (vector->array (list->vector (string->list ex-input)) '(10 10)))
(define ex-arr1 (vector->array (make-vector 100 #\.) '(10 10)))
(define ex-words (x-mas-search ex-arr0))
(x-mas-mark! ex-arr1 ex-words)
(define ex-res (list->string (vector->list (array-vector ex-arr1))))
(string=? ex-res ex-ans)

;; solving p8
(define p8-input (read-input "p8-input.txt"))
(define p8-dims (list (length p8-input) (string-length (car p8-input))))
(define p8-list
  (fold-left (lambda (acc elt) (append acc (string->list elt))) '() p8-input))
(define p8-arr0 (vector->array (list->vector p8-list) p8-dims))
(define p8-arr1
  (vector->array (make-vector (* (car p8-dims) (cadr p8-dims)) #\.) p8-dims))
(define p8-words (x-mas-search p8-arr0))
(x-mas-mark! p8-arr1 p8-words)
(length p8-words)

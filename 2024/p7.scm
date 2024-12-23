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

;; card-dirs list
(define card-dirs '(E NE N NW W SW S SE))

;; xmas-chars list
(define xmas-chars (list #\X #\M #\A #\S))

;; card-dir-delta-i procedure
(define (card-dir-delta-i dir)
  (cond ((pair? (memq dir '(NE N NW))) '(0 -1 -2 -3))
        ((pair? (memq dir '(SW S SE))) '(0 1 2 3))
        (else '(0 0 0 0))))

;; card-dir-delta-j procedure
(define (card-dir-delta-j dir)
  (cond ((pair? (memq dir '(E NE SE))) '(0 1 2 3))
        ((pair? (memq dir '(NW W SW))) '(0 -1 -2 -3))
        (else '(0 0 0 0))))

;; card-dir-inds procedure
(define (card-dir-inds ind dir)
  (let* ((i (car ind))
         (j (cadr ind))
         (i0 (list i i i i))
         (j0 (list j j j j))
         (di (card-dir-delta-i dir))
         (dj (card-dir-delta-j dir)))
    (map (lambda (e1 e2) (list e1 e2)) (map + i0 di) (map + j0 dj))))

;; valid-dir? predicate procedure
(define (valid-dir? arr ind dir)
  (let ((i (car ind))
        (j (cadr ind))
        (rows (car (array-dims arr)))
        (cols (cadr (array-dims arr))))
    (cond ((eq? dir 'E) (< j (- cols 3)))
          ((eq? dir 'NE) (and (> i 2) (< j (- cols 3))))
          ((eq? dir 'N) (> i 2))
          ((eq? dir 'NW) (and (> i 2) (> j 2)))
          ((eq? dir 'W) (> j 2))
          ((eq? dir 'SW) (and (< i (- rows 3)) (> j 2)))
          ((eq? dir 'S) (< i (- rows 3)))
          (else (and (< i (- rows 3)) (< j (- cols 3)))))))

;; xmas? predicate procedure
(define (xmas? arr inds dir)
  (equal? xmas-chars (map (lambda (x) (array-ref arr x)) inds)))

;; xmas-search procedure
(define (xmas-search arr)
  (do ((res '())
       (i 0 (+ i 1)))
      ((>= i (car (array-dims arr))) res)
    (do ((j 0 (+ j 1)))
        ((>= j (cadr (array-dims arr))) res)
      (if (eq? #\X (array-ref arr (list i j)))
          (do ((k 0 (+ k 1)))
              ((>= k (length card-dirs)) res)
            (let* ((ind (list i j))
                   (dir (list-ref card-dirs k))
                   (inds (card-dir-inds ind dir)))
              (if (and (valid-dir? arr ind dir)
                       (xmas? arr inds dir))
                  (set! res (append res (list inds))))))))))

;; xmas-mark! mutator procedure
(define (xmas-mark! arr words)
  (fold-left
   (lambda (acc word)
     (fold-left
      (lambda (acc ind c) (array-set! arr ind c))
      (void)
      word
      xmas-chars))
   (void)
   words))

;; example
(define ex-input "MMMSXXMASMMSAMXMSMSAAMXSXMAAMMMSAMASMSMXXMASAMXAMMXXAMMXXAMASMSMSASXSSSAXAMASAAAMAMMMXMMMMMXMXAXMASX")
(define ex-ans "....XXMAS..SAMXMS......S..A.....A.A.MS.XXMASAMX.MMX.....XA.AS.S.S.S.SS.A.A.A.A.A..M.M.M.MM.X.X.XMASX")
(define ex-arr0 (vector->array (list->vector (string->list ex-input)) '(10 10)))
(define ex-arr1 (vector->array (make-vector 100 #\.) '(10 10)))
(define ex-words (xmas-search ex-arr0))
(xmas-mark! ex-arr1 ex-words)
(define ex-res (list->string (vector->list (array-vector ex-arr1))))
(string=? ex-res ex-ans)

;; solving p7
(define p7-input (read-input "p7-input.txt"))
(define p7-dims (list (length p7-input) (string-length (car p7-input))))
(define p7-list
  (fold-left (lambda (acc elt) (append acc (string->list elt))) '() p7-input))
(define p7-arr0 (vector->array (list->vector p7-list) p7-dims))
(define p7-arr1
  (vector->array (make-vector (* (car p7-dims) (cadr p7-dims)) #\.) p7-dims))
(define p7-words (xmas-search p7-arr0))
(xmas-mark! p7-arr1 p7-words)
(length p7-words)

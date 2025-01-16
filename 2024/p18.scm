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

;; subvector-fill! procedure
(define (subvector-fill! vec beg end obj)
  (cond ((< beg end)
         (vector-set! vec beg obj)
         (subvector-fill! vec (+ beg 1) end obj))
        (else (void))))

;;; problem-specific procedures

;; populate! procedure
(define (populate! vec input k id)
  (cond ((null? (cdr input))
         (subvector-fill! vec k (+ k (car input)) id))
        (else
         (subvector-fill! vec k (+ k (car input)) id)
         (populate! vec
                   (cddr input)
                   (+ k (car input) (cadr input))
                   (+ id 1)))))

;; next-free procedure
(define (next-free vec beg end)
  (cond ((>= beg (vector-length vec))
         '())
        ((vector-ref vec beg)
         (next-free vec (+ beg 1) (+ beg 1)))
        ((>= end (vector-length vec))
         (cons beg (- end 1)))
        ((not (vector-ref vec end))
         (next-free vec beg (+ end 1)))
        (else
         (cons beg end))))

;; next-file procedure
(define (next-file vec rbeg rend)
  (cond ((negative? rbeg)
         '())
        ((not (vector-ref vec rbeg))
         (next-file vec (- rbeg 1) (- rbeg 1)))
        ((or (negative? rend) (not (vector-ref vec rend)))
         (cons rbeg rend))
        ((= (vector-ref vec rend) (vector-ref vec rbeg))
         (next-file vec rbeg (- rend 1)))
        (else
         (cons rbeg rend))))

;; compact! procedure
(define (compact! vec free file)
    (let* ((beg (if (null? free) #f (car free)))
           (end (if (null? free) #f (cdr free)))
           (len (if (null? free) #f (- end beg)))
           (rbeg (if (null? file) #f (car file)))
           (rend (if (null? file) #f (cdr file)))
           (rlen (if (null? file) #f (- rbeg rend))))
      (cond ((null? file)
             (void))
            ((> beg rbeg)
             (compact! vec (next-free vec 0 0) (next-file vec rend rend)))
            ((> rlen len)
             (compact! vec (next-free vec end end) file))
            (else
             (subvector-fill! vec beg (+ beg rlen) (vector-ref vec rbeg))
             (subvector-fill! vec (+ rend 1) (+ rbeg 1) #f)
             (compact! vec (next-free vec 0 0) (next-file vec rend rend))))))

;; chksum procedure
(define (chksum vec k acc)
  (cond ((and (< k (vector-length vec)) (vector-ref vec k))
         (chksum vec (+ k 1) (+ acc (* k (vector-ref vec k)))))
        ((< k (vector-length vec))
         (chksum vec (+ k 1) acc))
        (else
         acc)))

;; example
(define ex-str (car (read-input "p18-ex.txt")))
(define ex-input (map char->digit (string->list ex-str)))
(define ex-len (fold-left + 0 ex-input))
(define ex-vec (make-vector ex-len #f))
(populate! ex-vec ex-input 0 0)
(define ex-free (next-free ex-vec 0 0))
(define ex-file (next-file ex-vec (- ex-len 1) (- ex-len 1)))
(compact! ex-vec ex-free ex-file)
(define ex-chksum (chksum ex-vec 0 0))
(begin ex-chksum)

;; solving p18
(define p18-str (car (read-input "p18-input.txt")))
(define p18-input (map char->digit (string->list p18-str)))
(define p18-len (fold-left + 0 p18-input))
(define p18-vec (make-vector p18-len #f))
(populate! p18-vec p18-input 0 0)
(define p18-free (next-free p18-vec 0 0))
(define p18-file (next-file p18-vec (- p18-len 1) (- p18-len 1)))
(compact! p18-vec p18-free p18-file)
(define p18-chksum (chksum p18-vec 0 0))
(begin p18-chksum)

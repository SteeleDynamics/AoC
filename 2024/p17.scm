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

;; compact! procedure
(define (compact! vec left right)
  (cond ((= left right)
         (void))
        ((vector-ref vec left)
         (compact! vec (+ left 1) right))
        ((not (vector-ref vec right))
         (compact! vec left (- right 1)))
        (else
         (vector-set! vec left (vector-ref vec right))
         (vector-set! vec right #f)
         (compact! vec (+ left 1) (- right 1)))))

;; chksum procedure
(define (chksum vec k acc)
  (if (vector-ref vec k)
      (chksum vec (+ k 1) (+ acc (* k (vector-ref vec k))))
      acc))

;; example
(define ex-str (car (read-input "p17-ex.txt")))
(define ex-input (map char->digit (string->list ex-str)))
(define ex-len (fold-left + 0 ex-input))
(define ex-vec (make-vector ex-len #f))
(populate! ex-vec ex-input 0 0)
(compact! ex-vec 0 (- ex-len 1))
(define ex-chksum (chksum ex-vec 0 0))
(begin ex-chksum)

;; solving p17
(define p17-str (car (read-input "p17-input.txt")))
(define p17-input (map char->digit (string->list p17-str)))
(define p17-len (fold-left + 0 p17-input))
(define p17-vec (make-vector p17-len #f))
(populate! p17-vec p17-input 0 0)
(compact! p17-vec 0 (- p17-len 1))
(define p17-chksum (chksum p17-vec 0 0))
(begin p17-chksum)

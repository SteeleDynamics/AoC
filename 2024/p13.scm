;; Using MIT/GNU Scheme for string manipulation support.
;; See MIT/GNU Scheme Reference Manual (v12.1) Chapter 6

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

;; char-delim? predicate procedure
(define (char-delim? c)
  (or (eq? c #\space)
      (eq? c #\:)
      ;; add'l chars...
      ))

;; split procedure
(define split (string-splitter 'delimiter char-delim?))

;; proc-input procedure
(define (proc-input string) (map string->number (split string)))

;;; problem-specific procedures

;; char->digit procedure
(define (char->digit c) (- (char->integer c) (char->integer #\0)))

;; integer->bits procedure
(define (integer->bits ei prec)
  (let* ((len (integer-length ei))
         (pad (- prec len))
         (str (number->string ei 2))
         (bits (map char->digit (string->list str))))
    (cond ((negative? pad) #f)
          ((zero? pad) bits)
          (else (append (make-list pad 0) bits)))))

;; bit->symbol procedure
(define (bit->symbol b) (if (> b 0) '* '+))

;; bit->proc procedure
(define (bit->proc b) (if (> b 0) * +))

;; eval-cmb procedure
(define (eval-cmb rators rands)
  (let ((acc (car rands))
        (rands (cdr rands)))
    (fold-left (lambda (acc rator rand) (rator acc rand)) acc rators rands)))

;; find-cmbs-iter procedure
(define (find-cmbs-iter val rands i prec acc)
  (let* ((bits (integer->bits i prec))
         (cmb (if bits (map bit->symbol bits) #f))
         (rators (if bits (map bit->proc bits) #f))
         (res (if rators (eval-cmb rators rands) #f)))
    (cond ((and bits (= res val))
           (find-cmbs-iter val rands (+ i 1) prec (cons cmb acc)))
          (bits
           (find-cmbs-iter val rands (+ i 1) prec acc))
          (else
           acc))))

;; find-cmbs procedure
(define (find-cmbs val rands)
  (let ((prec (- (length rands) 1)))
    (find-cmbs-iter val rands 0 prec '())))

;; example
(define ex-input (map proc-input (read-input "p13-ex.txt")))
(define ex-cmbs (map (lambda (x) (find-cmbs (car x) (cdr x))) ex-input))
(define ex-vals (map car ex-input))
(define ex-ans
  (fold-left (lambda (acc cmb val) (+ acc (if (null? cmb) 0 val)))
             0 ex-cmbs ex-vals))
(begin ex-ans)

;; solving p13
(define p13-input (map proc-input (read-input "p13-input.txt")))
(define p13-cmbs (map (lambda (x) (find-cmbs (car x) (cdr x))) p13-input))
(define p13-vals (map car p13-input))
(define p13-ans
  (fold-left (lambda (acc cmb val) (+ acc (if (null? cmb) 0 val)))
             0 p13-cmbs p13-vals))
(begin p13-ans)

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

;; integer->trits procedure (trinary digit)
(define (integer->trits ei prec)
  (let* ((str (number->string ei 3))
         (trits (map char->digit (string->list str)))
         (len (string-length str))
         (pad (- prec len)))
    (cond ((negative? pad) #f)
          ((zero? pad) trits)
          (else (append (make-list pad 0) trits)))))

;; trit->symbol procedure
(define (trit->symbol t)
  (cond ((= t 2) '||)
        ((= t 1) '*)
        (else '+)))

;; trit->proc procedure
(define (trit->proc t)
  (cond ((= t 2) cat)
        ((= t 1) *)
        (else +)))

;; cat-iter procedure
(define (cat-iter a b pow)
  (if (>= b pow)
      (cat-iter a b (* pow 10))
      (+ (* a pow) b)))

;; cat procedure
(define (cat a b) (cat-iter a b 1))

;; eval-cmb procedure
(define (eval-cmb rators rands)
  (let ((acc (car rands))
        (rands (cdr rands)))
    (fold-left (lambda (acc rator rand) (rator acc rand)) acc rators rands)))

;; find-cmbs-iter procedure
(define (find-cmbs-iter val rands i prec acc)
  (let* ((trits (integer->trits i prec))
         (cmb (if trits (map trit->symbol trits) #f))
         (rators (if trits (map trit->proc trits) #f))
         (res (if rators (eval-cmb rators rands) #f)))
    (cond ((and trits (= res val))
           (find-cmbs-iter val rands (+ i 1) prec (cons cmb acc)))
          (trits
           (find-cmbs-iter val rands (+ i 1) prec acc))
          (else
           acc))))

;; find-cmbs procedure
(define (find-cmbs val rands)
  (let ((prec (- (length rands) 1)))
    (find-cmbs-iter val rands 0 prec '())))

;; example
(define ex-input (map proc-input (read-input "p14-ex.txt")))
(define ex-cmbs (map (lambda (x) (find-cmbs (car x) (cdr x))) ex-input))
(define ex-vals (map car ex-input))
(define ex-ans
  (fold-left (lambda (acc cmb val) (+ acc (if (null? cmb) 0 val)))
             0 ex-cmbs ex-vals))
(begin ex-ans)

;; solving p14
(define p14-input (map proc-input (read-input "p14-input.txt")))
(define p14-cmbs (map (lambda (x) (find-cmbs (car x) (cdr x))) p14-input))
(define p14-vals (map car p14-input))
(define p14-ans
  (fold-left (lambda (acc cmb val) (+ acc (if (null? cmb) 0 val)))
             0 p14-cmbs p14-vals))
(begin p14-ans)

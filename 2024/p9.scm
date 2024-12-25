;; Using MIT/GNU Scheme for string-splitter support.
;; See MIT/GNU Scheme Reference Manual (v12.1) Section 6.1

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

;; cond-split procedure
(define (cond-split pred? x)
  (cond ((null? x) '(()))
        ((pred? (car x)) (cons '() x))
        (else (let ((res (cond-split pred? (cdr x))))
                (cons (cons (car x) (car res)) (cdr res))))))

;; acc-rule procedure
(define (acc-rule elt acc)
  (let* ((res ((string-splitter 'delimiter #\|) elt))
         (key (string->number (car res)))
         (val (string->number (cadr res))))
    (cons (cons key val) acc)))

;; acc-update procedure
(define (acc-update elt acc)
  (let ((res ((string-splitter 'delimiter #\,) elt)))
    (cons (map string->number res) acc)))

;; rel(evant)-rules procedure
(define (rel-rules rules updt)
  (filter
   (lambda (rule) (and (pair? (memq (car rule) updt))
                  (pair? (memq (cdr rule) updt))))
   rules))

;; in-order? predicate procedure
(define (in-order? rel updt)
  (fold-left
   (lambda (acc elt) (and acc (pair? (memq (cdr elt) (memq (car elt) updt)))))
   #t
   rel))

;; midpt procedure
(define (midpt x) (list-ref x (truncate (/ (length x) 2))))

;; example
(define ex-input (read-input "p9-ex.txt"))
(define ex-split (cond-split string-null? ex-input))
(define ex-rules (fold-right acc-rule '() (car ex-split)))
(define ex-updts (cdr (fold-right acc-update '() (cdr ex-split))))
(define ex-order
  (filter (lambda (u) (in-order? (rel-rules ex-rules u) u)) ex-updts))
(define ex-answr (fold-left + 0 (map midpt ex-order)))
(begin ex-answr)

;; solving p9
(define p9-input (read-input "p9-input.txt"))
(define p9-split (cond-split string-null? p9-input))
(define p9-rules (fold-right acc-rule '() (car p9-split)))
(define p9-updts (cdr (fold-right acc-update '() (cdr p9-split))))
(define p9-order
  (filter (lambda (u) (in-order? (rel-rules p9-rules u) u)) p9-updts))
(define p9-answr (fold-left + 0 (map midpt p9-order)))
(begin p9-answr)

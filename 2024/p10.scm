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

;; rule-ok? predicate procedure
(define (rule-ok? rule updt)
  (pair? (memq (cdr rule) (memq (car rule) updt))))

;; in-order? predicate procedure
(define (in-order? rules updt)
  (fold-left
   (lambda (acc rule) (and acc (rule-ok? rule updt)))
   #t
   rules))

;; out-of-order? predicate procedure
(define (out-of-order? rules updt) (not (in-order? rules updt)))

;; swap! procedure
(define (swap! rule updt)
  (let* ((a (memq (car rule) updt))
         (b (memq (cdr rule) updt))
         (t (car a)))
    (set-car! a (car b))
    (set-car! b t)))

;; reorder! procedure
(define (reorder! rules updt)
  (cond ((in-order? rules updt) updt)
        (else
         (fold-left
          (lambda (acc rule) (if (rule-ok? rule updt) unspecific (swap! rule updt)))
          unspecific
          rules)
         (reorder! rules updt))))

;; midpt procedure
(define (midpt x) (list-ref x (truncate (/ (length x) 2))))

;; example
(define ex-input (read-input "p10-ex.txt"))
(define ex-split (cond-split string-null? ex-input))
(define ex-rules (fold-right acc-rule '() (car ex-split)))
(define ex-updts (cdr (fold-right acc-update '() (cdr ex-split))))
(define ex-mixed
  (filter (lambda (u) (out-of-order? (rel-rules ex-rules u) u)) ex-updts))
(map (lambda (u) (reorder! (rel-rules ex-rules u) u)) ex-mixed)
(define ex-answr (fold-left + 0 (map midpt ex-mixed)))
(begin ex-answr)

;; solving p10
(define p10-input (read-input "p10-input.txt"))
(define p10-split (cond-split string-null? p10-input))
(define p10-rules (fold-right acc-rule '() (car p10-split)))
(define p10-updts (cdr (fold-right acc-update '() (cdr p10-split))))
(define p10-mixed
  (filter (lambda (u) (out-of-order? (rel-rules p10-rules u) u)) p10-updts))
(map (lambda (u) (reorder! (rel-rules p10-rules u) u)) p10-mixed)
(define p10-answr (fold-left + 0 (map midpt p10-mixed)))
(begin p10-answr)

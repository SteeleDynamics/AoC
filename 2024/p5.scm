;; Using MIT/GNU Scheme for Regular Expression support.
;; See MIT/GNU Scheme Reference Manual (v12.1) Section 6.2

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

;; input-col procedure
(define (input-col input k)
  (fold-right (lambda (elt acc) (cons (list-ref elt k) acc)) '() input))

;; try-match procedure
(define (try-match pattern string)
  (regsexp-match-string (compile-regsexp pattern) string))

;; try-search procedure
(define (try-search pattern string)
  (regsexp-search-string-forward (compile-regsexp pattern) string))

;; regex "mul\([:digit:]{1,3},[:digit:]{1,3}\)"
(define pattern
  '(seq "mul("
        (group a (** 1 3 (char-in numeric))) ","
        (group b (** 1 3 (char-in numeric))) ")"))

;; try-search-all procedure
(define (try-search-all pattern string)
  (let ((res (try-search pattern string)))
    (if (not (pair? res))
        '()
        (cons (cddr res)
              (try-search-all pattern (substring string (cadr res)))))))

;; add-mul procedure
(define (add-mul acc elt)
  (+ acc (* (string->number (cdr (assq 'a elt)))
            (string->number (cdr (assq 'b elt))))))

;; proc-res procedure
(define (proc-res res) (fold-left add-mul 0 res))

;; proc-input procedure
(define (proc-input pat input)
  (fold-left (lambda (acc elt) (+ acc (proc-res (try-search-all pat elt)))) 0 input))

;; solving p5
(define p5-input (read-input "p5-input.txt"))
(define p5-ans (proc-input pattern p5-input))
p5-ans

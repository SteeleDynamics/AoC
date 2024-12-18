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

;; regex "do\(\)|don't\(\)|mul\([:digit:]{1,3},[:digit:]{1,3}\)"
(define pattern
  '(alt (group do "do()")
        (group dont "don't()")
        (seq (group mul "mul(")
             (group a (** 1 3 (char-in numeric))) ","
             (group b (** 1 3 (char-in numeric))) ")")))

;; try-search-all procedure
(define (try-search-all pattern string)
  (let ((res (try-search pattern string)))
    (if (not (pair? res))
        '()
        (cons (cddr res)
              (try-search-all pattern (substring string (cadr res)))))))

;; assq? predicate procedure
(define (assq? obj alist) (pair? (assq obj alist)))

;; eval-prog procedure
(define (eval-prog prog en acc)
  (if (null? prog)
      acc
      (let ((instr (car prog)))
        (cond ((and en (assq? 'dont instr))
               (eval-prog (cdr prog) #f acc))
              ((and en (assq? 'mul instr))
               (eval-prog
                (cdr prog) #t
                (+ acc (* (string->number (cdr (assq 'a instr)))
                          (string->number (cdr (assq 'b instr)))))))
              ((and (not en) (assq? 'do instr))
               (eval-prog (cdr prog) #t acc))
              (else
               (eval-prog (cdr prog) en acc))))))

;; solving p6
(define p6-input (read-input "p6-input.txt"))
(define p6-prog
  (fold-left append '() (map (lambda (x) (try-search-all pattern x)) p6-input)))
(define p6-ans (eval-prog p6-prog #t 0))
p6-ans

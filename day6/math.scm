;; 123 328  51 64 
;;  45 64  387 23 
;;   6 98  215 314
;; *   +   *   +  

;; Problem:
;; Each column (columns separated by one or more spaces) is a math problem.
;; Each number in the columns row are the operands of the math problem.
;; The last row in the column is the operand to apply to all the operands.
;; Sum the answers of each problem.


;; Steps:

;; 1. Parse the input
;;     - Each line is the same length
;;     - Split each row into a list of strings (split by space, make sure to greedily take all spaces out to avoid blank elements)
;;     - End up with multiple rows as lists
;; 2. Create an iterator that traverse all of the rows at the same time using car and cdr
;;     - Perform the arithmetic with the operator and current operands, adding to the sum
;; 3. done

(define (read-file file-port)
  ;; Load all rows into a list of lists (split by all spaces)
  (define (iter line rows)
    (if (eof-object? line)
      rows
      (iter (read-line file-port) (append rows (list (handle-line line))))))
  (handle-rows (iter (read-line file-port) '()) 0))

(define (handle-line line)
  ((string-splitter) line))

(define (handle-rows rows sum)
  (if (null? (car rows))
    sum
    (let* ((operator-string (car (list-ref rows (dec (length rows)))))
           (initial (if (equal? operator-string "*") 1 0))
           (operator (cond ((equal? operator-string "*") *)
                           ((equal? operator-string "+") +)
                           ((equal? operator-string "-") -)
                           (else "unknown operator")))
           (answer (accumulate operator initial (map (lambda (operand-row) (string->number (car operand-row))) (sublist rows 0 (dec (length rows)))))))
      (handle-rows (map (lambda (row) (cdr row)) rows) (+ sum answer)))))

(define (accumulate proc initial seq)
  (if (null? seq)
    initial
    (proc (car seq) (accumulate proc initial (cdr seq)))))

(define (println . args)
  (newline)
  (for-each (lambda (x) (display x) (display " ")) args))

(define (dec x)
  (- x 1))

(println (call-with-input-file "input" read-file))

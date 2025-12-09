(define (read-file file-port)
  (define (iter line sum)
    (if (eof-object? line)
      sum
      (iter (read-line file-port) (handle-line line sum))))
  (iter (read-line file-port) 0))

(define (handle-line line sum)
  (+ sum (find-largest-joltage line)))

(define (find-largest-joltage line)
  (let ((bank (string->number-list line)))

    ;; Returns a pair of the largest number with it's index.
    (define (find-left-largest index nums current-largest-pair)
      (cond ((null? nums) current-largest-pair)
	    ((and (> (car nums) (pair-value current-largest-pair)) (not-last-index bank index)) (find-left-largest (inc index) (cdr nums) (make-index-value-pair index (car nums))))
	    (else (find-left-largest (inc index) (cdr nums) current-largest-pair))))

    ;; Returns a pair of the largest number with it's index.
    (define (find-right-largest nums current-largest)
      (cond ((null? nums) current-largest)
	    ;; New largest and it's not the last number
	    ((and (> (car nums) current-largest)) (find-right-largest (cdr nums) (car nums)))
	    (else (find-right-largest (cdr nums) current-largest))))

    (let* ((left-largest-pair (find-left-largest 0 bank (make-index-value-pair -1 -1)))
	   (right-largest-value (find-right-largest (sublist bank (inc (pair-index left-largest-pair)) (length bank)) -1)))
      (string->number (string (pair-value left-largest-pair) right-largest-value)))))

(define (string->number-list str)
  (map (lambda (c) (string->number (string c))) (string->list str)))

(define (println . args)
  (newline)
  (for-each (lambda (x) (display x) (display " ")) args))

(define (inc x)
  (+ x 1))

(define (not-last-index seq index)
  (let ((last-index (- (length seq) 1)))
    (< index last-index)))

(define (make-index-value-pair index value)
  (cons index value))
(define (pair-index p)
  (car p))
(define (pair-value p)
  (cdr p))

(println (call-with-input-file "input" read-file))

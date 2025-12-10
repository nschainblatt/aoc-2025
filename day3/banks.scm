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

    (define (find-batteries nums)

      ;; Returns a index-value-pair of the largest number in the sequence.
      (define (find-largest index nums current-largest-pair)
	(cond ((null? nums) current-largest-pair)
	      ((> (car nums) (pair-value current-largest-pair)) (find-largest (inc index) (cdr nums) (make-index-value-pair index (car nums))))
	      (else (find-largest (inc index) (cdr nums) current-largest-pair))))

      ;; Returns a string representing the largest 12 digit number in the sequence of nums.
      ;; It does this by iterating through valid ranges for each digit in a 12 digit number,
      ;; starting with the most significant digit, all the way to the least.
      ;; A valid range is one where the current digit may reside, with enough numbers remaining (depends on the placement).
      ;; For example the most significant bit (12th from the right) must have room for at least 11 digits to the right of it
      ;; in the sequence.
      ;; We can be sure that this will find the largest possible 12 digit number in the sequence because we start searching for
      ;; the most significant bit.
      ;; Note that end-index is exclusive.
      (define (iter start-index end-index string-result)
	(if (out-of-bounds? nums (dec end-index))
	  string-result
	  (let ((index-value-pair (find-largest start-index (sublist nums start-index end-index) (make-index-value-pair -1 -1))))
	    (iter (inc (pair-index index-value-pair)) (inc end-index) (string string-result (pair-value index-value-pair))))))

      (iter 0 (- (length nums) 11) ""))
    (string->number (find-batteries bank))))

(define (string->number-list str)
  (map (lambda (c) (string->number (string c))) (string->list str)))

(define (println . args)
  (newline)
  (for-each (lambda (x) (display x) (display " ")) args))

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (not-last-index seq index)
  (let ((last-index (- (length seq) 1)))
    (< index last-index)))

(define (out-of-bounds? seq index)
  (> index (- (length seq) 1)))

(define (make-index-value-pair index value)
  (cons index value))
(define (pair-index p)
  (car p))
(define (pair-value p)
  (cdr p))

(println (call-with-input-file "input" read-file))

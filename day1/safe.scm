(define starting-dial 50)

(define (make-logger dial zero-count)
  (cons dial zero-count))
(define (logger-dial logger)
  (car logger))
(define (logger-zero-count logger)
  (cdr logger))

;; Returns a pair consisting of the zeroes encounters in the car position, and the final dial count in the cdr position.
(define (read-file file-port)
  (define (iter line logger)
    (if (eof-object? line)
      logger
      (iter (read-line file-port) (handle-line line logger))))
  (iter (read-line file-port) (make-logger starting-dial 0)))

(define (handle-line line logger)
  (let* ((dial (logger-dial logger))
	 (zero-count (logger-zero-count logger))
	 (distance (string->number (substring line 1)))
	 (direction (substring line 0 1))
	 (operation (if (equal? direction "L") - +))
	 (raw-amount (operation dial distance))
	 (new-dial (modulo raw-amount 100))
	 (amount-to-zero-left dial)
	 (amount-to-zero-right (- 100 dial))
	 ;; Notice how this eliminates having to loop over the distance, we only have to loop over the lines.
	 (crosses (cond ((and (equal? direction "L") (> distance amount-to-zero-left))
			 (+ (abs (quotient raw-amount 100)) (if (= dial 0) 0 1)))
			((and (equal? direction "R") (> distance amount-to-zero-right) (quotient raw-amount 100)))
			((and (equal? direction "L") (= distance amount-to-zero-left) 1))
			((and (equal? direction "R") (= distance amount-to-zero-right) 1))
			(else 0))))
    (make-logger new-dial (+ zero-count crosses))))

(define (println x)
  (newline)
  (display x))

(define (test line expected-dial expected-zero-count)
  (let* ((starting-pair (cons 50 0))
	 (expected (cons expected-dial expected-zero-count))
	 (actual (handle-line line starting-pair)))
    (assert (equal? expected actual) line 'Expected: expected 'but 'got actual)))

(test "L50" 0 1)
(test "L51" 99 1)
(test "L150" 0 2)
(test "L151" 99 2)

(test "R50" 0 1)
(test "R51" 1 1)
(test "R150" 0 2)
(test "R151" 1 2)

(println (call-with-input-file "input" read-file))

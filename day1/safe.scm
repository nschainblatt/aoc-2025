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
  (let* ((distance (string->number (substring line 1)))
	 (direction (substring line 0 1))
	 (operation (if (equal? direction "L") - +))
	 (new-dial (modulo (operation (logger-dial logger) distance) 100)))
    (make-logger new-dial (if (= new-dial 0) (+ (logger-zero-count logger) 1) (logger-zero-count logger)))))

(define (println x)
  (newline)
  (display x))

(println (call-with-input-file "input" read-file))

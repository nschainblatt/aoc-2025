;; 1. Read all fresh id ranges into a list, converting the left of the range to be the min, and the right to be the max.
;;     - Read lines until a blank line
;;     - End up with this: ((3 . 5) (10 . 14) ...)

;; 2. Go over each ID to check (starting at the line after the blank line). Validate it exists in at least one of the fresh id ranges by iterating through them.
;;     - ID is greater than or equal to min AND less than or equal to max of the range.
;;     - Try with each range until one succeeds, if one succeeds return 1, if none succeed return 0.

;; 3. Keep track of the fresh ID count.

(define (read-file file-port)
  (define (fresh-id-range-iter line fresh-id-ranges)
    (if (blank-line? line)
      fresh-id-ranges
      (fresh-id-range-iter (read-line file-port) (cons (convert-range-to-pair line) fresh-id-ranges))))
  (let ((fresh-id-ranges (fresh-id-range-iter (read-line file-port) '())))
    (define (id-iter line sum)
      (if (eof-object? line)
	sum
	(let ((id (string->number line)))
	  (if (is-fresh-id? id fresh-id-ranges)
	    (id-iter (read-line file-port) (inc sum))
	    (id-iter (read-line file-port) sum)))))
    (id-iter (read-line file-port) 0)))

(define (is-fresh-id? id fresh-id-ranges)
  (if (null? fresh-id-ranges)
    #f
    (let* ((range (car fresh-id-ranges))
	   (start (car range))
	   (end (cdr range)))
      (if (and (>= id start) (<= id end))
	#t
	(is-fresh-id? id (cdr fresh-id-ranges))))))

(define (blank-line? line)
  (equal? line ""))

(define (convert-range-to-pair range)
  (let* ((parts ((string-splitter 'delimiter #\-) range))
	 (start (string->number (car parts)))
	 (end (string->number (list-ref parts (- (length parts) 1)))))
    (cons start end)))

(define (println . args)
  (newline)
  (for-each (lambda (x) (display x) (display " ")) args))

(define (inc x)
  (+ x 1))

(define (>= x y)
  (or (> x y) (= x y)))

(define (<= x y)
  (or (< x y) (= x y)))

(println (call-with-input-file "input" read-file))

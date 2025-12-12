(define (read-file file-port)
  (define (iter line rows-cols)
    (if (eof-object? line)
      rows-cols
      (iter (read-line file-port) (append rows-cols (list (string->list line))))))
  (let ((input (iter (read-line file-port) '())))
    (handle-rows-cols input)))

(define (handle-rows-cols rows-cols)
  (define (row-iter row-index rows sum)
    (if (null? rows)
      sum
      (let ((row (car rows)))
	(define (col-iter col-index cols row-sum)
	  (if (null? cols)
	    row-sum
	    (col-iter (inc col-index)
		      (cdr cols)
		      (+ row-sum (let ((curr (car cols)))
				   (if (not (equal? curr #\@))
				     0
				     (if (> (get-rolls-surrounding-index row-index col-index rows-cols row) 3)
				       0
				       1)))))))
	(row-iter (inc row-index) (cdr rows) (+ sum (col-iter 0 (car rows) 0))))))
  (row-iter 0 rows-cols 0))

(define (get-rolls-surrounding-index row-index col-index rows-cols row)
  (+ (if (roll-up? row-index col-index rows-cols row) 1 0)
     (if (roll-down? row-index col-index rows-cols row) 1 0)
     (if (roll-left? row-index col-index rows-cols row) 1 0)
     (if (roll-right? row-index col-index rows-cols row) 1 0)
     (if (roll-up-left? row-index col-index rows-cols row) 1 0)
     (if (roll-up-right? row-index col-index rows-cols row) 1 0)
     (if (roll-down-left? row-index col-index rows-cols row) 1 0)
     (if (roll-down-right? row-index col-index rows-cols row) 1 0)))

(define (roll-up? row-index col-index rows-cols row)
  (let ((row-index (dec row-index)))
    (if (or (out-of-bounds? rows-cols row-index)
	    (out-of-bounds? row col-index))
      #f
      (equal? (list-ref (list-ref rows-cols row-index) col-index) #\@))))

(define (roll-down? row-index col-index rows-cols row)
  (let ((row-index (inc row-index)))
    (if (or (out-of-bounds? rows-cols row-index)
	    (out-of-bounds? row col-index))
      #f
      (equal? (list-ref (list-ref rows-cols row-index) col-index) #\@))))

(define (roll-left? row-index col-index rows-cols row)
  (let ((col-index (dec col-index)))
    (if (or (out-of-bounds? rows-cols row-index)
	    (out-of-bounds? row col-index))
      #f
      (equal? (list-ref (list-ref rows-cols row-index) col-index) #\@))))

(define (roll-right? row-index col-index rows-cols row)
  (let ((col-index (inc col-index)))
    (if (or (out-of-bounds? rows-cols row-index)
	    (out-of-bounds? row col-index))
      #f
      (equal? (list-ref (list-ref rows-cols row-index) col-index) #\@))))

(define (roll-up-left? row-index col-index rows-cols row)
  (let ((row-index (dec row-index))
	(col-index (dec col-index)))
    (if (or (out-of-bounds? rows-cols row-index)
	    (out-of-bounds? row col-index))
      #f
      (equal? (list-ref (list-ref rows-cols row-index) col-index) #\@))))

(define (roll-up-right? row-index col-index rows-cols row)
  (let ((row-index (dec row-index))
	(col-index (inc col-index)))
    (if (or (out-of-bounds? rows-cols row-index)
	    (out-of-bounds? row col-index))
      #f
      (equal? (list-ref (list-ref rows-cols row-index) col-index) #\@))))

(define (roll-down-left? row-index col-index rows-cols row)
  (let ((row-index (inc row-index))
	(col-index (dec col-index)))
    (if (or (out-of-bounds? rows-cols row-index)
	    (out-of-bounds? row col-index))
      #f
      (equal? (list-ref (list-ref rows-cols row-index) col-index) #\@))))

(define (roll-down-right? row-index col-index rows-cols row)
  (let ((row-index (inc row-index))
	(col-index (inc col-index)))
    (if (or (out-of-bounds? rows-cols row-index)
	    (out-of-bounds? row col-index))
      #f
      (equal? (list-ref (list-ref rows-cols row-index) col-index) #\@))))

(define (string->number-list str)
  (map (lambda (c) (string->number (string c))) (string->list str)))

(define (println . args)
  (newline)
  (for-each (lambda (x) (display x) (display " ")) args))

(define (set-ref! seq index value)
  (define (iter sub-seq i)
    (if (= i index)
      (set-car! sub-seq value)
      (iter (cdr sub-seq) (inc i))))
  (cond ((< index 0) (error "index must be positive"))
	((> index (dec (length seq))) (error "index out of bounds"))
	(else (iter seq 0))))

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (not-last-index seq index)
  (let ((last-index (- (length seq) 1)))
    (< index last-index)))

(define (out-of-bounds? seq index)
  (or (> index (- (length seq) 1))
      (< index 0)))

(define (make-index-value-pair index value)
  (cons index value))
(define (pair-index p)
  (car p))
(define (pair-value p)
  (cdr p))

(println (call-with-input-file "input" read-file))

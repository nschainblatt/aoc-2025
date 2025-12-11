;; There cannot be more than 3 rols in the 8 positions around a point for the machine to reach it.
;; 
;; I think if a roll is on the border, all the positions that aren't on screen are vacant.

;; Problems:

;; 1. Find a way to load the input into an efficient lookup data structure (probably not sequences or pairs, maybe look into vectors)
;; https://docs.scheme.org/guide/arrays/
;; can use lists or vectors, can make my solution in a way we can easily swap the data structure used to view performance differences.

;; 2. Given a way to instantly lookup the positions around the current, all we have to do is verify the current position doesn't
;; have more than 3 rolls surrounding it in the 8 positions.

;; 3. Utilize a positions matrix to easily get the directions, for example, given a two dimensional array:
;;                 r  c
;;    up:         -1, 0
;;    down:        1, 0
;;    left:        0,-1
;;    right:       0, 1
;;    up-right:   -1, 1
;;    up-left:    -1,-1
;;    down-right:  1, 1
;;    down-left:  -1,-1

;; Steps:

;; 1. Read the entire input into a list of lists (row columns)

;; 2. Pass the two dimensional list to a handler that does the following:
;;    - iterates over each row
;;       - iterates over each column
;;          - if the value in the cell is not a roll, continue
;;          - if the value is a roll, check the surrounding 8 characters
;;              - as soon as 4 rolls are found in the surrounding env, exits early with 0
;;              - if any of the surrounding positions are out of bounds, they are assumed to be empty
;;              - if 3 or less rolls are surrounding the current position, return 1 signalling this is a roll to count.

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

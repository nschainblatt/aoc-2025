;; We have to get the total number of valid ids (without duplicates for ranges that cross eachother)
;; The real input has some extremely large ranges, so we cannot just count through the range and store each id in a set to remove duplicates.

;; We are going to have to implement some kind of low and high range checker to do the following:

;; - Get the total number in the current fresh id range
;; - easy end-start + 1 (inclusive)

;; - how many of these numbers have we already encountered in a previous range?
;; - we will have to store the high and the low of a previous range in a data structure to then check against with every subsequent range

;; for example:

;; fresh ids:
;; 10-14
;; 12-18

;; Lets say we've gone through 10-14, and have (10-14) in some sort of data structure we have access to.
;; We are processing 12-18 currently (there are duplicates, but we don't know yet).
;; We take the high and low of this current range and compare it to all previous ranges, checking for overlap:

;; prev: 10-14
;; curr: 12-18
;; overlap: 12,13,14

;; how to detect without counting through???
;; there are a few conditions we can use

;; 1. if our current low falls inbetween the prev range (greater than or equal to prev low and less than or equal to prev high)
;; 2. if our current high falls inbetween the prev range (greater than or equal to prev low and less than or equal to prev high)

;; 1.
;; We would have to check both, in this case only the first passes (10 <= 12 <= 14)
;; When only the first passes, we have to find the difference between the low and the high (12 and 14) + 1 to be inclusive
;; That would be 14 - 12 + 1 = 3, which equals the three duplicates. 
;; We would then just add the rest of the ranges (assuming they don't conflict with any existing ranges). (15 16 17)

;; 2.
;; In the case our current range only had the high within range, we would only have to exclude one id count (the high value)
;; for example:
;; prev = 10-14
;; curr = 14-20
;; we would only have to leave out 1 id (14-14) + 1 = 1



;; 1 && 2.
;; If both the low and the high of the curr range in in the range of the previous, such as:
;; prev 10-14
;; curr 12-13

;; we would have to simply count the numbers in the curr range, and make sure not to include them in the id count.
;; this is pretty easy since they both fall in the same range.



;; All possible cases with solutions:

;; prev: 10-14
;; curr: 1-2
;; Explanation: both the curr low and high are not in the range. No change.

;; prev: 10-14
;; curr: 10-12
;; Explanation: both the high and the low are in the range, subtract the number of ids in the curr range. You can do this by doing 12-10 + 1 = 3.

;; prev: 10-14
;; curr: 12-18
;; Explanation: only the curr low is in the range, the high is not. So we must subtract the curr low from the prev high inclusive to get the total count to subtract:
;; 14 - 12 + 1 = 3 (12,13,14)

;; prev: 10-14
;; curr: 15-18
;; Explanation: both are too high, not in range.

;; prev: 10-14
;; curr: 10-14
;; Explanation: Both are in range, subtract the count of ids in the curr. 14-10 + 1 = 5.

;; prev: 10-14
;; curr 14-18
;; Explanation: only the high is in the range. Thats only one id to subtract. (14-14) + 1 = 1


;; PATTERN:
;; Check if the low and the high is in range, if so subtract the total number of ids in the current range.
;; Else if the low is in range, but the high is not, so prev high minus curr low + 1
;; Else if just the high is in the range, subtract 1.
;; Else do nothing (not in range)

;; Note, these examples were only for one prev range.
;; We will have to loop through all previous ranges to filter out duplicate ids.
;; NOTE: We have to be careful not to remove the same id twice.
;; We can do this by exiting early the first time we've dealt with the current ranges duplication (however, we would still need to check if there are any remaining ids not
;; removed from duplication, such as the first half of the range was deemed duplicate with the current prev, but had some leftover, so we have to check the next prev) 
;; Each time we check the current prev, we should go to the next prev with only the parts of the range we still have to check. (making an new range)

;; Steps:

;; 1. Parse the file, we now only care about the ranges, so stop reading once we hit that blank line.
;;
;; 2. Start reading ranges line by line
;;      - For each range, compare it against previous ranges using the algorithm described above.
;;	- When certain parts of the current range are deemed duplicate, create a new range without them and pass this new range to be checked against the next previous
;;        range. This will probably be a procedure that goes through all previous ranges with the current and returns the total number of distinct ids to add to our sum
;;      - add the current range to the prev list after processing it
;;      - move onto the next list
;; 3. Keep a running total of all the count of the distinct ids.

(define (read-file file-port)
  (define (iter line sum prev-ranges)
    (println "iter" line sum prev-ranges)
    (if (blank-line? line)
      sum
      (let* ((range (handle-line line))
	    (new-sum (+ sum (check-ids range prev-ranges))))
	(println new-sum)
	(iter (read-line file-port) new-sum (cons range prev-ranges)))))
  (iter (read-line file-port) 0 '()))

(define (handle-line range)
  (let* ((parts ((string-splitter 'delimiter #\-) range))
	 (low (string->number (car parts)))
	 (high (string->number (list-ref parts (- (length parts) 1)))))
    (make-range low high)))

;; Returns the number of fresh ids in curr-range that aren't duplicating any in the prev-ranges.
(define (check-ids curr-range prev-ranges)

  ;; prev: 10-14 oob
  ;; curr: 1-2

  ;; prev: 10-14
  ;; curr: 8-10  just low

  ;; prev: 10-14 both in
  ;; curr: 10-12

  ;; prev: 10-14 both in
  ;; curr: 12-13

  ;; prev: 10-14 both in
  ;; curr: 12-14

  ;; prev: 10-14
  ;; curr: 14-20 just high

  ;; prev: 10-14 oob
  ;; curr 15-20

  ;; TODO: handle case where they are both oob, we have to get the ids in the range that are duplicates.
  ;; prev: 10-14
  ;; curr: 8-18

  (if (null? prev-ranges)
    (numbers-in-range-inclusive curr-range)
    (let* ((prev (car prev-ranges))
	   (prev-low (range-low prev))
	   (prev-high (range-high prev))
	   (curr-low (range-low curr-range))
	   (curr-high (range-high curr-range)))

      (println "check-ids" curr-range prev)

      (cond
	;; Both ends of curr are too low, try full range again with next prev range (note don't have to check both ends because if curr-high is too low, so is curr-low)
	((< curr-high prev-low) (println "TOO LOW") (check-ids curr-range (cdr prev-ranges)))
	;; Both ends of curr are too high, try full range again with next prev range (")
	((> curr-low prev-high) (println "TOO HIGH") (check-ids curr-range (cdr prev-ranges)))
	;; Both are in bounds, end early (all duplicate ids)
	((and (>= curr-low prev-low) (<= curr-high prev-high)) (println "ALL DUPLICATE") (check-ids 'end '()))
	;; Just low is in bounds (only have to check low since they are both already not in the range at this point)
	((and (>= curr-low prev-low) (<= curr-low prev-high)) (println "JUST LOW") (check-ids (make-range (inc prev-high) curr-high) (cdr prev-ranges)))
	;; Just high is in bounds (")
	((and (>= curr-high prev-low) (<= curr-high prev-high)) (println "JUST HIGH") (check-ids (make-range curr-low (dec prev-low)) (cdr prev-ranges)))
	;; Curr low is too low, and curr high it too high, we have to create a new range leaving out duplicates in between
	;; We may have to use tree recursion to do this, we may have to create and check two additional ranges (before and after the prev range)
	((and (< curr-low prev-low) (> curr-high prev-high)) (println "TOO LOW AND TOO HIGH, BRANCHING") (+ (check-ids (make-range curr-low (dec prev-low)) (cdr prev-ranges)) (check-ids (make-range (inc prev-high) curr-high) (cdr prev-ranges))))
	(else (error "idk how i got here" curr-range prev)))

      )))

;; Range may be a range object, other types allow me to signal no valid ids.
(define (numbers-in-range-inclusive range)
  (if (not (pair? range))
    0
    (+ (- (range-high range) (range-low range)) 1)))

(define make-range cons)
(define range-low car)
(define range-high cdr)

(define (blank-line? line)
  (equal? line ""))

(define (println . args)
  (newline)
  (for-each (lambda (x) (display x) (display " ")) args))

(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))

(println (call-with-input-file "input" read-file))

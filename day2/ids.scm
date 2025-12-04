(define (read-file file-port)
  (define (iter line sum)
    (if (eof-object? line)
      sum
      (iter (read-line file-port) (handle-line line sum))))
  (iter (read-line file-port) 0))

(define (handle-line line sum)
  (let ((ranges ((string-splitter 'delimiter #\,) line)))
    (ranges-iter ranges sum)))

(define (ranges-iter ranges sum)
  (if (null? ranges)
    sum
    (ranges-iter (cdr ranges) (+ sum (handle-range (car ranges))))))

(define (handle-range range)
  (let* ((parts ((string-splitter 'delimiter #\-) range))
         (start (string->number (car parts)))
         (end (string->number (list-ref parts (- (length parts) 1)))))
    (count-through-range start end 0)))

(define (count-through-range start end sum)
  (if (> start end)
    sum
    (let ((start-as-string (number->string start)))
      (if (= (remainder (string-length start-as-string) 2) 0)
        (let* ((left-half (string-slice start-as-string 0 (quotient (string-length start-as-string) 2)))
               (right-half (string-slice start-as-string (quotient (string-length start-as-string) 2)))
               (new-sum (if (equal? left-half right-half) (+ sum start) sum)))
          (count-through-range (+ start 1) end new-sum))
        (count-through-range (+ start 1) end sum)))))

(define (println x)
  (newline)
  (display x))


(println (call-with-input-file "input" read-file))

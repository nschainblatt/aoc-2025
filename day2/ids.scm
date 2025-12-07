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
    (let* ((start-as-string (number->string start))
           (new-sum (if (is-id-invalid? start-as-string) (+ sum start) sum)))
      (count-through-range (+ start 1) end new-sum))))

;; Take the id, inspect sequence by sequence, starting with the first digit, increasing in size over time if no matches found so far.
(define (is-id-invalid? id-string)
  (define (iter seq rest)
          ;; If the rest of the string is empty, that means the seq is the whole string, thus no two matches exist.
    (cond ((string-empty? rest) #f)
          ((is-rest-repeating-seq? seq rest) #t)
          ;; Invalid condition not met yet, increase the size of the seq by one, check for matches again.
          (else (iter (string-append seq (substring rest 0 1)) (substring rest 1)))))

  ;; Return true if rest only contains repeats of seq, otherwise false.
  ;; Use two pointers to track separate positions in seq and rest.
  ;; As soon as a seq match is found in rest, the seq-ptr is reset to allow further matches to be found.
  (define (is-rest-repeating-seq? seq rest)

    (define (iter seq-ptr rest-ptr)
            ;; We've reached the end of the comparison, everything matched. seq-ptr must also be oob to prove that the last digit
            ;; of the id was apart of the last digit of the last match.
      (cond ((and (out-of-bounds? rest rest-ptr) (out-of-bounds? seq seq-ptr)) #t)
            ;; We've reached the end, seq isn't out of bounds, meaning it didn't finish it's last match before rest ended.
            ((out-of-bounds? rest rest-ptr) #f)
            ;; End of the current match, check the next one.
            ((out-of-bounds? seq seq-ptr) (iter 0 rest-ptr))
            ;; Check the current characters ensuring they are matching.
            (else
              (let ((seq-chr (string-ref seq seq-ptr))
                    (rest-chr (string-ref rest rest-ptr)))
                (if (equal? seq-chr rest-chr)
                  (iter (inc seq-ptr) (inc rest-ptr))
                  #f)))))
    (iter 0 0))

  (iter (substring id-string 0 1) (substring id-string 1)))

(define (println . args)
  (newline)
  (for-each (lambda (x) (display x) (display " ")) args))

(define (inc x)
  (+ x 1))

(define (out-of-bounds? str index)
  (> index (- (string-length str) 1)))

(define (string-empty? str)
  (= (string-length str) 0))

(println (call-with-input-file "example-input" read-file))

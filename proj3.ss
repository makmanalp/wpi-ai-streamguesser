(#%require scheme)

(require 2htdp/batch-io)

;; Parse a file and return a list of strings.
(define (parse-file filename)
  (filter (lambda (x) (not (= (string-length x) 0)))
          (regexp-split "[,(\r)(\n)]" (read-file filename))))

(define FREQUENCIES (make-hash))

;; _count-occurences  list(string) : changes FREQUENCIES
(define (count-occurences training_set)
  (if (empty? training_set)
      FREQUENCIES
      (begin
        (let ((curr (first training_set)))
          (if (hash-has-key? FREQUENCIES curr)
              (hash-set! FREQUENCIES curr (+ (hash-ref FREQUENCIES curr) 1))
              (hash-set! FREQUENCIES curr 1)))
        (count-occurences (rest training_set)))))


;; Get the most common symbol on the FREQUENCIES set.
(define (get-most-common)
  (let ((firstIndex (hash-iterate-first FREQUENCIES)))
    (hash-iterate-key FREQUENCIES (get-most-common-acc (hash-iterate-value FREQUENCIES firstIndex) 
                                                       firstIndex
                                                       firstIndex))))

(define (get-most-common-acc best index bestIndex)
      (let ((curr (hash-iterate-value FREQUENCIES index)))
        (if (false? (hash-iterate-next FREQUENCIES index))
            (if (> curr best)
                index
                bestIndex) 
            (if (> curr best)
                (get-most-common-acc curr (hash-iterate-next FREQUENCIES index) index)
                (get-most-common-acc best (hash-iterate-next FREQUENCIES index) bestIndex)))))

;; Majority class.  Just use the most common occurence.
(define (majority-class test_set)
  (if (empty? test_set)
      empty
      (cons (get-most-common) (majority-class (rest test_set)))))

;; Get the accuracy of the answer set based on the test set.
(define (accuracy answer test_set)
  (/ (accuracy-acc answer test_set 0)
     (length answer)))

(define (accuracy-acc answer test_set num)
  (if (empty? answer)
      num
      (if (string=? (first answer)
                    (first test_set))
          (accuracy-acc (rest answer) (rest test_set) (+ num 1))
          (accuracy-acc (rest answer) (rest test_set) num))))
  
;; get the random guess rate based on how many symbols there were.
(define (get-random-guess-rate)
  (/ 1 (hash-count FREQUENCIES)))



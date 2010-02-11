(#%require scheme)

(require 2htdp/batch-io)

;; Parse a file and return a list of strings.
(define (parse-file filename)
  (filter (lambda (x) (not (= (string-length x) 0)))
          (regexp-split "[,(\r)(\n)]" (read-file filename))))


;; _count-occurences  list(string) : changes FREQUENCIES
(define (count-occurences training_set (freq (make-hash)))
  (if (empty? training_set)
      freq
      (begin
        (let ((curr (first training_set)))
          (if (hash-has-key? freq curr)
              (hash-set! freq curr (+ (hash-ref freq curr) 1))
              (hash-set! freq curr 1)))
        (count-occurences (rest training_set) freq))))


;; Get the most common symbol on the FREQUENCIES set.
(define (get-most-common freq)
  (let ((firstIndex (hash-iterate-first freq)))
    (hash-iterate-key freq (get-most-common-index (hash-iterate-value freq firstIndex) 
                                             firstIndex
                                             firstIndex
                                             freq))))

(define (get-most-common-index best index bestIndex freq)
      (let ((curr (hash-iterate-value freq index)))
        (if (false? (hash-iterate-next freq index))
            (if (> curr best)
                index
                bestIndex) 
            (if (> curr best)
                (get-most-common-index curr (hash-iterate-next freq index) index freq)
                (get-most-common-index best (hash-iterate-next freq index) bestIndex freq)))))

;; Majority class.  Just use the most common occurence.
(define (majority-class test_set freq)
  (if (empty? test_set)
      empty
      (cons (get-most-common freq) (majority-class (rest test_set) freq))))

;; Get the accuracy of the answer set based on the test set.
(define (accuracy answer test_set)
  (/ (_accuracy answer test_set 0)
     (length answer)))

(define (_accuracy answer test_set num)
  (if (empty? answer)
      num
      (if (string=? (first answer)
                    (first test_set))
          (_accuracy (rest answer) (rest test_set) (+ num 1))
          (_accuracy (rest answer) (rest test_set) num))))
  
;; get the random guess rate based on how many symbols there were.
(define (get-random-guess-rate freq)
  (/ 1 (hash-count freq)))



(#%require scheme)

(require 2htdp/batch-io)

;; Parse a file and return a list of strings.
;; parse-file: string -> list[string]
(define (parse-file filename)
  (filter (lambda (x) (not (= (string-length x) 0)))
          (regexp-split "[,(\r)(\n)]" (read-file filename))))

;; Global for frequencies
(define FREQUENCIES (make-hash))

;; Update frequencies list from a sequence
;; count-occurences  list(string) : changes FREQUENCIES
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
    (hash-iterate-key FREQUENCIES (_get-most-common (hash-iterate-value FREQUENCIES firstIndex) 
                                                       firstIndex
                                                       firstIndex))))

(define (_get-most-common best index bestIndex)
      (let ((curr (hash-iterate-value FREQUENCIES index)))
        (if (false? (hash-iterate-next FREQUENCIES index))
            (if (> curr best)
                index
                bestIndex) 
            (if (> curr best)
                (_get-most-common curr (hash-iterate-next FREQUENCIES index) index)
                (_get-most-common best (hash-iterate-next FREQUENCIES index) bestIndex)))))

;; Majority class.  Just use the most common occurence.
;; majority-class : list[string] -> list[string]
(define (majority-class test_set)
  (if (empty? test_set)
      empty
      (cons (get-most-common) (majority-class (rest test_set)))))

;; Get the accuracy of the answer set based on the test set.
;; accuracy : list[string] list[string] -> number
(define (accuracy answer test_set)
  (/ (num-correct-answers answer test_set)
     (length answer)))

;; Get the number of correct answers
;; num-correct-answers : list[string] list[string] -> number
(define (num-correct-answers answer test_set (num 0))
  (if (empty? answer)
      num
      (if (string=? (first answer)
                    (first test_set))
          (num-correct-answers (rest answer) (rest test_set) (+ num 1))
          (num-correct-answers (rest answer) (rest test_set) num))))

;; Get the random guess rate based on how many symbols there were.
;; get-random-guess-rate :  void -> number
(define (get-random-guess-rate)
  (/ 1 (hash-count FREQUENCIES)))


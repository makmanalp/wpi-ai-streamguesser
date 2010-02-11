(#%require scheme)

(require 2htdp/batch-io)

;; Parse a file and return a list of strings.
(define (parse-file filename)
  (filter (lambda (x) (not (= (string-length x) 0)))
          (regexp-split "[,(\r)(\n)]" (read-file filename))))

(define TRAINING_SET (parse-file "train.txt"))
(define TEST_SET (parse-file "test.txt"))

(define FREQUENCIES (make-hash))

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


;; Count occurences
(count-occurences TRAINING_SET)

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

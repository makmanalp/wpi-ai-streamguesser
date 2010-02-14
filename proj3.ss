(#%require scheme)

(require 2htdp/batch-io)

;; Parse a file and return a list of strings.
;; parse-file: string -> list[string]
(define (parse-file filename)
  (filter (lambda (x) (not (string=? x "")))
          (regexp-split ",|\r?\n|\r" (read-file filename))))

;; Get's the nth item of a list
(define (get-nth alist n)
  (if (empty? alist)
      false
      (if (= n 0)
          (first alist)
          (get-nth (rest alist) (- n 1)))))

;; Update frequencies list from a sequence
;; count-occurences : list(string) -> hash
(define (count-occurences training-set chain-length (prev empty) (freq (make-hash)))
  (if (empty? training-set)
      freq
      (begin
        (let ((curr (first training-set))
              (prev-chain (string-join prev "")))
          (add-to-hash freq prev-chain curr)
          (if (= chain-length 1)
              (count-occurences (rest training-set) chain-length empty freq)
              (if (> (length prev) (- chain-length 2))
                  (count-occurences (rest training-set) chain-length (append (rest prev) (list curr)) freq)
                  (count-occurences (rest training-set) chain-length (append prev (list curr)) freq)))))))


(define (add-to-hash freq prev-chain curr)
  (if (hash-has-key? freq prev-chain)
      (begin
        (let ((prev-hash (hash-ref freq prev-chain)))
          (if (hash-has-key? prev-hash curr)
              (hash-set! prev-hash curr (+ (hash-ref prev-hash curr) 1))
              (hash-set! prev-hash curr 1))))
      (begin 
        (hash-set! freq prev-chain (make-hash))
        (hash-set! (hash-ref freq prev-chain) curr 1))))

;(define (count-occurences training-set (freq (make-hash)))
;  (if (empty? training-set)
;      freq
;      (begin
;        (let ((curr (first training-set)))
;          (if (hash-has-key? freq curr)
;              (hash-set! freq curr (+ (hash-ref freq curr) 1))
;              (hash-set! freq curr 1)))
;        (count-occurences (rest training-set) freq))))



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

(define (make-chains n train)
  (if (= n 0)
      empty
      (cons (count-occurences train n)
            (make-chains (- n 1) train))))

;;
(define (run train test n)
  (let ((chains (reverse (make-chains n train))))
    (predict-ngram test n chains)))

;; Use chains of n.
(define (predict-ngram test-set n chains (prev empty))
  (if (empty? test-set)
      empty
      (let  ;; This is the guess!
          ((guess (generate-guess prev chains)))
        ;; Update the model.
        (let ((curr (first test-set)))
          (add-to-model prev chains curr)
          (if (= n 1)
              (cons guess (predict-ngram (rest test-set) n chains empty))
              (if (> (length prev) (- n 2))
                  (cons guess (predict-ngram (rest test-set) n chains (append (rest prev) (list curr))))
                  (cons guess (predict-ngram (rest test-set) n chains (append prev (list curr))))))))))
        

(define (add-to-model prev chains curr)
  (if (empty? chains)
      empty
      (begin
        (add-to-hash (last chains) (string-join prev "") curr)
        (if (empty? prev)
            (add-to-model empty
                          (drop-right chains 1)
                          curr)
            (add-to-model (rest prev)
                          (drop-right chains 1)
                          curr)))))

(define (generate-guess prev chains)
    (if (empty? chains)
      empty
      (let ((prev-chain (string-join prev ""))
            (longest-chain (last chains)))
        (if (hash-has-key? longest-chain prev-chain)
            (get-most-common (hash-ref longest-chain prev-chain))
            (if (empty? prev)
                (generate-guess empty
                                (drop-right chains 1))
                (generate-guess (drop-right prev 1)
                                (drop-right chains 1)))))))

;;; Majority class.  Just use the most common occurence.
;;; majority-class : list[string] -> list[string]
;(define (majority-class test-set freq)
;  (if (empty? test-set)
;      empty
;      (cons (get-most-common freq) (majority-class (rest test-set) freq))))

;; Get the accuracy of the answer set based on the test set.
;; accuracy : list[string] list[string] -> number
(define (accuracy answer test-set)
  (* 1.0 (/ (num-correct-answers answer test-set 0)
            (length answer))))

;; Get the number of correct answers
;; num-correct-answers : list[string] list[string] -> number
(define (num-correct-answers answer test-set (num 0))
  (if (empty? answer)
      num
      (if (string=? (first answer)
                    (first test-set))
          (num-correct-answers (rest answer) (rest test-set) (+ num 1))
          (num-correct-answers (rest answer) (rest test-set) num))))

;; Get the random guess rate based on how many symbols there were.
;; get-random-guess-rate :  void -> number
(define (get-random-guess-rate freq)
  (/ 1.0 (hash-count (hash-ref (first freq) ""))))


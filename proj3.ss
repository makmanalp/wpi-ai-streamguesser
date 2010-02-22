(#%require scheme)

(require 2htdp/batch-io)


;; ========= Utility Functions ================

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

;; Add value curr to hash freq under key prev-chain
;; add-to-hash : hash list(string) string
;; effect: hash has been modified.
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


;; Get the most common symbol in freq
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


;; Make chains of length 1 to n from train
;; make-chains : number list(string) -> list(hash)
(define (make-chains n train)
  (if (= n 0)
      empty
      (cons (count-occurences train n)
            (make-chains (- n 1) train))))

;; Runs the ngram models
(define (run train test n (peek false))
  (set! NUM_READ 0)
  (set! PEEK-FREQ false)
  (let ((chains (reverse (make-chains n (parse-file train)))))
    (set! NUM_READ (length (parse-file train)))
    (let ((ans (predict-ngram (parse-file test) n chains peek)))
      (printf "Accuracy: ~a~n" (accuracy ans (parse-file test) peek))
      (printf "Guess: ~a~n" ans))))

(define (run-weighting train test n)
  (printf "Training a model...")
  (letrec ((train-data (parse-file train))
           (first-half (drop-right train-data (/ (length train-data) 2)))
           (second-half (take-right train-data (/ (length train-data) 2)))
           (weights (crunch 100 1000 n first-half second-half))
           (ans (run-ensemble train-data (parse-file test) n weights)))
    (printf "Weights: ~a~n" weights)
    (printf "Guess: ~a~n" (first ans))
    (printf "Accuracy: ~a~n" (second ans))))

(define PEEK-FREQ false)

(define (remove-from-chains letter chains)
  (if (empty? chains)
      empty
      (begin
        (hash-map (first chains) (lambda (k v) (hash-set! v letter -1)))
        (remove-from-chains letter (rest chains)))))

;; Use chains of n.
(define (predict-ngram test-set n chains (peek false) (prev empty))
  (if peek
      (set! PEEK-FREQ (generate-frequencies test-set))
      peek)
  (if (hash? PEEK-FREQ)
      (hash-map PEEK-FREQ (lambda (k v) (if (= v 0)
                                            (remove-from-chains k chains)
                                            false)))
      false)
  (if (empty? test-set)
      empty
      (let  ;; This is the guess!
          ((guess (generate-guess prev chains)))
        ;; Update the model.
        (let ((curr (first test-set)))
          (if (hash? PEEK-FREQ)
              (hash-set! PEEK-FREQ curr (- (hash-ref PEEK-FREQ curr) 1))
              false)
          (set! NUM_READ (+ NUM_READ 1))
          (add-to-model prev chains curr)
          (if (= n 1)
              (cons guess (predict-ngram (rest test-set) n chains false empty))
              (if (> (length prev) (- n 2))
                  (cons guess (predict-ngram (rest test-set) n chains false (append (rest prev) (list curr))))
                  (cons guess (predict-ngram (rest test-set) n chains false (append prev (list curr))))))))))


;; Use predict ensemble to compare weighted probabilities with different chain lengths.
(define (predict-ensemble test-set n chains weights (prev empty))
  (if (empty? test-set)
      empty
      (let  ;; This is the guess!
          ((guess (generate-ensemble-guess prev chains weights)))
        ;; Update the model.
        (let ((curr (first test-set)))
          (set! NUM_READ (+ NUM_READ 1))
          (add-to-model prev chains curr)
          (if (= n 1)
              (cons guess (predict-ensemble (rest test-set) n chains weights empty))
              (if (> (length prev) (- n 2))
                  (cons guess (predict-ensemble (rest test-set) n chains weights (append (rest prev) (list curr))))
                  (cons guess (predict-ensemble (rest test-set) n chains weights (append prev (list curr))))))))))


;; Adds curr under prev in all of the chains in chains.
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

;; Generate a guess by picking the most likely thing in chains starting with prev.
;; If you can't find it, use the next smaller chain.
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

;; Find the most probable giving the weights in all of the chains.
(define (generate-ensemble-guess prev chains weights)
  (most-probable (map (lambda (freq-tuple weight) (list (first freq-tuple) (* weight (/ (second freq-tuple) (third freq-tuple)))))
                      (reverse (get-most-common-for-each chains prev))
                      weights)))


;; Sum all of the values in the hash;
;; hash-sum : hash -> number
(define (hash-sum hash)
  (local ((define ans 0))
    (hash-for-each hash
                   (lambda (k v) (set! ans (+ ans v))))
    ans))

(define (most-probable prob (curr -inf.0) (sym ""))
  (if (empty? prob)
      sym
      (let ((curr-sym (first prob)))
        (if (> (second curr-sym) curr)
            (most-probable (rest prob) (second curr-sym) (first curr-sym))
            (most-probable (rest prob) curr sym)))))


(define (get-most-common-for-each chains prev)
  (if (empty? chains)
      empty
      (letrec ((prev-chain (string-join prev ""))
               (longest-chain (last chains)))
        (if (hash-has-key? longest-chain prev-chain)
            (letrec ((ans (get-most-common (hash-ref longest-chain prev-chain)))
                     (occurences (hash-ref (hash-ref longest-chain prev-chain) ans)))
              (cons (list ans occurences (hash-sum (hash-ref longest-chain prev-chain))) (if (empty? prev)
                                                                                             (get-most-common-for-each  (drop-right chains 1)
                                                                                                                        empty)
                                                                                             (get-most-common-for-each (drop-right chains 1)
                                                                                                                       (drop-right prev 1)))))
            (cons (list "" -1 1) (if (empty? prev)
                                     (get-most-common-for-each  (drop-right chains 1)
                                                                empty)
                                     (get-most-common-for-each (drop-right chains 1)
                                                               (drop-right prev 1))))))))



;; Majority class.  Just use the most common occurence.
;; majority-class : list[string] -> list[string]
(define (majority-class test-set freq)
  (if (empty? test-set)
      empty
      (cons (get-most-common freq) (majority-class (rest test-set) freq))))

;; Get the accuracy of the answer set based on the test set.
;; accuracy : list[string] list[string] -> number
(define (accuracy answer test-set peek)
  (if peek
      (* 1.0 (/ (- (num-correct-answers answer test-set 0) 5)
                (length answer)))      
      (* 1.0 (/ (num-correct-answers answer test-set 0)
                (length answer)))))
                                       

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

(define (fact x)
  (if (= x 0)
      1
      (* x (fact (- x 1)))))

(define (choose n r)
  (/ (fact n) (* (fact r) (fact (- n r)))))

(define (perm n r)
  (/ (fact n) (fact (- n r))))

(define (gen-inverse n choices func)
  (if (= n 0)
      empty
      (cons (/ 1 (func choices n)) (gen-inverse (- n 1) choices func))))


(define (crunch n max len train test (best empty) (acc 0))
   (if (= n 0)
      best
      (letrec ((tuple (rand-list len max))
               (newacc (second (run-ensemble train test len tuple))))
        (if (> newacc acc)
            (crunch (- n 1) max len train test tuple newacc)
            (crunch (- n 1) max len train test best acc)))))

(define (rand-list len max)
  (if (= len 0)
      empty
      (cons (random max) (rand-list (- len 1) max))))

(define (generate-frequencies strs)
  (hash-ref (first (make-chains 1 strs)) ""))

;; pretty-print a hash (TODO: fix this when i'm not lazy)
(define (hash-print h (spaces 0))
  (hash-map
    h
    (lambda (k v)
      (printf "~a~n" k)
      (cond
        [(hash? v)
         (hash-print v (+ 1 spaces))]
        [else
          (printf "~a~n" v)]))))


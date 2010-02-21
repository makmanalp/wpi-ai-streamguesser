(#%require scheme)

(include "proj3.ss")

(require test-engine/scheme-tests)

;;; TESTS
;(define TRAINING_SET (parse-file "train.txt"))
;(define TEST_SET (parse-file "test.txt"))
;
;;; Count occurences
;(define FREQUENCIES (count-occurences TRAINING_SET 1))
;
;(printf "WARMUP~n")
;(printf "-------------~n")
;(let ((majority-answer (majority-class TEST_SET FREQUENCIES)))
;  (printf "Training Set: ~a~n" (string-join TRAINING_SET ","))
;  (printf "Majority Algorithm: ~a~n" (string-join majority-answer ","))
;  (printf "Test Set: ~a~n" (string-join TEST_SET ","))
;  (printf "Accuracy: ~a~n" (accuracy majority-answer TEST_SET))
;  (printf "Random Guess Rate: ~a~n" (get-random-guess-rate FREQUENCIES)))

;;;;;;;;;;;;;;;;
;; TEST CASES ;;
;;;;;;;;;;;;;;;;

;; File parsing
(check-expect (parse-file "parse_test.txt") (list "hello" "my" "name" "is" "thomas" "liu"))

;; Getting the nth value in a list.
(check-expect (get-nth (list 1 2 3 4) 0) 1)
(check-expect (get-nth (list 1 2 3 4) 1) 2)
(check-expect (get-nth (list 1 2 3 4) 3) 4)
(check-expect (get-nth (list 1 2 3 4) 5) false)

;; Updating the frequencies list from a sequence.
(count-occurences (list "a" "b" "a" "a" "b") 2)
"should return #hash((b . #hash((a . 1))) (a . #hash((b . 2) (a . 1))) ( . #hash((a . 1))))"

;; Getting the most common symbol in freq.
(define test-hash (make-hash))
(add-to-hash test-hash "" "a")
(check-expect (get-most-common (hash-ref test-hash "")) "a")
(add-to-hash test-hash "" "b")
(add-to-hash test-hash "" "b")
(check-expect (get-most-common (hash-ref test-hash "")) "b")
(add-to-hash test-hash "" "a")
(add-to-hash test-hash "" "a")
(check-expect (get-most-common (hash-ref test-hash "")) "a")

;; Hash-sum
(set! test-hash (make-hash))
(hash-set! test-hash "a" 1)
(hash-set! test-hash "b" 12)
(hash-set! test-hash "c" 144)
(hash-set! test-hash "d" 16)
(hash-set! test-hash "e" 11)
(check-expect (hash-sum test-hash) 184)

;; Get the most probable, list of tuples where each tuple is (sym, prob)
(check-expect (most-probable (list (list 'x 1) (list 'y 2))) 'y)
(check-expect (most-probable (list (list 'x -1) (list 'y 2))) 'y)
(check-expect (most-probable (list (list 'x -1) (list 'y 0))) 'y)
(check-expect (most-probable (list (list 'x 1) (list 'y 2) (list 'z 4))) 'z)


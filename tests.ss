(include "proj3.ss")

;; TESTS
(define TRAINING_SET (parse-file "train.txt"))
(define TEST_SET (parse-file "test.txt"))

;; Count occurences
(define FREQUENCIES (count-occurences TRAINING_SET 1))

(printf "WARMUP~n")
(printf "-------------~n")
(let ((majority-answer (majority-class TEST_SET FREQUENCIES)))
  (printf "Training Set: ~a~n" (string-join TRAINING_SET ","))
  (printf "Majority Algorithm: ~a~n" (string-join majority-answer ","))
  (printf "Test Set: ~a~n" (string-join TEST_SET ","))
  (printf "Accuracy: ~a~n" (accuracy majority-answer TEST_SET))
  (printf "Random Guess Rate: ~a~n" (get-random-guess-rate FREQUENCIES)))
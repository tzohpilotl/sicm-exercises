;; Excercise 1.9
;; Each of the following two procedures defines a method for adding two positive integers in terms
;; of procedures `inc`, which increments its arguments by 1, and `dec`, which decrementes its
;; argument by 1.
;; Using the substitution model, illustrate the process generated by each procedure in evaluating (+
;; 4 5). Are these processes iterative or recursive?

(define (only-dec-first-add a b)
  (if (= a 0)
      b
      (inc (only-dec-first-add (dec a) b))))

(only-dec-a-add 4 5)
(inc (only-dec-a-add 3 5))
(inc (inc (only-dec-a-add 2 5)))
(inc (inc (inc (only-dec-a-add 1 5))))
(inc (inc (inc (inc (only-dec-a-add 0 5)))))
(inc (inc (inc (inc (5)))))
(inc (inc (inc (6))))
(inc (inc (7)))
(inc (8))
9

;; `only-dec-first-add` procedure produces a recursive process because it keeps sending recursive
;; calls to other functions (`inc`).

(define (dec-both-add a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(dec-both-add 4 5)
(dec-both-add (dec 4) (inc 5))
(dec-both-add 3 6)
(dec-both-add (dec 3) (inc 6))
(dec-both-add 2 7)
(dec-both-add (dec 2) (inc 7))
(dec-both-add 1 8)
(dec-both-add (dec 1) (inc 8))
(dec-both-add 0 9)
9

;; `dec-both-add` procedure produces an iterative process because the top function has all the data
;; it needs when called (because `dec` and `inc` are not recursive).



(define (sqrt x)
  (square-root 1.0 x))

(define (square-root guess x)
  (if (good-enough? guess x)
      guess
      (square-root (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;; Exercise 1.7
;; The new if implementation works well, the same as
;; the built-in one.
(define (new-sqrt x)
  (new-square-root 1.0 x))

(define (new-square-root guess x)
  (new-if (good-enough? guess x)
      guess
      (new-square-root (improve guess x) x)))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

;; Exercise 1.8
(define (cube-root guess x)
  (if (good-enough? guess x)
      guess
      (cube-root (cube-improve guess x) x)))

(define (cube-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

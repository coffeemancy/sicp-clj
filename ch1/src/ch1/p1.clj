(ns ch1.p1)

;; EXERCISE 1.1
;; What is the result for each expression?
10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6) ) ; (+ 8 -2) = 6
(def a 3) ; 3
(def b (+ a 1)) ; 4
(+ a b (* a b)) ; 19
(= a b) ; false
(if (and (> b a) (< b (* a b))) b a) ; 4
(cond
  (= a 4) 6
  (= b 4) (+ 6 7 a)
  :else 25) ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond
    (> a b) a
    (< a b) b
    :else -1)
  (+ a 1)) ; 16


;; EXERCISE 1.2
;; Translate into prefix form: (5+4+(2-(3-(6+4/5))))/(3*(6-2)*(2-7)) = -37/150
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;; EXERCISE 1.3
;; Define a procedure taking three numbers as aruments, returns sum of
;;   squares of two larger numbers.
(defn square [x]
  (* x x))

(defn sum-two-largest-squared [x y z]
  "returns sum of two largest of three numbers, each squared";;
  (cond
    (and (>= x z) (>= y z)) (+ (square x) (square y))
    (and (>= x y) (>= z y)) (+ (square x) (square z))
    :else (+ (square y) (square z))))

(sum-two-largest-squared 1 2 3) ; 13
(sum-two-largest-squared -2 -1 1) ; 2

;; EXERCISE 1.4
;; Describe the behavior of this procedure.
(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))

;; The procedure chooses whether to add or subtract b to/from a based
;;   on the polarity of b. So, it's affectively "a plus the absolute
;;   value of b".
(a-plus-abs-b 1 2) ; 3
(a-plus-abs-b 1 -2) ; 3

;; EXERCISE 1.5
;; With the following procedures and evaluatation,
;;   describe the behavior when evaluating with applicative-order.
(defn p [] (p))
(defn test [x y]
  (if (= x 0) 0 y))
;; (test 0 (p)) ; STACK OVERFLOW (applicative-order)

;; With applicative-order, there is a stack overflow due to infinite
;;   recursion trying to evaluate p. With normal-order, it would return
;;   0 before (instead of) evaluating p.

;; EXERCISE 1.6
;; What happens when using new-if to compute square roots?
(defn abs [x]
  (if (< x 0) (- x) x))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn new-if [predicate then-clause else-clause]
  (cond predicate then-clause
        :else else-clause))
(new-if ( = 2 3) 0 5)
(new-if (= 1 1) 0 5)
(defn sqrt-iter [guess x]
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

;; (sqrt-iter 1.0 9) ; STACK OVERFLOW

;; The recursive call is always evaluated before passing to new-if
;;   causing an infinite recursion.

;; EXERCISE 1.7
;; Show how good-enough? fails for very large numbers.
;;   Design a square-root procedure that has an end test watching how guess
;;   changes across iterations and stops when change is a very small
;;   fraction of guess.
;;   Does this work better for small and large numbers?
(defn sqrt-iter [guess x]
  (letfn [(good-enough? [guess x]
            (< (abs (- (square guess) x)) 0.001))]
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))))

(let [s 1000000009.0]
  (- (Math/sqrt s) (sqrt-iter 1.0 s)))

(let [s 0.000000000001000000009]
  (- (Math/sqrt s) (sqrt-iter 1.0 s)))

;; The errors are small on the JVM with Clojure. However, the expectations
;;   are that as the numbers get larger, comparing it to 0.001 has errors
;;   introduced by rounding/loss of precision. However, there are gross
;;   errors for very small numbers because 0.001 is very large comparatively.

(defn sqrt-iter [guess x]
  (letfn [(good-enough? [guess last-guess]
            (< (abs (- guess last-guess)) (/ guess 10000)))
          (sqrt-iterator [guess x last-guess]
            (if (good-enough? guess last-guess)
                guess
                (sqrt-iterator (improve guess x) x guess)))]
    (sqrt-iterator guess x 0.0)))

(let [s 1000000009.0]
  (- (Math/sqrt s) (sqrt-iter 1.0 s)))

(let [s 0.000000000001000000009]
  (- (Math/sqrt s) (sqrt-iter 1.0 s)))

;; This significantly improves accuracy for very small numbers because 0.001
;;   is grossly large for very small numbers.

;; EXERCISE 1.8
;; Implement a cube root method using Newton's Method:
;;   (x/(y^2)+2*y)/3
;;   where y is the guess for the cube-root of x
(defn cube-root-iter [guess x]
  (letfn [(good-enough? [guess last-guess]
            (< (abs (- guess last-guess)) (/ guess 10000)))
          (improve [guess x]
            (/ (+ (/ x (square guess)) (* 2 guess)) 3))
          (cr-iterator [guess x last-guess]
            (if (good-enough? guess last-guess)
                guess
                (cr-iterator (improve guess x) x guess)))]
    (cr-iterator guess x 0.0)))

(let [c (* 300 300 300)]
  (- (Math/pow c (/ 1 3)) (cube-root-iter 1.0 c)))

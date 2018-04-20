;;; 1.1 The Elements of Programming

;; 1.1.4 Compound Procedures

(defun square (x)
  (* x x))

(square 4) ; 16

(defun sum-of-squares (x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4) ; 25

;; 1.1.5 The Substitution Model for Procedure Application

; applicative-order evaluation (lisp uses)
;   * evaluate args, and substitute return values 
; normal-order evalutation
;   * substitute subroutine defs, then evaluate.

;; 1.1.6

(defun abs (x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

(abs -4)
(abs 4)
(abs 0)

(defun abs (x)
  (if (< x 0) (- x)
    x))

(defun >= (x y)
  (or (> x y) (= x y)))

(defun >= (x y)
  (not (< x y)))

; exercise 1.2

(/ 
 (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
 (* 3 (- 6 2) (- 2 7)))

; exercise 1.3

(defun sum-of-squares-of-max (a b c)
  (cond ((> a b)
	 (if (> b c)
	     (sum-of-squares a b)
	   (sum-of-squares a c)))
	((> a c) (sum-of-squares a b))
	((< a c) (sum-of-squares b c))
	(t (sum-of-squares a b))))

(sum-of-squares-of-max 2 3 4) ; 25
(sum-of-squares-of-max 3 4 2) ; 25
(sum-of-squares-of-max 4 3 2) ; 25
(sum-of-squares-of-max 4 2 3) ; 25
(sum-of-squares-of-max 3 2 4) ; 25
(sum-of-squares-of-max 2 2 2) ; 8

; exercise 1.4
; funcall/#' required here, but not in sicp scheme.

(defun a-plus-abs-b (a b)
  (funcall (if (> b 0) #'+ #'-) a b))

(a-plus-abs-b 3 -3) ; 6
(a-plus-abs-b 3 0)  ; 3
(a-plus-abs-b 3 3)  ; 6

;; 1.1.7 Example: Square Roots by Newton's Method

; sqrt(x) = the y such that y >= 0 and y^2 = x.
; 
; as pseudo-lisp
;
; (define (sqrt x)
;  (the y (and (>= y 0)
;              (= (square y) x))))
;
; but...this begs the question.

(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
      guess
    (sqrt-iter (improve guess x) x)))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun sqrt (x)
  (sqrt-iter 1.0 x))

(sqrt 36) ; 6.00000...

; exercise 1.8 
; newton's method for cube roots
; improve guess: (x/y^2 + 2y)/3

(defun cube (x)
  (* x x x))

(defun cubr-iter (guess x)
  (if (good-enough? guess x)
      guess
    (cubr-iter (improve guess x) x)))

(defun improve (guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(defun good-enough? (guess x)
  (< (abs (- (cube guess) x)) 0.001))

(defun cubr (x)
  (cubr-iter 1.0 x))

(cubr 27) ; 3.000...

;;; 1.2 Procedures andthe Processes They Generate

;; 1.2.1 Linear Recursion and Iteration

; linear recursive version
(defun factorial (n)
  (if (= n 1)
      1
    (* n (factorial (- n 1)))))

(factorial 2) ; 2
(factorial 3) ; 6

; linear iterative version
(defun factorial (n)
  (fact-iter 1 1 n))

(defun fact-iter (product counter max-count)
  (if (> counter max-count)
      product
    (fact-iter (* counter product)
	       (+ counter 1)
	       max-count)))

; a recursive procedure may yield a linear process,
; as fact-iter demonstrates. blah blah tail recursion.

; exercise 1.9

(defun plus (a b)
  (if (= a 0)
      b
    (plus (+ (dec a) b)))) ; recursive process

(defun plus (a b)
  (if (= a 0)
      b
    (plus (dec a) (inc b)))) ; iterative process

; exercise 1.10

; The Ackermann function is the simplest example of a well-defined total
; function which is computable but not primitive recursive.
; http://mathworld.wolfram.com/AckermannFunction.html

(defun A (x y)
  "Ackermann's function."
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(t (A (- x 1)
	      (A x (- y 1))))))

(A 1 10) ; 1024
(A 2 4)  ; 65536
(A 3 3)  ; 65536

(defun f (n)
  "computes 2n"
  (A 0 n))

(defun g (n)
  "computes 2^n"
  (A 1 n))


(g 0) ; 0
(g 1) ; 2
(g 2) ; 4
(g 3) ; 8
(g 4) ; 16
(g 5) ; 32
(g 6) ; 64

(defun h (n)
  "computes 2^^n"
  (A 2 n))

(h 0) ; 0 = 2^0
(h 1) ; 2 = 2^1
(h 2) ; 4 = 2^2
(h 3) ; 16 = 2^(2^2) = 2^4
(h 4) ; 65536 = 2^(2^(2^2) = 2^16

;; 1.2.2 Tree Recursion

;          { 0                   if n = 0,
; Fib(n) = { 1                   if n =1 ,
;          { Fib(n-1) + Fib(n-2) otherwise.

(defun fib (n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(t (+ (fib (- n 1))
	      (fib (- n 1)))))) ; grows exponentially with n :)
(defun fib (n)
  (fib-iter 1 0 n)) ; re-cast as an iteration with 3 state variables.

(defun fib-iter (a b count)
  (if (= count 0)
      b
    (fib-iter (+ a b) a (- count 1))))

; counting change

(defun count-change (amount)
  (cc amount 5))

(defun cc (amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0)
	     (= kinds-of-coins 0))
	 0)
	(t 
	 (+ (cc amount (- kinds-of-coins 1))
	    (cc (- amount (first-denomination
			   kinds-of-coins))
		kinds-of-coins)))))

(defun first-denomination (kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(count-change 5) ; 2
(setq max-lisp-eval-depth 100000) ; default is 500
(count-change 100) ; 292

; exercise 1.11

;        { n                          if n <  3
; f(n) = { f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3
;

(defun f (n)
  "literal / tree recursive version."
  (if (< n 3) n
    (+ (f (- n 1)) 
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(f 3) ; 4
(f 4) ; 11
(f 5) ; 25


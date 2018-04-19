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


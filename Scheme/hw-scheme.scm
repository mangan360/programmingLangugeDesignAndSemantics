#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stephen Mangan 14434682

(define (zipwith f . ls)
  (apply map f ls))



(define (poly-eval poly x)
  (apply + (zipwith (lambda (a n) (* a (expt x n)))
                    poly
                    (range (length poly)))))




;; Returns a list of 'n' zeros.
(define (zeros n)
  (map (lambda (x) 0) (range n)))

;; Pads p1 to the length of p2 and returns p1.
(define (pad-poly p1 p2)
  (cond
    ((< (length p1) (length p2))
      (append p1 (zeros (- (length p2) (length p1)))))
    (else p1)))


;; Adds polynomials (with padding).
(define (poly-add p1 p2)
  (zipwith + (pad-poly p1 p2) (pad-poly p2 p1)))


;; Multiplies a polynomial by a constant.
(define (const-poly-mul c p)
  (map (lambda (x) (* c x)) p))

;; Note that f(x) g(x) = (a0 + x f1(x)) g(x)
;;                     = a0 g(x) + x (f1(x) g(x))
;; - Multiplying by x is a right-shift (cons 0)
;; - f1 is f left-shifted (cdr)
;; - Multiplying by constant is done above
(define (poly-mul p1 p2)
  (cond
    ((null? p1) '())
    ((null? (cdr p1)) (const-poly-mul (car p1) p2))
    (else (poly-add (const-poly-mul (car p1) p2)
                    (cons 0 (poly-mul (cdr p1) p2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DERIVATIVES AND INTEGRALS


;; (map-apply f '((1 2) (3 4)))
;;   => '((f 1 2) (f 3 4))
(define (map-apply f ls)
  (map (lambda (l) (apply f l)) ls))

;; (zip '(1 2 3) '(4 5 6))
;;   => '((1 4) (2 5) (3 6))
(define (zip . ls)
  (apply map list ls))


;; diff ~= zipwith (* ℕ) ∘ left-shift
(define (poly-diff p)
  (map-apply * (zip (cdr p) (range 1 (length p)))))


;; integral ~= zipwith (/ ℕ) ∘ right-shift
(define (poly-int p)
  (cons 0 (map-apply / (zip p (range 1 (+ 1 (length p)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Govel-poly-eval


;; Checks whether something is an atomic symbol or a list of symbols. We use
;; 'pair?' instead of 'list?' here since 'list?' excludes dotted pairs, e.g.
;; '(1 . 2)
(define (atom? x)
  (not (or (pair? x) (null? x))))

(define (govel-poly-eval s x)
  (cond
    ;; If atomic, return
    ((atom? s) s)
    ;; If the list starts with 'poly', evaluate it
    ((eq? (car s) 'poly) (poly-eval (cdr s) x))
    ;; Otherwise, dig one layer down and repeat
    (else (map (lambda (s*) (govel-poly-eval s* x)) s))))

(define (grovel-poly-eval s x)
  (cond
    ;; If atomic, return
    ((atom? s) s)
    ;; If the list starts with 'poly', evaluate it
    ((eq? (car s) 'poly) (poly-eval (cdr s) x))
    ;; Otherwise, dig one layer down and repeat
    (else (map (lambda (s*) (grovel-poly-eval s* x)) s))))








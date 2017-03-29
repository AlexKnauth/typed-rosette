#lang typed/rosette

(require turnstile/rackunit-typechecking)

(struct foo ([a : Int])
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (fprintf out "~v" (foo-a this)))])

(define-generics colorable
  [(get-color [colorable : Colorable]) -> String]
  [(set-color [colorable : Colorable] [color : String]) -> Colorable])

(struct point ([x : Float] [y : Float] [color : String])
  #:transparent
  #:methods gen:colorable
  [(define (get-color self)
     (point-color self))
   (define (set-color self color)
     (point (point-x self) (point-y self) color))])

;; Test the Point struct itself

(check-type (point 1.2 4.16 "red") : Point -> (point 1.2 4.16 "red"))
(check-type (point-x (point 1.2 4.16 "red")) : Float -> 1.2)
(check-type (point-y (point 1.2 4.16 "red")) : Float -> 4.16)

;; Test the Colorable generic interface with Point

(check-type (get-color (point 1.2 2.4 "blue")) : String -> "blue")
;; TODO: return Colorable instead of Any
(check-type (set-color (point 1.2 2.4 "blue") "green") : Any
            -> (point 1.2 2.4 "green"))


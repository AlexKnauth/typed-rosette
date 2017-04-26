#lang typed/rosette

(require turnstile/rackunit-typechecking
         typed/rosette/types
         (only-in typed/rosette/base-forms unsafe-assign-type)
         (prefix-in $ rosette))

(define integer?
  (unsafe-assign-type $integer?
                      : (Ccase->
                         (C→* [CAny] [] CBool
                              : #:+ (@ 0 : CInt) #:- (!@ 0 : CInt))
                         (C→* [Any] [] Bool
                              : #:+ (@ 0 : Int) #:- (!@ 0 : Int)))))

(define boolean?
  (unsafe-assign-type $boolean?
                      : (Ccase->
                         (C→* [CAny] [] CBool
                              : #:+ (@ 0 : CBool) #:- (!@ 0 : CBool))
                         (C→* [Any] [] Bool
                              : #:+ (@ 0 : Bool) #:- (!@ 0 : Bool)))))

(define string?
  (unsafe-assign-type $string?
                      : (C→* [CAny] [] CBool
                             : #:+ (@ 0 : CString) #:- (!@ 0 : CString))))

(define natural?
  (unsafe-assign-type $exact-nonnegative-integer?
                      : (Ccase->
                         (C→* [CAny] [] CBool
                              : #:+ (@ 0 : CNat) #:- (!@ 0 : CNat))
                         (C→* [Any] [] Bool
                              : #:+ (@ 0 : Nat) #:- (!@ 0 : Nat)))))

(define add1
  (unsafe-assign-type $add1
                      : (Ccase-> (C→ CNat CPosInt)
                                 (C→ Nat PosInt))))

(define unneg
  (unsafe-assign-type $-
                      : (Ccase-> (C→ CNegInt CPosInt)
                                 (C→ NegInt PosInt))))

(: f : (C→ CInt CNat))
(define (f x)
  (if (natural? x)
      (add1 x)
      (unneg x)))

(: f/restricted : (C→ CPosInt CNat))
(define (f/restricted x)
  (if (natural? x)
      (add1 x)
      (unneg x)))

(: f* : (C→ Int Nat))
(define (f* x)
  (if (natural? x)
      (add1 x)
      (unneg x)))

;; ---------------------------------------------------------

;; Testing type restricting behavior

(: g : (C→ (CU CNegInt CZero) CNat))
(define (g x)
  (if (natural? x)
      (ann x : CZero)
      (unneg x)))

(: g* : (C→ (Term (CU CNegInt CZero)) Nat))
(define (g* x)
  (if (natural? x)
      (ann x : Zero)
      (unneg x)))

;; ---------------------------------------------------------

;; Unions with Nothings and non-Nothings in them should not
;; be Nothing!

(typecheck-fail
 (λ ([x : (CU CNothing CString)])
   (ann x : CNothing))
 #:with-msg
 "expected CNothing, given \\(CU CNothing CString\\)")

;; ---------------------------------------------------------

;; Testing occurrence typing with case->

(: h : (Ccase-> (C→ CInt CInt)
                (C→ CString CString)))
(define (h x)
  (if (integer? x)
      (f x)
      (string-append "|" (string-append x "|"))))

(check-type (h 4) : CInt -> 5)
(check-type (h -4) : CInt -> 4)
(check-type (h "four") : CString -> "|four|")

;; ---------------------------------------------------------

;; Testing occurrence typing with or

(: f/intstr : (C→ (CU CInt CString) CInt))
(define (f/intstr x)
  (if (integer? x)
      x
      (string-length x)))

(: f/or : (C→ CAny CInt))
(define (f/or x)
  (if (or (integer? x) (string? x))
      (f/intstr x)
      0))

(check-type (f/or 5) : CInt -> 5)
(check-type (f/or "five") : CInt -> 4)
(check-type (f/or (list 1 2 3 4 5)) : CInt -> 0)

(: bool->int : (C→ Bool Int))
(define (bool->int b)
  (if b 1 0))

(: f/intbool* : (C→ (U Int Bool) Int))
(define (f/intbool* x)
  (if (integer? x)
      x
      (bool->int x)))

(: f/or* : (C→ Any Int))
(define (f/or* x)
  (if (or (integer? x) (boolean? x))
      (f/intbool* x)
      0))


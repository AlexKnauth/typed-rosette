#lang typed/rosette

(require turnstile/rackunit-typechecking
         typed/rosette/types
         (only-in typed/rosette/base-forms unsafe-assign-type)
         (prefix-in $ rosette))

(define integer?
  (unsafe-assign-type $integer?
                      : (C→* [(CU CInt CString)] [] CBool
                             : #:+ (@ 0 : CInt) #:- (@ 0 : CString))))

(define natural?
  (unsafe-assign-type $exact-nonnegative-integer?
                      : (C→* [CInt] [] CBool
                             : #:+ (@ 0 : CNat) #:- (@ 0 : CNegInt))))

(define add1
  (unsafe-assign-type $add1
                      : (C→ CNat CPosInt)))

(define unneg
  (unsafe-assign-type $-
                      : (C→ CNegInt CPosInt)))

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

;; ---------------------------------------------------------

;; Testing type restricting behavior

(: g : (C→ (CU CNegInt CZero) CNat))
(define (g x)
  (if (natural? x)
      (ann x : CZero)
      (unneg x)))

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


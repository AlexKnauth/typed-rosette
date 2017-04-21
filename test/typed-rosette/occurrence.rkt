#lang typed/rosette

(require typed/rosette/types
         (only-in typed/rosette/base-forms unsafe-assign-type)
         (prefix-in $ rosette))

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
      1 #;(unneg x)))


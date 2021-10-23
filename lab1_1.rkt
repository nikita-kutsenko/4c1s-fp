#lang racket

; лічильник глибини рекурсії
(define rec_num -1)

(define (func b p m)
  ; визначення глибини рекурсії
    (set! rec_num (+ rec_num 1))
    (if (= p 0)
        (modulo 1 m)
        (if (even? p)
            (modulo (expt (func b (round (/ p 2)) m)  2) m)
            (* b (modulo (expt (func b (round (/ p 2)) m)  2) m))
        )
    )
)

; Input b
(display "Input b: ")
(define b (read))

; Input p
(display "Input p: ")
(define p (read))

; Input m
(display "Input m: ")
(define m (read))

(printf "RESULT ::  ~a" (func b p m) )
(printf "\nrec_num ::  ~a" rec_num )













; #lang racket

; ; лічильник глибини рекурсії
; (define rec_num -1)

; (define (func b p m)
;   ; визначення глибини рекурсії
;     (set! rec_num (+ rec_num 1))

;     (if (= p 0)
;         ; TRUE | p equal to 0 
;         (printf "p=0 ::  ~a" (modulo 1 m))

;         ; FALSE | p is NOT equal to 0 
;         (if (even? p)
;             ; TRUE | p is even 
;             (printf "p is even ::  ~a" 
;                 (modulo 
;                     (expt (expt b (/ p 2)) 2) 
;                     m) )
                
;             ; FALSE | p is NOT even 
;             (printf "p is not even ::  ~a"  
;                 (modulo 
;                     (* b (expt (expt b (/ p 2)) 2)) 
;                     m) )
;         )
;     )
; )

; ; Input b
; (display "Input b: ")
; (define b (read))

; ; Input p
; (display "Input p: ")
; (define p (read))

; ; Input m
; (display "Input m: ")
; (define m (read))

; (func b p m)
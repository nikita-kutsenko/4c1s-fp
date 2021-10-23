#lang racket

; лічильник глибини рекурсії
(define rec_num -1)
(define resultlist null )

(define (getNotEven resList n)
  (set! resList (list* n resList))
  (getNums resList (sub1 n)))

(define (getNums resultWitList n)
    ; визначення глибини рекурсії
    (set! rec_num (+ rec_num 1))
    (printf "\nresultlist ::  ~a" resultWitList )

    (cond
       [(zero? n) (printf "\nRESULT ::  ~a" resultWitList )]
        ; (set! resultlist (list* 0 resultlist))
       
       [(even? n) (getNums resultWitList (sub1 n))]
       
       [else (getNotEven resultWitList n)]
     )
)

; Input b
(display "Input n: ")
(define n (read))

(getNums resultlist n)
;(printf "RESULT ::  ~a" resultlist )
(printf "\nrec_num ::  ~a" rec_num )
































; #lang racket

; ; лічильник глибини рекурсії
; (define rec_num -1)

; (define (getNums resultlist n)
;     ; визначення глибини рекурсії
;     (add1 rec_num)

;     ; (printf "\nresultlist ::  ~a" resultlist )
;     ; (printf "\nn ::  ~a" n )
;     ; (if (= n 0)
;     ;     (set! resultlist (list* resultlist 0))
;     ;     (if (even? n)
;     ;         (getNums resultlist (sub1 n))
;     ;         (
;     ;          (set! resultlist (list* resultlist n))
;     ;          (getNums resultlist (sub1 n))
;     ;         )
;     ;     )
;     ; )
; )

; ; Input b
; (display "Input n: ")
; (define n (read))
; (define resultlist n )

; (printf "RESULT ::  ~a" (getNums resultlist n) )
; (printf "\nrec_num ::  ~a" rec_num )
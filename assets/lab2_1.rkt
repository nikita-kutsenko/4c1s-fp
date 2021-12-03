#lang racket

; функція розвинення ln у ряд Тейлора
(define (ln_taylor x)
  ; ініціалізація помилки 
  (define err 0.001)
  
  ; ініціалізація функції для вирахування вираження
  ; ln(x) = (x–1) – (x–1)^2/2 + (x–1)^3/3 – (x–1)^4/4 + ...
  ; (-1 * (x-1)) / (i * i)
  (define (calculate [cx 1.0] [iter 1] [res 0.0])
    (if (> (abs cx) err)
      ; true
      (calculate (* cx (/
                        (* -1 (- x 1))
                        (* iter iter)))
        (+ iter 1) (+ res cx))
      ; false
      res)
  )
  ; init
  (calculate)
)

; функція власного ln
(define (ln_custom x)
  ; check conditions 
  (cond
    ; 0 < x < 2 --> ln(x)/ln(x-2)
    [(and (< 0 x) (< x 2))
      (/ (ln_taylor x) (ln_taylor (- x 2)))]
    
    ; 2 <= x <= 4 --> ln(x/2)
    [(and (<= 2 x) (<= x 4))
      (ln_taylor (/ x 2))]

    ; else 
    [else empty]
  )
)

; функція обчислення вбудованою функцією ln
(define (ln_default x)
  ; check conditions 
  (cond
    ; ln(x)/ln(x-2) || 0 < x < 2
    [(and (< 0 x) (< x 2))
      (/ (log x) (log (- x 2)))]

    ; ln(x/2)       || 2 <= x <= 4
    [(and (<= 2 x) (<= x 4))
      (log (/ x 2))]

    ; else 
    [else empty]
  )
)

; start <= x <= end
(define (iterate start end step func)
  (define (lop array x)
    (if (> x end)
      array
      (lop (append array (list (func x))) (+ x step)))
  )
  (lop '() start)
)

; отримання абсолютного значення N, яке є числом та дійсне число. Іначе повертає 0
(define (get_absolute n)
  (if (number? n)
    (if (real? n) ; a number
      (abs n) ; a real number
      0) ; ;not a real number
    0) ;not a number
)

; функція визначення похибки
(define (get_difference list1 list2)
  (define (minus res iter)
    (if (< iter (length list1))
      ; abs(list1 - list2)
      (minus (append res (list (abs (-
                  (get_absolute (list-ref list1 iter))
                  (get_absolute (list-ref list2 iter))))))
                (+ iter 1))
      res)
  )
  (minus '() 0)
)


; init
(define result_default (iterate -2 4 0.5 ln_default))
(define result_custom (iterate -2 4 0.5 ln_custom))
(define errors (get_difference result_default result_custom))

; result 
(printf "\nln ::  ~a \n" result_default )
(printf "\npersonal ln ::  ~a \n" result_custom )
(printf "\nerrors ::  ~a \n" errors )
(printf "\nsum of errors ::  ~a \n" (apply + errors) )
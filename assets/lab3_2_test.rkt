#lang racket


; Метод Сімпсона
(define (f_simpson sum expr a b h )
  ; Отримуємо поточне значення трапеції
  (define trap 0) 
  (set! trap (* (/ h 3) (+ (expr a) (+ (* 4 (expr (+ a h))) (expr (+ a (+ h h)))))))
  ; Перевірка поточних значень
  (if (> a b)
    sum
    (f_simpson (+ sum trap) expr (+ a (+ h h)) b h)))

(define (f_left sum expr a b h )
  ; Перевірка поточних значень
  (if (>= a b)
    sum
    (f_left (+ sum (* h (expr a))) expr (+ a h) b h)))

(define (f_right sum expr a b h )
  ; Перевірка поточних значень
  (if (>= a b)
    sum
    (f_right (+ sum (* h (expr (+ a h)))) expr (+ a h) b h)))

(define (f_mid sum expr a b h )
  ; Отримуємо поточне значення трапеції
  (define trap 0)
  (set! trap (* h (* (+ (expr a) (expr (+ a h))) 0.5)))
  ; Перевірка поточних значень
  (if (> a b)
    sum
    (f_mid (+ sum trap) expr (+ a h) b h)))


(define (init a b h)
  ; Рівняння: e^(cos(x)) * cos(2x)
  (define expr (lambda (x) (* (exp (cos x)) (cos (* 2 x)))))

  ; Отримання значень для кожного методу
  (define res_simpson (f_simpson 0 expr a b h))
  (define res_left (f_left 0 expr a b h))
  (define res_right (f_right 0 expr a b h))
  (define res_mid (f_mid 0 expr a b h))
  (printf "\n:::: Результати ::::\n:: Метод Сімпсона ::  ~a \n" res_simpson)
  (printf ":: Метод Лівих прямокутників ::  ~a \n" res_left)
  (printf ":: Метод Правих прямокутників ::  ~a \n" res_right)
  (printf ":: Метод Середніх прямокутників ::  ~a \n" res_mid)
)

(init 0 pi 0.001)
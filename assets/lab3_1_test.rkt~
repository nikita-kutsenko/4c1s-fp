#lang racket

; Нелінійне рівняння: 2 - x - ln(x) = 0
(define expr (lambda (x) (- (- 2 x) (log x))))
; Похідна від нелінійного рівняння: -(x + 1) / x = 0
(define expr_drv (lambda (x) (* -1 (/ (+ x 1) x))))


; Метод Ньютона
(define (f_newton err x)
  ; Відображення поточних значень
  (printf "\n--> err ::  ~a \n--> x ::  ~a \n" err x)
  ; Перевірка поточних значень
  (if (< (abs (expr x)) err)
    x ; Якщо значення наближене до 0 то повертаємо поточне значення х
    (f_newton err (- x (/ (expr x) (expr_drv x)))))) ; Інакше викликаємо поточну функцію використовуючи рекурсию та значення х яке зменшене
; Метод Перебору
(define (f_search err x b)
  ; Відображення поточних значень
  (printf "\n--> err ::  ~a \n--> x ::  ~a \n--> (abs (expr_drv x)) ::  ~a \n--> b ::  ~a \n" err x (abs (expr_drv x)) b)
  ; Перевірка поточних значень
  (if (and (< (abs (expr_drv x)) err) (<= x b))
    x ; Якщо значення наближене до 0 то повертаємо поточне значення х
    (f_search err (+ x err) b))) ; Інакше викликаємо поточну функцію використовуючи рекурсию та значення х яке збіьшене на 1 крок


; Головна функція
(define (init err a b)
  ; Отримання результатів методу Ньютона
  (define r_newton (f_newton err b))
  ; Отримання результатів методу Перебору
  (define r_search (f_search err a b))
  (printf "\n:::: Результати ::::\nМетод Ньютона ::  ~a \n" r_newton)
  (printf "Метод Перебору ::  ~a \n" r_search)

  ; Отримання різниці значень
  (if (number? r_search)
    (begin
      (printf "Різниця значень ::  ~a \n" (abs (- r_search r_newton))))
    (displayln "В межах для простої ітерації рішення не знайдено"))
)

; Виклик
(init 0.1 -2 1)
#lang racket

; Інінціалізація змінних
(define m -1)
(define n -1)
(define amount 0)
; Перевірка отриманих значень m та n від користувача
(define (check_M m)
  (if (> m 100)
    (printf "--> m ::  ~a \n" m)
    (getNemValue_M)))
(define (check_N n)
  (if (> n m)
    (printf "--> n ::  ~a \n" n)
    (getNemValue_N)))
(define (getNemValue_M)
  (display "ERROR :: Input m that is > 100: ")
  (set! m (read))
  (check_M m))
(define (getNemValue_N)
  (display "ERROR :: Input N that is > m: ")
  (set! n (read))
  (check_N n))


; Отримання соток
(define (getHundreds full_number)
  (floor (/ full_number 100)))
; Отримання десяток
(define (getTens full_number hundreds)
  (floor (/ (- full_number (* hundreds 100)) 10)))
; Отримання єдиниць
(define (getNums full_number hundreds tens)
  (- (- full_number (* hundreds 100)) (* tens 10)))
; Перевірка на те чи усі значення цифри різні. 1 це так. -1 це ні
(define (checkNumberOnDifference hundreds tens nums)
  (if (or (or (equal? hundreds tens) (equal? hundreds nums)) (equal? tens nums))
    -1 
    1))
; Перевірка на те чи усі значення є непарними. 1 це так. -1 це ні
(define (checkNumberOdd hundreds tens nums)
  (if (and (and (odd? hundreds) (odd? tens)) (odd? nums))
    1
    -1))
; Продовження перевірки чисел
(define (continue number n)
  ; Якщо поточне число дорівнює значенню n, 
  ; відображаємо кількість чисел та закінчуємо обрахунки.
  ; Якщо ні, продовжуємо чисел.
  (if (equal? number n)
    (printf "--> Amount of numbers between ~a and ~a , that have or odd numbers or different numbers  ::  ~a \n" m n amount)
    (check number)))



(define (check number)
  ; Отримання соток
  (define hundreds (getHundreds number))
  ; Отримання десяток
  (define tens (getTens number hundreds))
  ; Отримання єдиниць
  (define nums (getNums number hundreds tens))
  ; Відображення отриманого результату
  (printf "\n--> hundreds ::  ~a \n--> tens ::  ~a \n--> nums ::  ~a \n" hundreds tens nums)

  ; Перевірка на те чи усі значення цифри різні. 1 це так. -1 це ні
  (define isDifferent (checkNumberOnDifference hundreds tens nums))
  ; Перевірка на те чи усі значення є непарними. 1 це так. -1 це ні
  (define isOdd (checkNumberOdd hundreds tens nums))
  (printf "--> isDifferent ::  ~a \n" isDifferent)
  (printf "--> isOdd ::  ~a \n" isOdd)

  ; У разі якщо хоча б одне значення 1 то збільшуємо лічільник. Інакше залишаємо без змін
  (if (or (equal? isDifferent 1) (equal? isOdd 1))
    (set! amount (+ amount 1))
    (set! amount amount))
  ; Продовжуємо перевірку але вже з наступним числом
  (continue (+ number 1) n))
  


; Ввод значення m
(display "Input m that is > 100: ")
(set! m (read))
(check_M m)
; Ввод значення n
(display "Input n that is > m: ")
(set! n (read))
(check_N n)
; Перевірка кожного значення
(check m)
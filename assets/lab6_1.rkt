#lang racket

; скалярное произведение двух векторов = x1*x2 + y1*y2 + z1*z2
(define (scalar-multiply vec1 vec2)
  ; проход по индексам векторов
  (define (iter-multiply index sum)
    (cond ((< index (vector-length vec1)) ; пока индекс меньше длины вектора
           ; переход к следующему индексу
           (iter-multiply (+ index 1)
                          (+ sum (* (vector-ref vec1 index) (vector-ref vec2 index))))) ; добавление к сумме произведения соответствующих элементов векторов
          (else ; иначе элементы закончились
           sum)) ; вернуть скалярное произведение
    )
  (iter-multiply 0 0.0))



; расчет длины вектора
; длина вектора = sqrt(a^2 + b^2 + c^2)
(define (find-vector-length vec)
  ; проход по всем элементам вектора
  (define (iter-length index squares-sum)
    (cond ((< index (vector-length vec)) ; пока есть элементы
           ; переход к следующему элементу и добавление к сумме квадратов элемент^2
           (iter-length (+ index 1) (+ squares-sum (expt (vector-ref vec index) 2))))
          (else ; иначе элементы закончились
           (sqrt squares-sum)))) ; возвращение квадратного корня из суммы квадратов
  
  (iter-length 0 0.0))



; расчет косинуса угла между векторами
(define (find-cos-vectors vec1 vec2 scalar)
  ; cos = скалярное произведение разделить на произведение длин векторов
  (define length1 (find-vector-length vec1)) ; длина вектора 1
  (define length2 (find-vector-length vec2)) ; длина вектора 2
  (/ scalar (* length1 length2))) ; возвращение результата деления




; два вектора должны быть одинаковой длины
(define vector-1 (vector 13 25 6))
(define vector-2 (vector 8 16 7))
(printf "--> Вектор 1 :: ~a \n" vector-1)
(printf "--> Вектор 2 :: ~a \n" vector-2)

; расчет скалярного произведения 
(define scalar-mul (scalar-multiply vector-1 vector-2))
(printf "\n--> Скалярное произведение :: ~a \n" scalar-mul)

; расчет cos между векторами 
(printf "\n--> cos между векторами :: ~a \n" (find-cos-vectors vector-1 vector-2 scalar-mul))
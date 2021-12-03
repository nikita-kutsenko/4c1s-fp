; 13.1 Створити список з парною кількістю елементів, які є різними раціональними 
;       числами у вигляді дробив. Для кожної пари елементів списку виконати ділення 
;       дробив, результат якого записати у новий список. Надрукувати список, утворений 
;       з результатів ділення кожної пари. Для розв’язання задачі необхідно перевернути 
;       дріб, на який ділимо, після чого здійснити множення дробив. 

#lang racket



; создание рационального числа
(define (create-rat c z) (cons c z))



; создание списка рациональных чисел
; count - кол-во рациональных чисел
(define (create-ratinal-list count)
  (define (list-iter num denom i) ; итеративное создание пар(числитель, знаменатель)
    (cond ((= i count) ; если счетчик равен кол-ву рац чисел
           (create-rat num denom)) ; возвращается последнее рац число
          (else
           ; иначе присоединение к паре рац числа следующего числа
           (cons (create-rat num denom) (list-iter (+ num 2) (+ denom 2) (+ i 1)))))
    )
  ; вызов рекурсивной функции с начальными аргументами
  (list-iter 1 3 1))



; поиск НОД для 2 чисел методом Евклида
; НОД(a, b) = НОД(b,r) r = остаток от деления a и b
(define (my-gcd a b)
  (if (= b 0)
      a
      (my-gcd b (remainder a b))))
; числитель рационального числа
(define (numer x)
  ; поиск НОД для сокращения дроби
  (let (
    (g (my-gcd (car x) (cdr x))))
    (/ (car x) g)))
; знаменатель рационального числа
(define (denom x)
  ; поиск НОД для сокращения дроби
  (let (
    (g (my-gcd (car x) (cdr x))))
    (/ (cdr x) g)))



; функция деления двух дробей
(define (divide-two-rat rat-left rat-right)
  (printf ":: получение числителя :: ( ~a  *  ~a ) / ( ~a  *  ~a )\n" (numer rat-left) (denom rat-right) (denom rat-left) (numer rat-right))
  (create-rat (* (numer rat-left) (denom rat-right)) ; получение числителя
              (* (denom rat-left) (numer rat-right)))) ; получение знаменателя

; функция обработки деления пар рациональных чисел
(define (division-rational list-rat)
  (cond ((not (pair? (cddr list-rat))) ; когда последний элемент уже не пара, достигнут конец списка
         (divide-two-rat (car list-rat) (cdr list-rat))) ; выполняется деление последних двух рац чисел
        (else
         (cons (divide-two-rat (car list-rat) (cadr list-rat)) ; деление двух рац чисел
               (division-rational (cddr list-rat))))) ; переход к следующим двум рац числам
  )



; исходный список рациональных чисел (задаётся четное кол-во чисел)
(define start-list (create-ratinal-list 6))
(printf "--> Исходный список рац. чисел :: ~a \n" start-list)
; создание нового списка в результате деления пар рац чисел исходного списка
(define divide-list (division-rational start-list))
(printf "--> Список в результате деления :: ~a \n" divide-list)
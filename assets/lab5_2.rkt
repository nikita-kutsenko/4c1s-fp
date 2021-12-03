; 13.2 Створити список з парною кількістю елементів, які є комплексними числами в алгебраїчній формі a + ib. 
; Створити новий список, елементами якого є добутки кожної пари комплексних чисел першого списку. 
; Надрукувати новий список.

#lang racket



; ---------- Алгебраическая форма
; создать компл число (действ и мнимая части)
(define (make-real-imag a b) (cons a b))

; действительная часть компл числа
(define (my-real-part z) (car z))

; мнимая часть компл числа
(define (my-imag-part z) (cdr z))

; вывод компл число в алгебраической форме
(define (print-real-imag z)
  (printf "~a + ~ai" (my-real-part z) (my-imag-part z)))





; ---------- Полярная форма
; получение модуля из алгебраической формы
(define (modul-from-algebraic z)
  (sqrt (+ (expt (my-real-part z) 2) (expt (my-imag-part z) 2))))

; получение аргумента из алгебраической формы
(define (argument-from-algebraic z)
  (atan (my-imag-part z) (my-real-part z)))

; перевод из алгебраической в полярную форму
(define (make-polar-from-algebraic z)
  (cons (modul-from-algebraic z) (argument-from-algebraic z)))

  

; создание компл числа (модуль и аргумент)
(define (make-modul-arg mod arg) (cons mod arg))

; модуль компл числа
(define (modul z) (car z))

; аргумент компл числа
(define (argument z) (cdr z))

; вывод компл число в полярной форме
(define (print-modul-arg z)
  (printf ":: modul = ~a \t:: arg = ~a \n" (car z) (cdr z)))
; ---------------------------------------------------





; создание списка компл чисел
; count - кол-во компл чисел
(define (create-complex-list count)
  (define (list-iter real imag i) ; итеративное создание пар(действительная, мнимая)
    (cond ((= i count) ; если счетчик равен кол-ву компл чисел
           (make-real-imag real imag)) ; возвращается последнее компл число
          (else
           ; иначе присоединение к паре компл числа следующего числа
           (cons (make-real-imag real imag) (list-iter (+ real 1) (+ imag 2) (+ i 1)))))
    )
  ; вызов рекурсивной функции с начальными аргументами
  (list-iter 1 4 1)) ; a b i




; создание списка чисел в полярной форме из списка чисел в алгебраической форме
(define (create-list-polar algebraic-list)
  ; если список не пустой и car(list) не пара - значит это последняя пара списка
  (cond ((and (not (null? algebraic-list)) (not (pair? (car algebraic-list))))
         (make-polar-from-algebraic algebraic-list)) ; создать полярную форму из декартовой
        (else
         ; иначе рекурсивно соединять результаты преобразований в полярную форму
         (cons (make-polar-from-algebraic (car algebraic-list))
               (create-list-polar (cdr algebraic-list)))
         )
        )
  )




; вывод списков комплексных чисел в двух формах форме
(define (print-two-forms list-algebraic list-polar)
  ; если список не пустой и первый элемент пара
  (cond ((and (not (null? list-algebraic)) (pair? (car list-algebraic)))
         (print-real-imag (car list-algebraic)) ; вывесте компл число в алгебраической форме
         (display "\t\t\t")
         (print-modul-arg (car list-polar)) ; вывесте компл число в полярной форме
         (print-two-forms (cdr list-algebraic) (cdr list-polar)))

        (else ; иначе последняя пара в списке
         (print-real-imag list-algebraic)
         (display "\t\t\t")
         (print-modul-arg list-polar))))





; умножение двух компл чисел
(define (multiply-two-complex complex-left complex-right)
  (printf ":: умножение двух компл чисел :: ( ~a  *  ~a ) . ( ~a  *  ~a )\n" (modul complex-left) (modul complex-right) (argument complex-left) (argument complex-right))
  (make-modul-arg (* (modul complex-left) (modul complex-right))
                  (* (argument complex-left) (argument complex-right))))





; обработка списка компл чисел для умножения (умножение в порярной форме)
(define (multily-complex list-complex)
  (cond ((not (pair? (cddr list-complex)))
         (multiply-two-complex (car list-complex) (cdr list-complex)))
        (else
         (cons (multiply-two-complex (car list-complex) (cadr list-complex))
               (multily-complex (cddr list-complex))))
        )
  )





; список компл чисел в алгебраической форме (задаётся четное кол-во чисел)
(define algebraic-list (create-complex-list 4))
(printf "--> Исходный список компл. чисел :: ~a \n" algebraic-list)

; преобразование списка компл чисел в полярную форму
(define polar-list (create-list-polar algebraic-list))

; вывод чисел в двух формах
(print-two-forms algebraic-list polar-list)

; создание списка в результате умножения пар компл чисел
(define multiply-list (multily-complex polar-list))
(printf "\n--> Список в результате умножения :: ~a \n" multiply-list)
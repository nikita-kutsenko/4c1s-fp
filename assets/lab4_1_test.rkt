; (+) 13.1 Створити список п’ятикутних чисел, задавши їх кількість. П’ятикутні числа складають послідовність 1, 5, 12, 22, 35, 51,.... 
; (+) Формула для обчислення n-го п’ятикутного числа  Pn = 1/2×n×(3×n - 1). Вивести створений список. Виконати такі операції:
; (+) Замінити значення елементів на парних позиціях на їх подвоєне значення;
; (+) Знайти середнє арифметичне чисел списку
; ( ) Поділити список на два рівних по кількості елементів. Значення останнього елемента першого списку та першого елемента другого списку зробити однаковими.


#lang racket

; Отримання п’ятикутного числа
(define (get_item n) (* 0.5 n (* 3 (- n 1))))
; Створення списку з п’ятикутних чисел
(define (create_items_list items_list items_length n)
  (define item (get_item n))
  (printf "--> generating next number :: value ~a :: index ~a \n" item (- n 2))
  (if (<= n items_length)
    (create_items_list (list (append items_list item)) items_length (+ n 1))
    (flatten items_list)))


; Перевірка індексу поточного значення. якщо парний - збільшуємо значення в два рази. інакше залишаємо тим самим
(define (check_item_value example_list new_list items_length n)
  (if (odd? n)
    (* 2 (list-ref example_list n))
    (list-ref example_list n)))
; Перевірка індексу поточного значення. якщо парний - збільшуємо значення в два рази. інакше залишаємо тим самим
(define (change_even_values example_list new_list items_length n)
  (if (< n items_length)
    (let ([item (check_item_value example_list new_list items_length n)])
      (printf "--> checking with index ~a :: item value ~a\n" n item)
      (change_even_values example_list (list (append new_list item)) items_length (+ n 1))
    )
    (flatten new_list)))


; Знаходження середнього арифметичного чисел списку
(define (get_average list)
  (/ (foldr (lambda (x y) (+ x y)) 0 list) 
     (length list)))


; Отримання першого списку
(define (get_list1 list len) (list-set (list-tail list len) 0 1000))
; Отримання другого списку
(define (get_list2 list len) (list-set (take list len) (- len 1) 1000))
; Отримання відокремеленизх списків
(define (split_lists list len)
  (let (
    [list1 (get_list1 list len)]
    [list2 (get_list2 list len)]
  )
  (printf ":: first separeated list ~a :: with length ~a\n" list1 len)
  (printf ":: second separeated list ~a :: with length ~a\n\n" list2 len)))



; Головна функція
(define (init items_length)
  (displayln ":::: Результати ::::\n")

  (define items (create_items_list (list 1 5 12 22 35 51) items_length 7))
  (printf ":: items ::  ~a \n\n" items)

  (define items_changed_even (change_even_values items (list) items_length 0))
  (printf ":: items with changed even ::  ~a \n\n" items_changed_even)

  (define splited_lists (split_lists items_changed_even (/ items_length 2)))

  (define items_average (get_average items_changed_even))
  (printf ":: items average  ::  ~a \n\n" items_average)
)



(init 10)
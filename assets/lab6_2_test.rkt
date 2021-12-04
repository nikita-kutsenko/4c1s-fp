; представление списка в виде пары: (голова, конец)
; получение указателя на начало очереди
(define (head-ptr queue)
  (car queue))

; получение указателя на конец очереди
(define (tail-ptr queue)
  (cdr queue))

; изменение указателя начала очереди
(define (set-head-ptr! queue elem)
  (set-car! queue elem))

; изменение указателя конца очереди
(define (set-tail-ptr! queue elem)
  (set-cdr! queue elem))

; очередь пустая, если указатель на начало пустой
(define (empty-queue? queue)
  (null? (head-ptr queue)))

; создание очереди - пустой
(define (create-queue)
  (cons (list) (list)))

; добавление элемента в конец очереди
(define (push-queue! queue elem)
  (let ((new-pair (cons elem (list)))) ; новая пара: элемент, пустой список
    (cond ((empty-queue? queue) ; если очередь пустая, то
           (set-head-ptr! queue new-pair) ; и начало очереди
           (set-tail-ptr! queue new-pair)) ; и конец указывают на новый элемент
          (else ; в противном случае
           (set-cdr! (tail-ptr queue) new-pair) ; изменить последнюю пару очереди
           (set-tail-ptr! queue new-pair) ; перенаправление хвостового указателя на последний элемент
           ))))




; временная переменная
(define temp 'cons)

; сортировка значений в порядке возрастания
(define (sort-queue queue)
  ; проход по очереди i
  (define (iter-queue elems1)
    ; проход по очереди j
    (define (inner-iter elems2)
      (cond ((not (null? elems2)) ; пока есть элементы
             (cond ((> (cdr (car elems1)) (cdr (car elems2))) ; если i элемент больше j
                    (set! temp (car elems1)) ; то выполняется замена
                    (set-car! elems1 (car elems2))
                    (set-car! elems2 temp))
                    )
             (inner-iter (cdr elems2))))) ; переход к следующему элементу внутренней рекурсии
    
    (cond ((not (null? elems1)) ; пока есть элементы
           (inner-iter (cdr elems1)) ; вызов внутренней рекурсии со следующего элемента
           (iter-queue (cdr elems1))) ; переход к следующему элементу внешней рекурсии
          (else
           queue))) ; когда элементы закончились - вернуть отсортированную очередь
    
  (iter-queue (head-ptr queue)))




; создание очереди и добавление названия задач с приоритетами
(define queue-nbrs (create-queue))
(push-queue! queue-nbrs (cons "Task" 6))
(push-queue! queue-nbrs (cons "Task" 5))
(push-queue! queue-nbrs (cons "Task" 8))
(push-queue! queue-nbrs (cons "Task" 3))
(push-queue! queue-nbrs (cons "Task" 2))
(push-queue! queue-nbrs (cons "Task" 1))


; вывод исходной очереди
(display "--> Исходная очередь :: ")
(display (head-ptr queue-nbrs)) (newline) (newline)


; сортировка очереди
(define sorted-nbrs (sort-queue queue-nbrs))
(display "--> Отсортированная очередь :: ")
(display (head-ptr sorted-nbrs))
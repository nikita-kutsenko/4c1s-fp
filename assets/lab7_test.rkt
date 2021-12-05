#lang racket



; запись в файл n строк
(define (write-to-file filename)
  ;(delete-file filename) ; удалить файл, когда он существует

  (define fd (open-output-file filename)) ; открытие файла для записи

  ; запись поочередно строк с файл
  (define (write-lines i)
    (cond ((< i 2)
           (display " Input line ")(display i) (display ": ")
           (displayln (read-line) fd) ; запись строки в файл
           (write-lines (+ i 1))))) ; переход к записи следующей строки

  (write-lines 0)
  
  (close-output-port fd)); закрытие файла






; создать список слов из строки
(define (create-words-list line)
  (define list-of-words (list)) ; список для слов
  (define start-word-index 0) ; индекс начала слова
  
  (define (iter-line i)
    ; проход по символам пока индекс не выходит за размер строки
    (cond ((< i (string-length line))
           ; найдено начало слова, если символ не пробел и (предыдущий символ пробел или начало строки i=0)
           (cond ((and (not (eq? (string-ref line i) #\space)) (or (= i 0) (eq? (string-ref line (- i 1)) #\space)))
                  (set! start-word-index i)) ; запись индекса начала слова

                 ; если i != 0 И str[i] == space, а предыдущий символ в строке не пробел -> значит найден конец слова
                 ((and (not (= i 0)) (eq? (string-ref line i) #\space) (not (eq? (string-ref line (- i 1)) #\space)))
                  ; добавление слова в список слов | выделение слова с помощью substring
                  (set! list-of-words (append list-of-words (list (substring line start-word-index i)))))

                 ; если i!=0 И i=длине строки И предыдущий символ не пробел -> найден конец строки (случай со словом в конце строки)
                 ((and (not (= i 0)) (= (+ i 1) (string-length line)) (not (eq? (string-ref line (- i 1)) #\space)))
                  ; добавление слова в список слов | выделение слова с помощью substring
                  (set! list-of-words (append list-of-words (list (substring line start-word-index (string-length line)))))))
           (iter-line (+ i 1)))) ; переход к следующему символу
    )
  (iter-line 0)
  list-of-words) ; возвращение списка слов






; замена слов в строках
(define (create-new-line first-line words1 words2)
  (define result-string (string))
  
  (define i 0) ; счетчик для прохода по первой строке
  (define list1 words1) ; список слов первой строки
  (define list2 words2) ; список слов второй строки
  (define count-words 1) ; счетчик слов
  
  ; проход по первой строке
  (define (main-iter)
    ; пока индекс не превышает длину строки
    (cond ((< i (string-length first-line))
           ; если встретился пробел
           (cond ((eq? (string-ref first-line i) #\space)
                  ; к результирующей строке добавляется пробел
                  (set! result-string (string-append result-string (string #\space)))
                  (set! i (+ i 1))) ; счетчик увеличивается на 1 для перехода к след символу
                 
                 ; если встречается начало слова
                 ((and (not (eq? (string-ref first-line i) #\space)) (or (= i 0) (eq? (string-ref first-line (- i 1)) #\space)))
                  (cond ((and (= (remainder count-words 2) 0) (not (null? list2))) ; если слово четное
                         ; то к строке добавляется слово на нечетной позиции из второй строки
                         (set! result-string (string-append result-string (car list2)))

                         (set! i (+ i (string-length (car list1)))) ; индекс смещается на след символ после конца строки

                         (set! count-words (+ count-words 1)) ; увеличивается счетчик слов

                         (set! list1 (cdr list1)) ; переход к след слову первой строки

                         (if (null? (cdr list2)) ; если слова во второй строке закончились
                             (set! list2 (list)) ; то списку слов второй строки указывается пустой список
                             (set! list2 (cddr list2)))) ; иначе переход к след нечетному слову


                        ((not (null? list1)) ; в другом случае: если слово нечетное или слова во второй строке закончились
                         (set! result-string (string-append result-string (car list1))) ; к строке добавляется слово из первой строки

                         (set! i (+ i (string-length (car list1)))) ; индекс смещается на след символ после конца строки

                         (set! count-words (+ count-words 1)) ; увеличивается счетчик слов

                         (set! list1 (cdr list1)))))) ; переход к след слову первой строки
           (main-iter))))
  
  (main-iter)
  result-string)







; функция обработки строк
(define (transform-of-lines list-lines)
  (define list-words1 (create-words-list (car list-lines))) ; список слов из первой строки
  (define list-words2 (create-words-list (car (cdr list-lines)))) ; список слов из второй строки
  (define count-words1 0) ; счетчик слов в первой строке

  ;(delete-file "result_file.txt") ; удалить файл, когда он существует
  ; открытие файла на запись
  (define fd-write (open-output-file "result_file.txt"))

  ; запись в новый файл преобразованной первой строки
  (displayln (create-new-line (car list-lines) list-words1 list-words2) fd-write)

  ; запись в файл второй строки
  (displayln (car (cdr list-lines)) fd-write)

  ; закрытие файла
  (close-output-port fd-write)
  )





; считывание содержимого файла
(define (read-from-file filename)
  (displayln "Сontent of file:")

  ; список для сохранения строк из файла
  (define list-lines (list))

  ; открытие файла для чтения
  (define fd (open-input-file filename))

  ; построчное чтение из файла
  (define (iter-read n)
    (define next-line (read-line fd)) ; считывание следующей строки

    (cond ((not (eof-object? next-line)) ; пока не достигнут конец файла
           (displayln next-line) ; вывод на консоль считанной строки
           ; добавление считанной строки в список строк
           (set! list-lines (append list-lines (list next-line)))
           (iter-read (+ n 1))))); переход к считыванию следующей строки
  
  (iter-read 0)

  ; закрытие файла
  (close-input-port fd)

  ; обработка и запись в файл строк
  (transform-of-lines list-lines)
  )




























; записать в файл строки, считанные с клавиатуры
(write-to-file "input.txt")

; вывод содержимого из файла
(read-from-file "input.txt")









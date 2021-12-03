#lang racket

(define time 10)
(define containers 20)
(define time_back_80_1 0)
(define time_back_80_2 0)
(define time_back_80_3 0)
(define time_back_140_1 0)
(define time_back_140_2 0)
(define waiting_time_140 0)





(define (set_plane_time_back plane)
  ; check conditions 
  (cond 
    ;  timer for plane 80_1
    [(equal? plane "80_1")
     (set! time_back_80_1 (+ time 180))
     (printf ":: ~a mins ---> LEFT :: plane 80_1 || ARRIVES at :: ~a \n" time time_back_80_1)]
     
    ;  timer for plane 80_2
    [(equal? plane "80_2")
     (set! time_back_80_2 (+ time 180))
     (printf ":: ~a mins ---> LEFT :: plane 80_2 || ARRIVES at :: ~a \n" time time_back_80_2)]

    ;  timer for plane 80_3
    [(equal? plane "80_3")
     (set! time_back_80_3 (+ time 180))
     (printf ":: ~a mins ---> LEFT :: plane 80_3 || ARRIVES at :: ~a \n" time time_back_80_3)]

    ;  timer for plane 140_1
    [(equal? plane "140_1")
     (set! time_back_140_1 (+ time 180))
     (printf ":: ~a mins ---> LEFT :: plane 140_1 || ARRIVES at :: ~a \n" time time_back_140_1)]

    ;  timer for plane 140_2
    [(equal? plane "140_2")
     (set! time_back_140_2 (+ time 180))
     (printf ":: ~a mins ---> LEFT :: plane 140_2 || ARRIVES at :: ~a \n" time time_back_140_2)]
  ))





(define (set_plane_is_back open_planes)
  ; check conditions 
  (cond 
    ;  time of plane 80_1 comming back
    [(equal? time time_back_80_1)
      (set! time_back_80_1 0)
      (printf ":: ~a mins ---> ARRIVED :: plane 80_1 ~a \n" time time_back_80_1)
      (append (list "80_1") open_planes)]
     
    ;  time of plane 80_2 comming back
    [(equal? time time_back_80_2) 
      (set! time_back_80_2 0)
      (printf ":: ~a mins ---> ARRIVED :: plane 80_2 ~a \n" time time_back_80_2)
      (append (list "80_2") open_planes)]

    ;  time of plane 80_3 comming back
    [(equal? time time_back_80_3)
      (set! time_back_80_3 0)
      (printf ":: ~a mins ---> ARRIVED :: plane 80_3 ~a \n" time time_back_80_3)
      (append (list "80_3") open_planes)]

    ;  time of plane 140_1 comming back
    [(equal? time time_back_140_1)
      (set! time_back_140_1 0)
      (printf ":: ~a mins ---> ARRIVED :: plane 140_1 ~a \n" time time_back_140_1)
      (append (list "140_1") open_planes)]

    ;  time of plane 140_2 comming back
    [(equal? time time_back_140_2)
      (set! time_back_140_2 0)
      (printf ":: ~a mins ---> ARRIVED :: plane 140_2 ~a \n" time time_back_140_2)
      (append (list "140_2") open_planes)]

    [else open_planes]))



(define (finish)
  (display "finish"))






(define (next pre_open_planes)
  (let ([open_planes (set_plane_is_back pre_open_planes)])
    (printf "\n--> open planes ~a\n" open_planes)
    ; (printf "\n--> TIME ~a\n" time)
    ; check conditions 
    (cond 
      ; containers == 80 && one of 80 planes' is open 
      [(and (equal? containers 80) (or (member "80_1" open_planes) (member "80_2" open_planes) (member "80_3" open_planes)))
        ((set! containers 0)
        (set_plane_time_back (car open_planes))
        ; (printf ":: ~a mins :: decreased containers ~a :: used plane ~a :: rest of planes ~a\n" time containers (car open_planes) (rest open_planes))
        (next (rest open_planes)))]

      ; containers == 140 && one of 140 planes' is open 
      [(and (equal? containers 140) (or (member "140_1" open_planes) (member "140_2" open_planes)))
        ((set! containers 0)
        (set_plane_time_back (car open_planes))
        ; (printf ":: ~a mins :: decreased containers ~a :: used plane ~a :: rest of planes ~a\n" time containers (car open_planes) (rest open_planes))
        (next (rest open_planes)))]

      ; Finishhing the program
      [(and (> containers 110) (> time 360)) (finish)]

      ; increase time and amount of containers for open planes
      [(and (<= containers 140) (< time 370))
        (
          ; (printf "\n--> containers <=140 :: ~a  && time < 370 :: ~a\n" containers time)
         (set! containers (+ containers 20))
         (set! time (+ time 10))
        ;  (if (and (not (or (member "80_1" open_planes) (member "80_2" open_planes) (member "80_3" open_planes))) (or (member "140_1" open_planes) (member "140_2" open_planes)) (> containers 60))
        ;    (printf "~a " #t)
        ;    (printf "~a " #f))
         (printf ":: ~a mins :: increased cdontainers ~a\n" time containers)
         (next open_planes)
         )]
    )))







(next (list "80_1" "80_2" "80_3" "140_1" "140_2"))
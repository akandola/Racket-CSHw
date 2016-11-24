;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Anmolpreet_Kandola_Sarah_Rathje_PSET2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Anmolpreet Kandola and Sarah Rathje
; PSET 2
(require 2htdp/image)
(require 2htdp/universe)
; Problem 1: Design a star with side-length and color, inside a square with
;            sq-len
(define (favorite-star side-length color sq-len)
  (overlay (star side-length "solid" color)
           (square sq-len "outline" "black")))
(favorite-star 12 "red" 20)

; Problem 2 (2e Exercise 43) Making the car move across the screen
(define WIDTH-OF-WORLD 600)
(define HEIGHT-OF-WORLD 50)

;Car Box
(define BOXOFCAR (rectangle 50 30 "solid" "red"))
(define tree
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))
; Full car
(define CAR
  (overlay (beside (circle 5 "solid" "black")
                   (rectangle 10 5 "outline" "red")
                   (circle 5 "solid" "black"))
           (rectangle 50 30 "solid" "red")))

(define BACKGROUND
  (place-image tree
               (/ WIDTH-OF-WORLD 2)
               (/ HEIGHT-OF-WORLD 2)
               (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD)))

(define Y-CAR (- HEIGHT-OF-WORLD 5))                
; render : 
;   WorldState -> Image 
; big-bang evaluates (render cw) to obtain image of
; current world cw when needed
(define (render num)
  (place-image CAR num Y-CAR BACKGROUND))
     
; tock:
; WorldState -> WorldState
(define (tock num)
  (cond
    [(>= num 0) (+ num 3)]))

;WorldState -> Boolean
(define (end? num)
  (>= num (+ (image-width CAR) WIDTH-OF-WORLD)))

; Key-event
(define (reset num a-key) 0)

;Mouse-event
(define (reset2 num a b a-mouse) 0)

(define (main num)
  (big-bang num
       [on-tick tock]
       [to-draw render]
       ;[on-key reset]
       ;[on-mouse reset2]
       [stop-when end?]))
;(main 0) ;651 ticks each time and after which the animation stops

; Probem 3 (exercise 45) Making the car move in a sin pattern
(define (render2 num)
  (place-image CAR num (* (sin num) 50) BACKGROUND))

;WorldState->WorldState
(define (tock2 num)
  (cond
    [(>= num 0) (+ num 3)]))

(define (main2 num)
  (big-bang num
       [on-tick tock2 .05]
       [to-draw render2]
       ;[on-key reset]
       ;[on-mouse reset2]
       [stop-when end?]))
(main2 0)

; Problem 4: Humidity percentage (number) -> Statement about condition (string)
(define (humidity percentage)
  (cond
    ((> percentage 65) "humid")
    ((< percentage 20) "dry")
    (else "comfortable")))

(humidity 15) ;dry
(humidity 20) ;comfortable
(humidity 40) ;comfortable
(humidity 65) ;comfortable
(humidity 80) ;humid




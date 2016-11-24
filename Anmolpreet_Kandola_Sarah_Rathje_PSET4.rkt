;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Anmolpreet_Kandola_Sarah_Rathje_PSET4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Anmolpreet Kandola and Sarah Rathje
; Problem Set 4
; 02/03/16
(require 2htdp/image)
(require 2htdp/universe)
;problem 1----------------------------------------------------------------------------------------------------------------------------


(define-struct lecture-hall (number capacity))
;-make-lecturehall
;-lecture-hall-number
;-lecture-hall-capacity
;-lecture-hall?
;;;; lecture-hall:
;;; +--------+-----------+
;;; | number | capacity  |
;;; +--------+-----------+
;A LectureHall is (make-lecture-hall Number Number)
;interpretation: number is the classroom number 
;interpretation: capacity is max number of people in a particular classroom
#;
(define (lecture-hall-tmpl lh)
  (... (lecture-hall-number lh)... (lecture-hall-capacity lh)...))
  
(define-struct automobile (year make model))
;make-automobile
;-automobile-year
;-automobile-make
;-automobile-model
;-automobile?
;;; automobile:
;;; +-----+-------+------+
;;; | year| make  | model|
;;; +-----+-------+------+
;An Automobile is (make-automobile (Number String String)
;interpretation: year is the year the car was produced
;interpretation: make is the brand of the car
;interpretation: model is the type of car

#;
(define (automobile-tmpl a)
  (... (automobile-year a)...(automobile-make a)...(automobile-model a)...))

(define-struct football-player (name position number))
;make-football-player
;-football-player-name
;-football-player-position
;-football-player-number
;-football-player?
;;; football-player:
;;; +------+-----------+--------+
;;; | name | position  | number |
;;; +------+-----------+--------+
; A FootballPlayer is a (make- football-player(String String Number))
; interpretation: name is the name of the player
; interpretation: position is the postion of the player
; interpretation: number is the number of the player's jersey

#;

(define (football-player-tmpl fp)
  (...(football-player-name fp)... (football-player-position fp)... (football-player-number fp)...))

(define-struct shirt (material size color))
;make-shirt
;-shirt-material
;-shirt-size
;-shirt-color
;-shirt?
;;; shirt:
;;; +----------+------+-------+
;;; |  material| size | color |
;;; +----------+------+-------+
; A Shirt is a (make-shirt (String Number String)
; interpretation: a material is what the shirt is made of
; interpretation: a size is the size of the shirt in numbers (Euro szing)
; interpretation: a color is the color of the shirt

#;
  (define (shirt-tmpl s)
    (...(shirt-material s)...(shirt-size s)... (shirt-color s)...))





;problem 2----------------------------------------------------------------------------------------------------------------------------

(define-struct time (hours minutes))
; A Time is a structure:
;    (make-time Number Number)
; interpretation: (make-time h m) is the time  
; expressed in hours, and minutes
; Constraints:
; – hours is always between 0 and 11
; – minutes is always between 0 and 59
  
;Time -> Time
;Adds one minute to given time
  (define (tock t)
  (cond
    [ (< (time-minutes t) 59)
         (make-time (time-hours t)
         (+ (time-minutes t) 1))]
    [(and (= (time-minutes t) 59)
           (< (time-hours t) 11))
           (make-time ( + (time-hours t) 1) 0)]    
    [ else (make-time 0 0)]))
                              
  (check-expect (tock (make-time 1 1)) (make-time 1 2))

  (check-expect (tock (make-time 3 59)) (make-time 4 0))

  (check-expect (tock (make-time 11 59)) (make-time 0 0))

;Time -> Image
;displays time as an image
(define (time->text t)
  (place-image(text (string-append (number->string(time-hours t))
                 ":"
                 (number->string(time-minutes t))) 70 "red") 100 100 (empty-scene 200 200)))

(check-expect (time->text (make-time 11 37))
              (place-image(text "11:37" 70 "red") 100 100 (empty-scene 200 200)))

(define (main t0)
  (big-bang t0
            [on-tick tock 60]
            [to-draw time->text]))

;problem 3 ---------------------------------------------------------------------------------------------------------------------------

; A ballpos is a make-ball (Number Number String)
;interpretation: x, y coordinates and direction
(define-struct ballpos (x y direction))

(define SPEED 10)
;Speed is the distance moved per tick

; Direction is one of:
; - "up"
; - "down"
; - "left"
; - "right"

(define BALL (circle 5 "solid" "red"))
; A BALL is an image

;test ballpos for checks/examples
(define b2 (make-ballpos 50 50 "up"))
(define b3 (make-ballpos 50 50 "down"))
(define b4 (make-ballpos 50 50 "left"))
(define b5 (make-ballpos 50 50 "right"))


; ballpos -> ballpos
; Moves ball to next position.

;Examples:
;(ball-next b2) -> (make-ballpos 50 40 "up")
;(ball-next b3) -> (make-ballpos 50 60 "down")
;(ball-next b4) -> (make-ballpos 40 50 "left")
;(ball-next b5) -> (make-ballpos 60 50 "right")
              


#;
(define (ball-tmpl b1)
  (cond
    [(string=? "up" (ballpos-direction b1)) ... (ballpos-x b1) ...
                                            ... (ballpos-y b1) ...
                                            ... (ballpos-direction) ...]
    [(string=? "down" (ballpos-direction b1)) ... (ballpos-x b1) ...
                                              ... (ballpos-y b1) ...
                                              ... (ballpos-direction) ...]
    [(string=? "left" (ballpos-direction b1)) ... (ballpos-x b1) ...
                                              ... (ballpos-y b1) ...
                                              ... (ballpos-direction) ...]
    [(string=? "right" (ballpos-direction b1)) ... (ballpos-x b1) ...
                                               ... (ballpos-y b1) ...
                                               ... (ballpos-direction) ...]))


(define (ball-next b1)
  (cond
    [(string=? "up" (ballpos-direction b1)) (make-ballpos
                                             (ballpos-x b1)
                                             (- (ballpos-y b1) SPEED)
                                             (ballpos-direction b1))]
    [(string=? "down" (ballpos-direction b1)) (make-ballpos
                                               (ballpos-x b1)
                                               (+ (ballpos-y b1) SPEED)
                                               (ballpos-direction b1))]
    [(string=? "left" (ballpos-direction b1)) (make-ballpos
                                               (- (ballpos-x b1) SPEED)
                                               (ballpos-y b1)
                                               (ballpos-direction b1))]
    [(string=? "right" (ballpos-direction b1)) (make-ballpos
                                                (+ (ballpos-x b1) SPEED)
                                                (ballpos-y b1)
                                                (ballpos-direction b1))]))

(check-expect (ball-next b2)
              (make-ballpos
               50
               40
               "up"))
(check-expect (ball-next b3)
              (make-ballpos
               50
               60
               "down"))
(check-expect (ball-next b4)
              (make-ballpos
               40
               50
               "left"))
(check-expect (ball-next b5)
              (make-ballpos
               60
               50
               "right"))


; ballpos -> image
; Makes the image of the ball.


;Examples
;(ball-image b1) -> (place-image BALL (ballpos-x b1) (ballpos-y b1) (rectange 300 300 "outline" "black"))
;(ball-image b2) -> (place-image BALL 50 50 (rectange 300 300 "outline" "black"))

#;
(define (ball-tmpl b1)
  (cond
    [(string=? "up" (ballpos-direction b1)) ... (ballpos-x b1) ...
                                            ... (ballpos-y b1) ...
                                            ... (ballpos-direction) ...]
    [(string=? "down" (ballpos-direction b1)) ... (ballpos-x b1) ...
                                              ... (ballpos-y b1) ...
                                              ... (ballpos-direction) ...]
    [(string=? "left" (ballpos-direction b1)) ... (ballpos-x b1) ...
                                              ... (ballpos-y b1) ...
                                              ... (ballpos-direction) ...]
    [(string=? "right" (ballpos-direction b1)) ... (ballpos-x b1) ...
                                               ... (ballpos-y b1) ...
                                               ... (ballpos-direction) ...]))

(define (ball-image b1)
  (place-image BALL
               (ballpos-x b1)
               (ballpos-y b1)
               (rectangle 300 300 "outline" "black")))

(check-expect (ball-image b2)
              (place-image BALL
                           50
                           50
                           (rectangle 300 300 "outline" "black")))
              
; ballpos key-event -> ballpos
; Changes direction of the ball's motion.

;Examples
;(ball-change b3 "up") -> (make-ballpos 50 50 "up")
;(ball-change b4 "right") -> (make-ballpos 50 50 "right")

#;
(define (ball-tmpl b1)
  (cond
    [(string=? "up" (ballpos-direction b1)) ... (ballpos-x b1) ...
                                            ... (ballpos-y b1) ...
                                            ... (ballpos-direction) ...]
    [(string=? "down" (ballpos-direction b1)) ... (ballpos-x b1) ...
                                              ... (ballpos-y b1) ...
                                              ... (ballpos-direction) ...]
    [(string=? "left" (ballpos-direction b1)) ... (ballpos-x b1) ...
                                              ... (ballpos-y b1) ...
                                              ... (ballpos-direction) ...]
    [(string=? "right" (ballpos-direction b1)) ... (ballpos-x b1) ...
                                               ... (ballpos-y b1) ...
                                               ... (ballpos-direction) ...]))

(define (ball-change b1 key-ev)
  (cond
    [(string=? "up" key-ev) (make-ballpos
                             (ballpos-x b1)
                             (ballpos-y b1)
                             key-ev)]
    [(string=? "down" key-ev) (make-ballpos
                             (ballpos-x b1)
                             (ballpos-y b1)
                             key-ev)]
    [(string=? "left" key-ev) (make-ballpos
                             (ballpos-x b1)
                             (ballpos-y b1)
                             key-ev)]
    [(string=? "right" key-ev) (make-ballpos
                             (ballpos-x b1)
                             (ballpos-y b1)
                             key-ev)]
    [else b1]))

(check-expect (ball-change b2 "down")
              (make-ballpos
               50
               50
               "down"))
(check-expect (ball-change b3 "up")
              (make-ballpos
               50
               50
               "up"))
(check-expect (ball-change b4 "right")
              (make-ballpos
               50
               50
               "right"))
(check-expect (ball-change b5 "left")
              (make-ballpos
               50
               50
               "left"))

(define (main2 world0)
  (big-bang world0
            (to-draw ball-image)
            (on-tick ball-next)
            (on-key ball-change)))

; Test for main2 (ballpos function)
; (main2 (make-ballpos 50 50 "up"))

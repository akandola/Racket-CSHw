;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Anmolpreet_Kandola_Sarah_Rathje_PSET3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Anmolpreet Kandola and Sarah Rathje
; Problem Set 3 - CS 2500
(require 2htdp/image)
(require 2htdp/universe)

; Problem 1
;define NinjaTurtle -> name
; A NinjaTurtle is one of:
; - Leonardo
; - Michaelangelo
; - Raphael
; - Donatello
;NinjaTurtle->???
#;
(define (nt-temp state)
  (cond
    [(string=? state "Leonardo") ...]
    [(string=? state "Michaelangelo") ...]
    [(string=? state "Raphael") ...]
    [(string=? state "Donatello") ...]))

; A percentage is one of:
; - A real number between 0 and 100 (and (>= percentage 0) (<= percentage 100))


; Problem 2 (exercise 60)
; A Price falls into one of three intervals: 
; — 0 through 1000;
; — 1000 through 10000;
; — 10000 and above.
; interpretation the price of an item
; Price -> Number
; computes the amount of tax charged for price p

(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p 1000)) 0]
    [(and (<= 1000 p) (< p 10000)) (* .05 p)]
    [(>= p 10000) (* .08 p)]))

(check-expect (sales-tax 537) 0)
(check-expect (sales-tax 1000) (* 0.05 1000))
(check-expect (sales-tax 12017) (* 0.08 12017))

;Constant Definitions
(define luxury 10000)
(define low 1000)

;implementing the constant definitions to manipulate taxes
(define (sales-tax2 p)
  (cond
    [(and (<= 0 p) (< p low)) 0]
    [(and (<= low p) (< p luxury)) (* .05 p)]
    [(>= p luxury) (* .08 p)]))

;Problem 3 exercise 66
; Posn -> Number
; computes the distance of a posn to the origin
(define (manhattan-distance a-posn)
  (+
   (posn-x a-posn)
   (posn-y a-posn)))

(define example (make-posn 3 4))
(check-expect (manhattan-distance example) 7) ;should be 7

; Problem 4
; Posn Posn -> Image
(define BG (empty-scene 200 200))

;places red circle on BG
;img posn posn -> new image
(define (img1 posn1 posn2)
  (place-image
   (circle 5 "solid" "red") (posn-x posn1) (posn-y posn1)
   BG))

;places blue circle on most recent image
;posn posn -> image
(define (img2 posn1 posn2)
  (place-image 
   (circle 5 "solid" "blue") (posn-x posn2) (posn-y posn2)
   (img1 posn1 posn2)))
;places green dot 90% of distance between blue and red dot
;
(define (img3 posn1 posn2)
  (place-image
   (circle 5 "solid" "green")
     (+ (* (- (posn-x posn2) (posn-x posn1)) .9) (posn-x posn1))
     (+ (* (- (posn-y posn2) (posn-y posn1)) .9) (posn-y posn1))
     (img2 posn1 posn2)))
;places line through all 3 dots
(define (linedraw posn1 posn2)
  (add-line
   (img3 posn1 posn2)
   (posn-x posn1)
   (posn-y posn1)
   (posn-x posn2)
   (posn-y posn2)
   "black"))


(define posn1 (make-posn 30 30))
(define posn2 (make-posn 100 100))
(linedraw posn1 posn2)
(check-expect (linedraw posn1 posn2)
              (add-line
               (img3 posn1 posn2)
               (posn-x posn1)
               (posn-y posn1)
               (posn-x posn2)
               (posn-y posn2)
               "black"))


;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Anmolpreet_Kandola_Ezra_Levy_PSET5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; PSET 5

;Anmolpreet Kandola and Ezra Levy
;Anmolpreet: 001621744
;Ezra: 001624514

;_____________________________

; Problem 1

; A Movie is one of:
; Regular
; Classic

; A Regular is a (make-regular String Number Number)
; interp. pid is product I.D.
;         bp is the base price
;         yis is the years in stock
(define-struct regular (pid bp yis))
;  -make-regular
;  -regular-pid
;  -regular-bp
;  -regular-yis

; A Classic is a (make-classic String Number Number)
; interp. pid is product I.D.
;         bp is the base price
(define-struct classic (pid bp ))
;  -make-classic
;  -classic-pid
;  -classic-bp

(define FN (make-classic "Finding Nemo" 100))
(define TV (make-regular "The Vow" 20 4))

; Movie -> Number
; takes in a movie, outputs current price

(check-expect (movie-price (make-regular "Cars" 15 10)) 9.75)
(check-expect (movie-price (make-classic "Titanic" 20)) 20)

#|
(define (movie-price-templ m)
  (cond [(regular? m) ...
         (regular-pid m) ...
         (regular-bp m) ...
         (regular-yis m) ...]
        [(classic? m)...
         (classic-pid m) ...
         (classic-bp m) ...]))
|#

; Assuming no movie bp is <2
(define (movie-price m)
  (cond [(regular? m)
         (if (< (- (regular-bp m)
                   (* (* (regular-bp m) .035) (regular-yis m))) 2)
             2
             (- (regular-bp m) (* (* (regular-bp m) .035)
                                         (regular-yis m))))]
        [(classic? m) (classic-bp m)]))

;_____________________________

;; Problem 2

;; Shape is one of:
;; -- Circle
;; -- Square
;; -- Rectangle

(define-struct circl [x y r outline c])
;; A Circle is a (make-circl Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of the circle,
;;   r the radius, outline whether it's outlined or solid,
;;   and c its color

(define-struct squar [x y size outline c])
;; A Square is a (make-squar Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of the square
;;   size is the side length, where sides are parallel to borders of canvas,
;;   outline whether it's outlined or solid, and c its color

(define-struct recta [x y width height outline c])
;; A Rectangle is a (make-recta Number Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of the square
;;   width is the side length of the top and bottom, height is the side length
;;   of the left and right, where sides are parallel to borders of canvas,
;;   outline whether it's outlined or solid, and c its color

#|(define (templ sh)
  (cond [(circle? sh) ...
         ((circle-x sh) ...
         (circle-y sh) ...
         (circle-r sh) ...
         (circle-outline sh) ...
         (circle-c sh) ...]
        [(squar? sh) ...
         (squar-x sh) ...
         (squar-y sh) ...
         (squar-size sh) ...
         (squar-outline sh) ...
         (squar-c sh) ...]
        [(recta? sh) ...
         (recta-x sh) ...
         (recta-y sh) ...
         (recta-width sh) ...
         (recta-height sh) ...
         (recta-outline sh) ...
         (recta-c sh) ...]))
|#

;; Shape-shift-x
;; SH DELTA -> Image
;; produces an image of sh shifted by delta-x

(define c1 (make-circl 50 50 10 #true 'black))
(define s1 (make-squar 50 50 20 #false 'black))
(define r1 (make-recta 50 50 10 20 #true 'black))
(define MT (empty-scene 500 500))

(check-expect (shape-shift-x c1 10)
              (place-image
               (circle
                10
                'outline
                'black)
               60
               50
               MT))

(check-expect (shape-shift-x s1 10)
              (place-image
               (square
                20
                'solid
                'black)
               60
               50
               MT))

(check-expect (shape-shift-x r1 30)
              (place-image
               (rectangle
                10
                20
                'outline
                'black)
               80
               50
               MT))

(define (shape-shift-x sh x)
  (cond [(circl? sh) (place-image
                      (circle
                       (circl-r sh)
                       (if (boolean=? (circl-outline sh) #true)
                           'outline
                           'solid)
                       (circl-c sh))
                      (+ (circl-x sh) x)
                      (circl-y sh)
                      MT)]
        [(squar? sh) (place-image
                      (square
                       (squar-size sh)
                       (if (boolean=? (squar-outline sh) #true)
                           'outline
                           'solid)
                       (squar-c sh))
                      (+ (squar-x sh) x)
                      (squar-y sh)
                      MT)]
        [(recta? sh) (place-image
                      (rectangle
                       (recta-width sh)
                       (recta-height sh)
                       (if (boolean=? (recta-outline sh) #true)
                           'outline
                           'solid)
                       (recta-c sh))
                      (+ (recta-x sh) x)
                      (recta-y sh)
                      MT)]))


;; Shape-in
;; Posn SH -> Boolean
;; checks whether given Posn is inside boundary os SH

(check-expect (shape-in? c1 (make-posn 55 55)) #true)
(check-expect (shape-in? c1 (make-posn 100 100)) #false)
(check-expect (shape-in? s1 (make-posn 60 60)) #true) ;boundary condition
(check-expect (shape-in? s1 (make-posn 100 100)) #false)
(check-expect (shape-in? r1 (make-posn 52 52)) #true)
(check-expect (shape-in? r1 (make-posn 100 100)) #false)

(define (shape-in? sh p)
  (cond [(circl? sh)
         (<= (sqrt (+ (sqr (- (posn-x p) (circl-x sh)))
                      (sqr (- (posn-y p) (circl-y sh)))))
             (circl-r sh))]
        [(squar? sh)
         (and (and (<= (posn-x p) (+ (squar-x sh) (/ (squar-size sh) 2)))
                   (>= (posn-x p) (- (squar-x sh) (/ (squar-size sh) 2))))
              (and (<= (posn-y p) (+ (squar-y sh) (/ (squar-size sh) 2)))
                   (>= (posn-y p) (- (squar-y sh) (/ (squar-size sh) 2)))))]
        [(recta? sh)
         (and (and (<= (posn-x p) (+ (recta-x sh) (/ (recta-width sh) 2)))
                   (>= (posn-x p) (- (recta-x sh) (/ (recta-width sh) 2))))
              (and (<= (posn-y p) (+ (recta-y sh) (/ (recta-height sh) 2)))
                   (>= (posn-y p) (- (recta-y sh) (/ (recta-height sh) 2)))))]))


;; Shape-draw
;; SH SC -> Image
;; places shape sh onto scene sc

(check-expect (shape-draw c1 MT) (place-image
                                  (circle 10 'outline 'black)
                                  50
                                  50
                                  MT))
(check-expect (shape-draw s1 MT) (place-image
                                  (square 20 'solid 'black)
                                  50
                                  50
                                  MT))
(check-expect (shape-draw r1 MT) (place-image
                                  (rectangle 10 20 'outline 'black)
                                  50
                                  50
                                  MT))

(define (shape-draw sh MT)
  (cond [(circl? sh)
         (place-image (circle (circl-r sh)
                              (if (boolean=? (circl-outline sh) #true)
                                  'outline
                                  'solid)
                              (circl-c sh))
                      (circl-x sh)
                      (circl-y sh)
                      MT)]
        [(squar? sh)
         (place-image (square (squar-size sh)
                              (if (boolean=? (squar-outline sh) #true)
                                  'outline
                                  'solid)
                              (squar-c sh))
                      (squar-x sh)
                      (squar-y sh)
                      MT)]
        [(recta? sh)
         (place-image (rectangle (recta-width sh)
                                 (recta-height sh)
                                 (if (boolean=? (recta-outline sh) #true)
                                     'outline
                                     'solid)
                                 (recta-c sh))
                      (recta-x sh)
                      (recta-y sh)
                      MT)]))




;; ... problem solving steps ...

;; inspect for expected results:
(define sh (make-squar 100 100 50 true 'red))
(define pt (make-posn  130 130))

(shape-in? sh pt)

(shape-draw (make-circl 130 130 5 true 'red)
            (shape-draw sh
                        (empty-scene 300 300))) 



;_____________________________

;; Problem 3

;; A Password is:
;; A String

;; List of Passwords -> boolean
;; Determines whether all strings in List of Strings
;; are between at least 6 characters long but less
;; than 11 characters long.

(define L1 (cons "foo1203" (cons "barred" (cons "123456789" empty))))
(define L2 (cons "alice" (cons "bob" (cons "carol" L1))))

(check-expect (passwords-6-11? L1) #true)
(check-expect (passwords-6-11? L2) #false)

#|
(define (password-tmpl p)
  (cond
    [(empty? p) ...]
    [(cons? p) ... (password-tmpl (first p)) ...
                 ... (password-tmpl (rest p))...]))
|#

(define (passwords-6-11? p)
  (cond
    [(empty? p) #true]
    [(cons? p) (if (not (and (>= (string-length (first p)) 6)
                             (< (string-length (first p)) 11)))
                   #false
                   (passwords-6-11? (rest p)))]))

;; List of Passwords, Number, Number -> Boolean
;; Tells whether all Strings in the List of Passwords
;; are between the minimum and maxiumum lengths.

(check-expect (passwords-ok? L1 6 10) #true)
(check-expect (passwords-ok? L1 7 10) #false)
(check-expect (passwords-ok? L2 3 10) #true)
(check-expect (passwords-ok? L2 5 10) #false)

#|
(define (password-tmpl p)
  (cond
    [(empty? p) ...]
    [(cons? p) ... (password-tmpl (first p)) ...
                 ... (password-tmpl (rest p))...]))
|#

(define (passwords-ok? p min max)
  (cond
    [(empty? p) #true]
    [(cons? p) (if (not (and (>= (string-length (first p)) min)
                             (< (string-length (first p)) max)))
                   #false
                   (passwords-ok? (rest p) min max))]))

;_____________________________

;; Problem 4

(define-struct ball [x y color])
;; A Ball is a (make-ball Number Number Color)
;; Color is one of 'red, 'yellow, 'blue, etc.

;; A LOB (list of balls) is one of:
;; empty
;; (cons Ball LOB)

#|
(define (lob-temp alob)
  (cond [(empty? alob) ...]
        [(cons? alob) ... (first alob)
                      ... (lob-temp (rest alob))]))
|#

(define ball1 (make-ball 5 10 'red))
(define ball2 (make-ball 10 5 'green))
(define ball3 (make-ball 15 15 'black))
(define ball4 (make-ball 150 350 'blue))

;; Lob-length
;; LOB -> Number
;; counts how many balls are on a given list of balls

(check-expect (lob-length (cons ball1 '())) 1)
(check-expect (lob-length (cons ball1 (cons ball2 (cons ball3 '())))) 3)

(define (lob-length alob)
  (cond [(empty? alob) 0]
        [(cons? alob) (+ 1 (lob-length (rest alob)))]))

;; A LON (list of numbers) is one of:
;; empty
;; (cons Number LON)

;; Lob-x
;; LOB -> LON
;; extracts all x coordinates of LOB and places in LON

(check-expect (lob-x (cons ball1 '())) (cons 5 '()))
(check-expect (lob-x (cons ball1 (cons ball2 (cons ball3 '()))))
              (cons 5 (cons 10 (cons 15 '()))))

(define (lob-x alob)
  (cond [(empty? alob) '()]
        [(cons? alob) (cons (ball-x (first alob))
                            (lob-x (rest alob)))]))

;; Lob-draw
;; LOB -> Image
;; draws the balls in LOB as radius 3 balls on a 300x300 empty scene

(define MT2 (empty-scene 300 300))

(check-expect (lob-draw (cons ball1 '()))
              (place-image
               (circle 3 'outline 'red)
               (ball-x ball1)
               (ball-y ball1)
               MT2))
(check-expect (lob-draw (cons ball1 (cons ball2 (cons ball3 '()))))
              (place-images
               (list (circle 3 'outline 'red)
                     (circle 3 'outline 'green)
                     (circle 3 'outline 'black))
               (list (make-posn (ball-x ball1) (ball-y ball1))
                     (make-posn (ball-x ball2) (ball-y ball2))
                     (make-posn (ball-x ball3) (ball-y ball3)))
               MT2))

(define (lob-draw alob)
  (cond [(empty? alob) MT2]
        [(cons? alob) (place-image (circle
                                    3
                                    'outline
                                    (ball-color (first alob)))
                                   (ball-x (first alob))
                                   (ball-y (first alob))
                                   (lob-draw (rest alob)))]))

;; Lob-filter
;; LOB -> LOB
;; produces a list of balls whose centers are within a 300x300 grid

(check-expect (lob-filter (cons ball1 (cons ball4 (cons ball2 '()))))
              (cons ball1 (cons ball2 '())))
(check-expect (lob-filter (cons ball2 (cons ball3 '())))
              (cons ball2 (cons ball3 '())))

(define (lob-filter alob)
  (cond [(empty? alob) '()]
        [(cons? alob) (if (ball-pos (first alob))
                          (cons (first alob) (lob-filter (rest alob)))
                          (lob-filter (rest alob)))]))

;; Ball -> Boolean
;; checks if a ball's coordinates are within a 300x300 grid

(define (ball-pos b)
  (and (< (ball-x b) 300)
       (< (ball-y b) 300)))

;; Lob-member?
;; LOB Ball -> Boolean
;; checks if a ball b occurs in a list of balls

(check-expect (lob-member? (cons ball1
                                 (cons ball4
                                       (cons ball2 '()))) ball1) #true)
(check-expect (lob-member? (cons ball2 (cons ball3 '())) ball1) #false)
(check-expect (lob-member? '() ball1) #false)

(define (lob-member? alob b)
  (cond [(empty? alob) #false]
        [(cons? alob) (or (is-b? alob b)
                          (lob-member? (rest alob) b))]))

;; Is-b?
;; LOB Ball -> Boolean
;; checks if (first alob) is the given ball b

(check-expect (is-b? (cons ball4 (cons ball2 '())) ball4) #true)
(check-expect (is-b? (cons ball2 '()) ball1) #false)

(define (is-b? alob b)
  (and (ball? (first alob))
       (= (ball-x (first alob)) (ball-x b))
       (= (ball-y (first alob)) (ball-y b))
       (symbol=? (ball-color (first alob)) (ball-color b))))




;_____________________________

;; Problem 5

(define-struct txt [content x y])
;; Txt = (make-txt String Number Number)
;; Represents the occurrence of the given text at the given location,
;; in computer-graphics coordinates.
 
;; LoTxt is one of:
;; -- empty
;; -- (cons Txt LoTxt)
 
(define-struct world [image hidden])
#;
(define (world-tmpl w)
  (cond
    [(empty? (world-hidden w)) ...]
    [(cons? (world-hidden w)) ... (world-image w) ..
                              ... (txt-content (first (world-hidden w))) ...
                              ... (txt-x (first (world-hidden w))) ...
                              ... (txt-y (first (world-hidden w))) ...
                              ... (world-tmpl (rest (world-hidden w))) ...]))
  
;; World = (make-world Image LoTxt)
;; intepretation:
;;  The world's image represents the image that the audience can see.
;;  The world's list of Txt represents the yet-to-be-revealed elements.
(define BG (empty-scene 400 400))
(define world1 (make-world
                BG
                 (cons (make-txt "On your mark." 200 75)
                       (cons (make-txt "Get set." 200 150)
                             (cons (make-txt "Go!" 200 225) empty)))))

(define world2 (make-world
                (place-image
                 (text "On your mark." 20 'blue)
                 200 75
                 BG)
                (cons (make-txt "Get set." 200 150)
                      (cons (make-txt "Go!" 200 225) empty))))

;; display
;; World -> Image
;; Draws an image of the current world

(check-expect (display world1) BG)
(check-expect (display world2) (place-image
                                (text "On your mark." 20 'blue)
                                200 75
                                BG))

#;
(define (world-tmpl w)
  (cond
    [(empty? (world-hidden w)) ...]
    [(cons? (world-hidden w)) ... (world-image w) ..
                              ... (txt-content (first (world-hidden w))) ...
                              ... (txt-x (first (world-hidden w))) ...
                              ... (txt-y (first (world-hidden w))) ...
                              ... (world-tmpl (rest (world-hidden w))) ...]))

(define (display w)
  (world-image w))


;; next
;; World -> World
;; Adds hidden text to the slide image

(check-expect (next world1)
              (make-world (place-image
                           (text "On your mark." 20 'blue)
                           200 75
                           BG)
                          (cons (make-txt "Get set." 200 150)
                                (cons (make-txt "Go!" 200 225) empty))))

(check-expect (next world2) (make-world (place-image
                                         (text "Get set." 20 'blue)
                                         200 150
                                         (place-image
                                          (text "On your mark." 20 'blue)
                                          200 75
                                          BG))
                                        (cons (make-txt "Go!" 200 225) empty)))

#;
(define (world-tmpl w)
  (cond
    [(empty? (world-hidden w)) ...]
    [(cons? (world-hidden w)) ... (world-image w) ..
                              ... (txt-content (first (world-hidden w))) ...
                              ... (txt-x (first (world-hidden w))) ...
                              ... (txt-y (first (world-hidden w))) ...
                              ... (world-tmpl (rest (world-hidden w))) ...]))                            

(define (next w)
  (cond
    [(empty? (world-hidden w)) w]
    [(cons? (world-hidden w))
     (make-world (place-image
                  (text (txt-content (first (world-hidden w))) 20 'blue)
                  (txt-x (first (world-hidden w)))
                  (txt-y (first (world-hidden w)))
                  (world-image w))
                 (rest (world-hidden w)))]))
                                                                                                     

(define (main w0)
  (big-bang w0
            (on-tick next 1)
            (to-draw display)))

;; Test for main below.
;; (main world1)



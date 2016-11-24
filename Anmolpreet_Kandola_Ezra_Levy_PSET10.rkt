;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Anmolpreet_Kandola_Ezra_Levy_PSET10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; PSET 10 - Centipede (complete with loop functions)

; Anmolpreet Kandola and Ezra Levy
; Anmolpreet: 001621744
; Ezra: 001624514

; ++++++++++++++++++++++++++++++++++++++++++

; List-based World Program

; Designing Centipede Game

; Outline for creating a world program

; 1. Define our constants - what is not changing in the program
; 2. Define our world - what is changing
; 3. Write Main and Wishlist
; 4. Design top down (use design recipe)

; Constants - size of player, bullet, centipede seg, tongue, cell, world
;             speed of player, centipede, bullet, mushroom size

; World - size of centipede - list of segments (posn)
;         direction - 2 strings from key events - "left", "right"
;         

; Wish List:

; on-tick - centipede moves, hit by bullet/end dies
; on-key - move left/right, fire bullet
; stop-when - centipede or player annihilated by each other
; to-draw - draws the centipede and player on the board

; ++++++++++++++++++++++++++++++++++++++++++

; Constants

(define GRID-WIDTH 25)
(define GRID-HEIGHT 40)
(define CELL-SIZE 15)
(define BG (empty-scene
            (* CELL-SIZE GRID-WIDTH)
            (* CELL-SIZE GRID-HEIGHT)))

(define PLAYER (square CELL-SIZE 'solid 'black))
(define BULLET (rectangle 3 8 'solid 'orange))
(define CENTIPEDE-CELL (square CELL-SIZE 'solid 'green))
(define TONGUE (triangle 5 'solid 'red))
(define LEFT-HEAD
  (overlay/align "left" "middle" (rotate 90 TONGUE) CENTIPEDE-CELL))
(define RIGHT-HEAD
  (overlay/align "right" "middle" (rotate 30 TONGUE) CENTIPEDE-CELL))
(define DOWN-HEAD
  (overlay/align "middle" "bottom" (rotate 60 TONGUE) CENTIPEDE-CELL))
(define UP-HEAD
  (overlay/align "middle" "top" (rotate 120 TONGUE) CENTIPEDE-CELL))


(define MUSHROOM-RADIUS (/ CELL-SIZE 2))
(define MUSHROOM-1-C 'LightSalmon)
(define MUSHROOM-2-C 'Salmon)
(define MUSHROOM-3-C 'OrangeRed)
(define MUSHROOM-4-C 'DarkRed)

(define WINNER (text "WINNER" 72 'black))
(define LOSER (text "LOSER" 72 'black))

; ++++++++++++++++++++++++++++++++++++++++++

; World Definitions

; A World is a (make-world LOC Player Bullet LOM)
(define-struct world (loc player bullet lom))

; A Centipede is a (make-cent direction LOP Number)
(define-struct cent (goal dir cpos count))

; A Player is a (make-player Posn)
(define-struct player (ppos))

; A Bullet is a (make-bullet Posn)
(define-struct bullet (bpos))

; A Mushroom is a (make-mush Posn Symbol)
(define-struct mush (mpos state))

; A LOP (list of posns) is one of:
; '()
; (cons Posn LOP)

; A dir is one of:
; "left"
; "right"
; "up"
; "down"

; A goal is one of:
; "up"
; "down"

; A Pdir is one of:
; "left"
; "right"
; "none"

; A LOM (list of Mushroom) is one of:
; - '()
; - (cons Mushroom LOM)

; A LOC (list of Centipedes) is one of:
; - '()
; - (cons Cent LOC)
; ++++++++++++++++++++++++++++++++++++++++++

; Templates

#;(define (world-temp w n)
    (... (loc-temp (world-loc w))
         (player-temp (world-player w))
         (bullet-temp (world-bullet w))
         (mush-temp (world-lom n))))

#;(define (cent-temp c)
    (...
     (goal-templ (cent-goal c))
     (dir-temp (cent-dir c))
     (lop-temp (cent-cpos c))
     ...(cent-count c)))

#;(define (player-temp p)
    (...(posn-temp(player-ppos p))))

#;(define (bullet-temp b)
    (... (posn-temp (bullet-bpos b))))

#;(define (mush-temp m)
    (... (posn-temp (mush-mpos m))
         (mush-state m)))

#;(define (dir-temp cd)
    (cond [(string=? cd "left") ...]
          [(string=? cd "right") ...]
          [(string=? cd "up") ...]
          [(string=? cd "down") ...]))

#;(define (goal-temp cd)
    (cond [(string=? cd "left") ...]
          [(string=? cd "right") ...]
          [(string=? cd "up") ...]
          [(string=? cd "down") ...]))

#;(define (pdir-temp pd)
    (cond [(string=? pd "left") ...]
          [(string=? pd "right") ...]))

#;(define (lop-temp segs)
    (cond [(empty? segs) ...]
          [(cons? segs) ... (first segs)
                        ... (los-temp (rest segs))]))

#;(define (posn-temp aposn)
    (... (posn-x aposn) ... (posn-y aposn) ...))

#;(define (lom-temp alom)
    (cond [(empty? alom) ...]
          [(cons? alom) ... (first alom)
                        ... (lom-temp (rest alom))]))
#; (define (loc-temp aloc)
     (cond [(empty? aloc) ...]
           [(cons? aloc)  (cent-temp (first aloc))
                          ... (loc-temp (rest aloc))]))
; ++++++++++++++++++++++++++++++++++++++++++

(define lom1 (cons (make-mush (make-posn 14 15) 'LightSalmon)
                   (cons (make-mush (make-posn 8 18) 'LightSalmon)
                         '())))
(define lom2 (cons (make-mush (make-posn 2 4) 'LightSalmon)
                   (cons (make-mush (make-posn 2 1) 'LightSalmon)
                         '())))

(define cent1 (make-cent "down" "right" (list (make-posn 4 1)
                                              (make-posn 3 1)
                                              (make-posn 2 1)
                                              (make-posn 1 1)) 4))
(define cent2 (make-cent "down" "left" (list (make-posn 10 1)
                                             (make-posn 9 1)
                                             (make-posn 8 1)
                                             (make-posn 7 1)) 0))
(define cent3 (make-cent "down" "right" (list (make-posn GRID-WIDTH 1)
                                              (make-posn (- GRID-WIDTH 1) 1)
                                              (make-posn (- GRID-WIDTH 2) 1)
                                              (make-posn (- GRID-WIDTH 3) 1)) 4))
(define cent4 (make-cent "down" "left" (list (make-posn 1 1)
                                             (make-posn 3 1)
                                             (make-posn 2 1)
                                             (make-posn 4 1)) 4))
(define cent5 (make-cent "down" "right" (list (make-posn (- GRID-WIDTH 1) 1)
                                              (make-posn (- GRID-WIDTH 2) 1)
                                              (make-posn (- GRID-WIDTH 3) 1)) 4))
(define cent6 (make-cent "down" "left" (list (make-posn 0 1)
                                             (make-posn 1 1)
                                             (make-posn 2 1)
                                             (make-posn 3 1)) 4))
(define cent7 (make-cent "down" "left" (list (make-posn 7 1)
                                             (make-posn 8 1)
                                             (make-posn 9 1)
                                             (make-posn 10 1)) 4))
(define cent8 (make-cent "down" "down" (list (make-posn (- GRID-WIDTH 1) 2)
                                             (make-posn (- GRID-WIDTH 1) 1)
                                             (make-posn (- GRID-WIDTH 2) 1)) 4))

(define cent9 (make-cent "down" "down" (list (make-posn 1 2)
                                             (make-posn 1 1)
                                             (make-posn 2 1)
                                             (make-posn 3 1)) 4))

(define cent10 (make-cent "up" "right" (list (make-posn 4 1)
                                             (make-posn 3 1)
                                             (make-posn 2 1)
                                             (make-posn 1 1)) 4))

(define cent11 (make-cent "up" "left" (list (make-posn 7 1)
                                            (make-posn 8 1)
                                            (make-posn 9 1)
                                            (make-posn 10 1)) 4))
(define cent12 (make-cent "down" "right" (list (make-posn (- GRID-WIDTH 1) (- GRID-HEIGHT 1))
                                               (make-posn (- GRID-WIDTH 2) (- GRID-HEIGHT 1))
                                               (make-posn (- GRID-WIDTH 3) (- GRID-HEIGHT 1))) 4))
(define cent13 (make-cent "up" "up" (list (make-posn (- GRID-WIDTH 1) (- GRID-HEIGHT 2))
                                          (make-posn (- GRID-WIDTH 1) (- GRID-HEIGHT 1))
                                          (make-posn (- GRID-WIDTH 2) (- GRID-HEIGHT 1))) 4))
(define cent14 (make-cent "down" "left" (list (make-posn 0 (- GRID-HEIGHT 1))
                                              (make-posn 1 (- GRID-HEIGHT 1))
                                              (make-posn 2 (- GRID-HEIGHT 1))) 4))
(define cent15 (make-cent "up" "up" (list (make-posn 0 (- GRID-HEIGHT 2))
                                          (make-posn 0 (- GRID-HEIGHT 1))
                                          (make-posn 1 (- GRID-HEIGHT 1))) 4))

; go right, hit, switch goal up, go left, opposite for left

(define bullet1 (make-bullet (make-posn -1 -1)))
(define bullet2 (make-bullet (make-posn 2 1)))

(define world0 (make-world (list (make-cent "down" "right" (list (make-posn 2 0)
                                                                 (make-posn 1 0)
                                                                 (make-posn 0 0)) 0))
                           (make-player (make-posn 12 39))
                           (make-bullet (make-posn -1 -1))
                           lom2))
(define world1 (make-world (list (make-cent "down" "right" (list (make-posn 5 0)
                                                                 (make-posn 4 0)
                                                                 (make-posn 3 0)
                                                                 (make-posn 2 0)
                                                                 (make-posn 1 0)
                                                                 (make-posn 0 0)) 0))
                           (make-player (make-posn 12 39))
                           (make-bullet (make-posn -1 -1))
                           (list (make-mush (make-posn 13 14) 'LightSalmon))))
(define world2 (make-world (list (make-cent "down" "right" (list (make-posn 12 20)
                                                                 (make-posn 11 20)
                                                                 (make-posn 10 20)) 0))
                           (make-player (make-posn 12 39))
                           (make-bullet (make-posn 6 20))
                           (list (make-mush (make-posn 13 14) 'LightSalmon))))
(define world3 (make-world (list (make-cent "down" "right" (list (make-posn 12 20)
                                                                 (make-posn 11 20)
                                                                 (make-posn 10 20)) 0))
                           (make-player (make-posn 12 39))
                           (make-bullet (make-posn 12 20))
                           (make-mush (make-posn 13 14) 'LightSalmon)))
(define world4 (make-world (list (make-cent "down" "right" (list (make-posn 12 39)
                                                                 (make-posn 11 39)
                                                                 (make-posn 10 39)) 0))
                           (make-player (make-posn 12 39))
                           (make-bullet (make-posn 6 20))
                           (list (make-mush (make-posn 13 14) 'LightSalmon))))

(define player1 (make-player (make-posn 0 GRID-HEIGHT)))
(define player2 (make-player (make-posn (- GRID-WIDTH 1) GRID-HEIGHT)))

(define mush1 (make-mush (make-posn 12 5) 'LightSalmon))
(define mush2 (make-mush (make-posn 2 1) 'Salmon))


; ++++++++++++++++++++++++++++++++++++++++++

; main : World -> World
; launch the snake game

(define (main w n)
  (big-bang (world-gen w n)
            [to-draw draw-world]
            [on-tick update-world .005]
            [on-key move-shoot-player]
            [stop-when end end-screen]))

; world-gen : Number Number -> World
; creates a world from with centipede length w, number of mushrooms n

; NEEDS TO BE UPDATED TO INCLUDE MUSHROOMS

(check-random (world-gen 3 2) (make-world (list (make-cent "down" "right"
                                                           (list (make-posn 2 0)
                                                                 (make-posn 1 0)
                                                                 (make-posn 0 0)) 0))
                                          (make-player (make-posn 12 39))
                                          (make-bullet (make-posn -1 -1))
                                          (lom-gen 2)))
(check-random (world-gen 7 3) (make-world (list (make-cent "down" "right"
                                                           [list (make-posn 6 0)
                                                                 (make-posn 5 0)
                                                                 (make-posn 4 0)
                                                                 (make-posn 3 0)
                                                                 (make-posn 2 0)
                                                                 (make-posn 1 0)
                                                                 (make-posn 0 0)] 0))
                                          (make-player (make-posn 12 39))
                                          (make-bullet (make-posn -1 -1))
                                          (lom-gen 3)))

(define (world-gen w n)
  (make-world (cent-gen w)
              (make-player (make-posn 12 39))
              (make-bullet (make-posn -1 -1))
              (lom-gen n)))

; cent-gen : Number -> Centipede
; generates list of 1 centipede of input length

(check-expect (cent-gen 3) (list (make-cent "down" "right" (list (make-posn 2 0)
                                                                 (make-posn 1 0)
                                                                 (make-posn 0 0)) 0)))
(check-expect (cent-gen 7) (list (make-cent "down" "right" [list (make-posn 6 0)
                                                                 (make-posn 5 0)
                                                                 (make-posn 4 0)
                                                                 (make-posn 3 0)
                                                                 (make-posn 2 0)
                                                                 (make-posn 1 0)
                                                                 (make-posn 0 0)] 0)))

(define (cent-gen w)
  (cons (make-cent "down" "right" (make-posn-list w) 0) empty))

; make-posn-list : Number -> LOP
; generates list of posns from input number n

(check-expect (make-posn-list 3) (list (make-posn 2 0)
                                       (make-posn 1 0)
                                       (make-posn 0 0)))
(check-expect (make-posn-list 7) [list (make-posn 6 0)
                                       (make-posn 5 0)
                                       (make-posn 4 0)
                                       (make-posn 3 0)
                                       (make-posn 2 0)
                                       (make-posn 1 0)
                                       (make-posn 0 0)])

(define (make-posn-list w)
  (cond [(= w 0) '()]
        [else (cons (make-posn (sub1 w) 0)
                    (make-posn-list (sub1 w)))]))

; lom-gen : Number -> LOM
; generates a list of as many randomly placed mushrooms as the input number

(check-random (lom-gen 5)
              (list (make-mush (make-posn (random 25)
                                          (random 50))
                               'LightSalmon)
                    (make-mush (make-posn (random 25)
                                          (random 50))
                               'LightSalmon)
                    (make-mush (make-posn (random 25)
                                          (random 50))
                               'LightSalmon)
                    (make-mush (make-posn (random 25)
                                          (random 50))
                               'LightSalmon)
                    (make-mush (make-posn (random 25)
                                          (random 50))
                               'LightSalmon)))


(define (lom-gen n)
  (cond [(= 0 n) '()]
        [else (cons (make-mush
                     (make-posn
                      (random 25)
                      (random 50))
                     'LightSalmon)
                    (lom-gen (sub1 n)))]))


; end-screen : World -> Image
; outputs victory or defeat image when game ends
#|
(check-expect (end-screen world3) (place-image WINNER 175 250 BG))
(check-expect (end-screen world4) (place-image LOSER 175 250 BG))|#

(define (end-screen w)
  (cond
    [(empty? (world-loc w))
     (place-image WINNER 175 250 BG)]
    [(dead? (world-loc w) (world-player w))
     (place-image LOSER 175 250 BG)]
    [else draw-world]))

; end : World -> Boolean
; ends game and displays victor image or defeat image
; if bullet hits head or if centipede eats player

(check-expect (end world1) #false)
(check-expect (end world3) #false)
(check-expect (end world4) #true)

(define (end w)
  (cond
    [(cells-empty? (world-loc w)) true]
    [(dead? (world-loc w) (world-player w)) true]
    [else false]))

;; cells-empty? : LOC -> Boolean
;; determines if every cent is empty
(check-expect (cells-empty? (list (make-cent "right" "right"
                                             '() 5)
                                  (make-cent "up" "left"
                                             '() 5))) true)
(check-expect (cells-empty? (list cent1)) false)

(define (cells-empty? aloc)
  (cond [(empty? aloc) true]
        [(cons? aloc) (if (empty? (cent-cpos (first aloc)))
                          (cells-empty? (rest aloc))
                          false)]))

;; dead? : LOC Player -> Boolean
;; has the player been hit by a centipede

(define (dead? aloc p)
  (local ((define (posn=p g) (posn=? g (player-ppos p)))) 
    (cond [(empty? aloc) #false]
          [(empty? (cent-cpos (first aloc)))
           (dead? (rest aloc) p)]
          [else (or (ormap posn=p (cent-cpos (first aloc)))
                    (dead? (rest aloc) p))])))


; update-world: World -> World
; Updates location of centipede and bullet and player

; DOES NOT NEED TO BE UPDATED TO INCLUDE MUSHROOMS
#|
(check-expect (update-world world1) (make-world
                                     (list (make-cent
                                      "down" 
                                      "right"
                                      (list
                                       (make-posn 5 0)
                                       (make-posn 4 0)
                                       (make-posn 3 0)
                                       (make-posn 2 0)
                                       (make-posn 1 0)
                                       (make-posn 0 0)) 1))
                                     (make-player (make-posn 12 39))
                                     (make-bullet (make-posn -1 -1))
                                     (update-alom (world-lom world1)
                                                  (make-bullet (make-posn -1 -1)))))|#

(define (update-world w)
  #|(if (collision? (world-bullet w) (world-loc w))
      (make-world (update-loc (split-cent (world-loc w) (world-bullet w)) (world-bullet w))
                  (update-player (world-player w))
                  (update-bullet (world-bullet w) (world-loc w))
                  (cons (new-mush (world-bullet w)) (world-lom w)))
       (make-world (update-loc (world-loc w) (world-bullet w))
                   (update-player (world-player w))
                   (update-bullet (world-bullet w) (world-loc w))
                   (update-alom (world-lom w) (world-bullet w))))|#
  (make-world (update-loc (world-loc w) (world-bullet w) (world-lom w))
              (update-player (world-player w))
              (update-bullet (world-bullet w) (world-loc w))
              (update-alom (world-lom w) (world-loc w) (world-bullet w))))

; new-mush: Posn LOM -> LOM
; adds new mushroom where bullet hits cent to the LOM

(check-expect (new-mush (make-posn (posn-x (bullet-bpos bullet2))
                                   (posn-y (bullet-bpos bullet2))) lom1) (cons (make-mush (make-posn 2 1) 'LightSalmon)
                                                                               (cons (make-mush (make-posn 14 15) 'LightSalmon)
                                                                                     (cons (make-mush (make-posn 8 18) 'LightSalmon)
                                                                                           '()))))
(define (new-mush pos alom)
  (cons (make-mush (make-posn (posn-x pos)
                              (posn-y pos))
                   'LightSalmon)
        alom))

; split-cent: LOC Bullet -> LOC
; splits cent if hit by bullet.

(check-expect (split-cent (list cent1) bullet2)
              (list (make-cent "down" "right" (list (make-posn 1 1)) 5)
                    (make-cent "down" "right" (list (make-posn 4 1)
                                                    (make-posn 3 1)) 5)))
(check-expect (split-cent (list (make-cent "down" "right" (list (make-posn 4 3)
                                                                (make-posn 3 3)) 4))
                          (make-bullet (make-posn 4 3)))
              (list (make-cent "down" "right" (list (make-posn 3 3)) 5)))
(check-expect (split-cent (list (make-cent "down" "right" (list (make-posn 4 3)
                                                                (make-posn 3 3)) 4))
                          (make-bullet (make-posn 3 3)))
              (list (make-cent "down" "right" (list (make-posn 4 3)) 5)))
(check-expect (split-cent (list (make-cent "down" "right" (list (make-posn 5 3)
                                                                (make-posn 4 3)
                                                                (make-posn 3 3)) 4))
                          (make-bullet (make-posn 4 3)))
              (list (make-cent "down" "right" (list (make-posn 3 3)) 5)
                    (make-cent "down" "right" (list (make-posn 5 3)) 5)))

(define (split-cent aloc b)
  (cond
    [(hit-head? (cent-cpos (first aloc)) b) (cons (make-cent
                                                   (cent-goal (first aloc))
                                                   (cent-dir (first aloc))
                                                   (rest (cent-cpos (first aloc)))
                                                   (add1 (cent-count (first aloc))))
                                                  (rest aloc))]
    [(hit-end? (cent-cpos (first aloc)) b) (cons (make-cent
                                                  (cent-goal (first aloc))
                                                  (cent-dir (first aloc))
                                                  (remove-other-segs (cent-cpos (first aloc))
                                                                     (bullet-bpos b))
                                                  (add1 (cent-count (first aloc))))
                                                 (rest aloc))]
    [else (cons (make-cent (cent-goal (first aloc))
                           (cent-dir (first aloc))
                           (rest-of-posns (cent-cpos (first aloc))
                                          (bullet-bpos b))
                           (add1 (cent-count (first aloc))))
                (cons (make-cent (cent-goal (first aloc))
                                 (cent-dir (first aloc))
                                 (remove-other-segs (cent-cpos (first aloc))
                                                    (bullet-bpos b))
                                 (add1 (cent-count (first aloc))))
                      (rest aloc)))]))

;; hit-head? : LOP Bullet -> Boolean
;; checks if bullet hits centipede head
(check-expect (hit-head? empty bullet2) false)
(check-expect (hit-head? (list (make-posn 4 5)
                               (make-posn 3 5))
                         (make-bullet (make-posn 3 5))) false)
(check-expect (hit-head? (list (make-posn 3 5)
                               (make-posn 4 5))
                         (make-bullet (make-posn 3 5))) true)

(define (hit-head? alop b)
  (cond [(empty? alop) false]
        [(cons? alop) (if (posn=? (bullet-bpos b) (first alop))
                          true
                          false)]))

;; hit-end? : LOP Bullet -> Boolean
;; checks if bullet hits last posn
(check-expect (hit-end? empty bullet2) false)
(check-expect (hit-end? (list (make-posn 4 5)
                              (make-posn 3 5))
                        (make-bullet (make-posn 3 5))) true)
(check-expect (hit-end? (list (make-posn 3 5)
                              (make-posn 4 5))
                        (make-bullet (make-posn 3 5))) false)

(define (hit-end? alop b)
  (cond [(empty? alop) false]
        [(cons? alop) (if (= (length alop) 1)
                          (posn=? (bullet-bpos b) (first alop))
                          (hit-end? (rest alop) b))]))

;; rest-of-posns : LOP Posn -> LOP
;; returns everything after a given posn
(check-expect (rest-of-posns empty (make-posn 5 5)) empty)
(check-expect (rest-of-posns (list (make-posn 3 0)
                                   (make-posn 4 0)
                                   (make-posn 5 0))
                             (make-posn 3 0))
              (list (make-posn 4 0)
                    (make-posn 5 0)))
(check-expect (rest-of-posns (list (make-posn 1 1)
                                   (make-posn 2 1)
                                   (make-posn 3 1)
                                   (make-posn 3 2))
                             (make-posn 2 1))
              (list (make-posn 3 1)
                    (make-posn 3 2)))

(define (rest-of-posns alop p)
  (cond [(empty? alop) empty]
        [(cons? alop) (if (posn=? p (first alop))
                          (rest alop)
                          (rest-of-posns (rest alop) p))]))


(define (make-lop cpos b)
  (cond
    [(posn=? (bullet-bpos b) (first cpos))
     empty]
    [else (cons (first cpos)
                (make-lop (rest cpos) b))]))

; update-bullet: Bullet LOC -> Bullet
; updates position of bullet

(check-expect (update-bullet bullet2 (list cent3)) (make-bullet (make-posn 2 0)))
(check-expect (update-bullet bullet2 (list cent4)) bullet1)

(define (update-bullet b aloc)
  (cond
    [(empty? aloc) b]
    [(collision? b aloc) (make-bullet (make-posn -1 -1))]
    [(on-screen? b) (move-bullet b)]
    [else
     (make-bullet (make-posn (posn-x (bullet-bpos b))
                             (posn-y (bullet-bpos b))))]))

; collision? : bullet LOC -> boolean
; determines if the bullet has hit any cent (centipede)

(check-expect (collision? bullet2 (list cent1)) #true)
(check-expect (collision? bullet1 (list cent1)) #false)
(check-expect (collision? bullet2 empty) #false)

(define (collision? b aloc)
  (cond
    [(empty? aloc) false]
    [(cons? aloc) (or (collision-cent? b (first aloc))
                      (collision? b (rest aloc)))]))


; collision-cent: bullet cent -> Boolean
; checks if the bullet has hit any part of a cent

(check-expect (collision-cent? bullet2 cent1) #true)
(check-expect (collision-cent? bullet1 cent1) #false)


(define (collision-cent? b c)
  (cond
    [(empty? (cent-cpos c)) #false]
    [(cons? (cent-cpos c)) (or (posn=? (bullet-bpos b) (first (cent-cpos c)))
                               (collision-cent? b (make-cent
                                                   (cent-goal c)
                                                   (cent-dir c)
                                                   (rest (cent-cpos c))
                                                   (cent-count c))))]))

; on-screen?: bullet -> boolean
; determines if bullet is on the screen

(check-expect (on-screen? bullet1) #false)
(check-expect (on-screen? bullet2) #true)

(define (on-screen? b)
  (> (posn-y (bullet-bpos b)) -1))

; update-loc: LOC bullet LOM -> LOC
; updates position of all centipedes

#|
(check-expect (update-loc (cons cent1 (cons cent7 '())) bullet1 (list
                                                      (make-mush
                                                       (make-posn 2 2)
                                                       'LightSalmon)))
              (list (make-cent "down" "right" (list (make-posn 5 1)
                                              (make-posn 4 1)
                                              (make-posn 3 1)
                                              (make-posn 2 1)) 5)
                    (make-cent "down" "left" (list (make-posn 6 1)
                                             (make-posn 7 1)
                                             (make-posn 8 1)
                                             (make-posn 9 1)) 5)))|#
(check-expect (update-loc empty bullet1 (list
                                         (make-mush
                                          (make-posn 2 2)
                                          'LightSalmon))) empty)

(define (update-loc aloc b alom)
  (cond
    [(empty? aloc) empty]
    [else (if (collision-cent? b (first aloc))
              (split-cent aloc b)
              (cons (update-cent (first aloc) b alom)
                    (update-loc (rest aloc) b alom)))]))



; update-cent : cent bullet LOM -> cent
; updates the position of the centipede and adds mushroom if there is a collision
; THIS IS WHERE CENT HITS MUSH IS TAKEN INTO ACCOUNT 

(check-expect (update-cent cent1 bullet1 (list (make-mush (make-posn 5 5)
                                                          'LightSalmon)))
              (make-cent "down" "right" (list (make-posn 5 1)
                                              (make-posn 4 1)
                                              (make-posn 3 1)
                                              (make-posn 2 1)) 5))
(check-expect (update-cent cent7 bullet1 (list (make-mush (make-posn 5 5)
                                                          'LightSalmon)))
              (make-cent "down" "left" (list (make-posn 6 1)
                                             (make-posn 7 1)
                                             (make-posn 8 1)
                                             (make-posn 9 1)) 5))
(check-expect (update-cent cent5 bullet1 (list (make-mush (make-posn 5 5)
                                                          'LightSalmon)))
              (make-cent "down" "down" (list (make-posn (- GRID-WIDTH 1) 2)
                                             (make-posn (- GRID-WIDTH 1) 1)
                                             (make-posn (- GRID-WIDTH 2) 1)) 5))
(check-expect (update-cent cent6 bullet1 (list (make-mush (make-posn 5 5)
                                                          'LightSalmon)))
              (make-cent "down" "down" (list (make-posn 0 2)
                                             (make-posn 0 1)
                                             (make-posn 1 1)
                                             (make-posn 2 1)) 5))
(check-expect (update-cent cent8 bullet1 (list (make-mush (make-posn 5 5)
                                                          'LightSalmon)))
              (make-cent "down" "left" (list (make-posn (- GRID-WIDTH 2) 2)
                                             (make-posn (- GRID-WIDTH 1) 2)
                                             (make-posn (- GRID-WIDTH 1) 1)) 5))
#;(check-expect (update-cent cent9 bullet1 (list (make-mush (make-posn 5 5)
                                                            'LightSalmon)))
                (make-cent "down" "right" (list (make-posn 1 3)
                                                (make-posn 1 2)
                                                (make-posn 1 1)
                                                (make-posn 2 1)) 5))
(check-expect (update-cent cent10 bullet1 (list (make-mush (make-posn 5 5)
                                                           'LightSalmon)))
              (make-cent "up" "right" (list (make-posn 5 1)
                                            (make-posn 4 1)
                                            (make-posn 3 1)
                                            (make-posn 2 1)) 5))
(check-expect (update-cent cent11 bullet1 (list (make-mush (make-posn 5 5)
                                                           'LightSalmon)))
              (make-cent "up" "left" (list (make-posn 6 1)
                                           (make-posn 7 1)
                                           (make-posn 8 1)
                                           (make-posn 9 1)) 5))
(check-expect (update-cent cent12 bullet1 (list (make-mush (make-posn 5 5)
                                                           'LightSalmon)))
              (make-cent "down" "down" (list (make-posn (- GRID-WIDTH 1) (- GRID-HEIGHT 0))
                                             (make-posn (- GRID-WIDTH 1) (- GRID-HEIGHT 1))
                                             (make-posn (- GRID-WIDTH 2) (- GRID-HEIGHT 1))) 5))
(check-expect (update-cent cent13 bullet1 (list (make-mush (make-posn 5 5)
                                                           'LightSalmon)))
              (make-cent "up" "left" (list (make-posn (- GRID-WIDTH 2) (- GRID-HEIGHT 2))
                                           (make-posn (- GRID-WIDTH 1) (- GRID-HEIGHT 2))
                                           (make-posn (- GRID-WIDTH 1) (- GRID-HEIGHT 1))) 5))
(check-expect (update-cent cent14 bullet1 (list (make-mush (make-posn 5 5)
                                                           'LightSalmon)))
              (make-cent "down" "down" (list (make-posn 0 (- GRID-HEIGHT 0))
                                             (make-posn 0 (- GRID-HEIGHT 1))
                                             (make-posn 1 (- GRID-HEIGHT 1))) 5))
(check-expect (update-cent cent15 bullet1 (list (make-mush (make-posn 5 5)
                                                           'LightSalmon)))
              (make-cent "up" "right" (list (make-posn 1 (- GRID-HEIGHT 2))
                                            (make-posn 0 (- GRID-HEIGHT 2))
                                            (make-posn 0 (- GRID-HEIGHT 1))) 5))

(define (update-cent c b alom)
  (if (= (modulo (cent-count c) 3) 1)
      (move-cent
       (cond
         [(and (and (hit-right-down? c) (= (posn-y (first (cent-cpos c)))
                                           (- GRID-HEIGHT 3)))
               (string=? "right" (get-dir c)))
          (make-cent "up" "up" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (and (hit-right-up? c) (= (posn-y (first (cent-cpos c)))
                                         (- GRID-HEIGHT 2)))
               (string=? "up" (get-dir c)))
          (make-cent "up" "left" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (and (hit-left-down? c) (= (posn-y (first (cent-cpos c)))
                                          (- GRID-HEIGHT 3)))
               (string=? "left" (get-dir c)))
          (make-cent "up" "up" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (and (hit-left-up? c) (= (posn-y (first (cent-cpos c)))
                                        (- GRID-HEIGHT 2)))
               (string=? "up" (get-dir c)))
          (make-cent "up" "right" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (and (hit-right-up? c) (= (posn-y (first (cent-cpos c))) 0))
               (string=? "right" (get-dir c)))
          (make-cent "down" "down" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (and (hit-right-down? c) (= (posn-y (first (cent-cpos c))) 1))
               (string=? "down" (get-dir c)))
          (make-cent "down" "left" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (and (hit-left-up? c) (= (posn-y (first (cent-cpos c))) 0))
               (string=? "left" (get-dir c)))
          (make-cent "down" "down" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (and (hit-left-down? c) (= (posn-y (first (cent-cpos c))) 1))
               (string=? "down" (get-dir c)))
          (make-cent "down" "right" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (hit-right-down? c) (string=? "right" (get-dir c)))
          (make-cent "down" "down" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (hit-right-down? c) (string=? "down" (get-dir c)))
          (make-cent "down" "left" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (hit-left-down? c) (string=? "left" (get-dir c)))
          (make-cent "down" "down" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (hit-left-down? c) (string=? "down" (get-dir c)))
          (make-cent "down" "right" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (hit-right-up? c) (string=? "right" (get-dir c)))
          (make-cent "up" "up" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (hit-right-up? c) (string=? "up" (get-dir c)))
          (make-cent "up" "left" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (hit-left-up? c) (string=? "left" (get-dir c)))
          (make-cent "up" "up" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (hit-left-up? c) (string=? "up" (get-dir c)))
          (make-cent "up" "right" (cent-cpos c) (+ (cent-count c) 1))]
         [(string=? "left" (get-dir c))
          (make-cent (cent-goal c)"left" (cent-cpos c) (+ (cent-count c) 1))]
         [(string=? "right" (get-dir c))
          (make-cent (cent-goal c)"right" (cent-cpos c) (+ (cent-count c) 1))]
         [(and (hit-mush-cent? c alom)
               (or (string=? "right" (cent-dir c))
                   (string=? "left" (cent-dir c))))
          (make-cent
           (cent-goal c)
           (cent-goal c)
           (cent-cpos c) (+ (cent-count c) 1))]
         [(or (string=? "down" (cent-dir c))
              (string=? "up" (cent-dir c)))
          (make-cent
           (cent-goal c)
           (hit-mush-dir c alom) (cent-cpos c) (+ (cent-count c) 1))]
         [(or (string=? "left" (cent-dir c))
              (string=? "right" (cent-dir c)))
          (make-cent (cent-goal c) (cent-dir c) (cent-cpos c)
                     (+ (cent-count c) 1))]) alom)
      (make-cent (cent-goal c) (cent-dir c) (cent-cpos c)
                 (+ (cent-count c) 1))))



; hit-mush-cent? : Cent LOM -> Boolean
; Tells you if cent hit a mush
(check-expect (hit-mush-cent? (make-cent "down" "right"
                                         (list (make-posn 4 0)) 0)
                              (list (make-mush (make-posn 3 0) 'LightSalmon)
                                    (make-mush (make-posn 25 30) 'LightSalmon)))
              #false)
(check-expect (hit-mush-cent? (make-cent "down" "left" (list (make-posn 4 0)) 0)
                              empty) false)
(check-expect (hit-mush-cent? (make-cent "down" "left" (list (make-posn 4 0)) 0)
                              (list (make-mush (make-posn 12 12) 'Salmon)
                                    (make-mush (make-posn 3 0) 'LightSalmon)))
              #true)

(define (hit-mush-cent? cent alom)
  (cond [(empty? alom) #false]
        [(cons? alom) (cond [(empty? (cent-cpos cent)) #false]
                            [else (if (and (posn=? (first (cent-cpos cent))
                                                   (make-posn (- (posn-x (mush-mpos (first alom))) 1)
                                                              (posn-y (mush-mpos (first alom)))))
                                           (string=? (cent-dir cent) "right"))
                                      #true
                                      (if (and (posn=? (first (cent-cpos cent))
                                                       (make-posn (+ (posn-x (mush-mpos (first alom))) 1)
                                                                  (posn-y (mush-mpos (first alom)))))
                                               (string=? (cent-dir cent) "left"))
                                          #true
                                          (hit-mush-cent? cent (rest alom))))])]))




;; hit-mush-dir : Cent LOM -> Dir
;; returns directions of centipede base of mush position

(check-expect (hit-mush-dir cent1 empty) (cent-dir cent1))
(check-expect (hit-mush-dir (make-cent "down" "down" (list (make-posn 20 2)
                                                           (make-posn 20 1)
                                                           (make-posn 21 1)) 1)
                            (list (make-mush (make-posn 19 1) 'LightSalmon)))
              "right")
(check-expect (hit-mush-dir (make-cent "down" "down" (list (make-posn 5 5)
                                                           (make-posn 5 4)
                                                           (make-posn 4 4)) 1)
                            (list (make-mush (make-posn 6 4) 'LightSalmon)))
              "left")

(define (hit-mush-dir c alom)
  (cond [(empty? alom) (cent-dir c)]
        [(and (cons? alom)
              (posn-left? (mush-mpos (first alom))
                          c))
         "right"]
        [(and (cons? alom)
              (posn-right? (mush-mpos (first alom))
                           c))
         "left"]
        [else (hit-mush-dir c (rest alom))]))

;; posn-left? : Posn Cent -> Boolean
;; checks if mush was to left of centipede

(check-expect (posn-left? (make-posn 18 1) (make-cent "down" "down"
                                                      (list (make-posn 19  2)
                                                            (make-posn 19  1)
                                                            (make-posn 20  1)) 1))
              #true)
(check-expect (posn-left? (make-posn 20 1) (make-cent "down" "down"
                                                      (list (make-posn 19  2)
                                                            (make-posn 19  1)
                                                            (make-posn 20  1)) 1))
              #false)

(define (posn-left? pos c)
  (if (or (posn=? (first (cent-cpos c))
                  (make-posn (+ (posn-x pos) 1)
                             (+ (posn-y pos) 1)))
          (posn=? (first (cent-cpos c))
                  (make-posn (+ (posn-x pos) 1)
                             (- (posn-y pos) 1))))
      #true
      #false))
;; posn-right? : Posn Cent -> Boolean
;; checks if mush was to right of centipede
(check-expect (posn-right? (make-posn 20 1) (make-cent "down" "down"
                                                       (list (make-posn 19  2)
                                                             (make-posn 19  1)
                                                             (make-posn 20  1)) 1))
              #true)
(check-expect (posn-right? (make-posn 18 1) (make-cent "down" "down"
                                                       (list (make-posn 19  2)
                                                             (make-posn 19  1)
                                                             (make-posn 20  1)) 1))
              #false)


(define (posn-right? pos c)
  (if (or (posn=? (first (cent-cpos c))
                  (make-posn (- (posn-x pos) 1)
                             (+ (posn-y pos) 1)))
          (posn=? (first (cent-cpos c))
                  (make-posn (- (posn-x pos) 1)
                             (- (posn-y pos) 1))))
      #true
      #false))



; remove-other-segs : LoPosn Posn -> LoPosn
; returns the cent after its segments that have been hit have been removed

(check-expect (remove-other-segs (cent-cpos cent1) (bullet-bpos bullet2))
              (list (make-posn 4 1)
                    (make-posn 3 1)))
(check-expect (remove-other-segs (cent-cpos cent1) (bullet-bpos bullet1))
              (list (make-posn 4 1)
                    (make-posn 3 1)
                    (make-posn 2 1)
                    (make-posn 1 1)))
(check-expect (remove-other-segs (list (make-posn 10 1)
                                       (make-posn 9 1)
                                       (make-posn 8 1)
                                       (make-posn 7 1)
                                       (make-posn 6 1)
                                       (make-posn 5 1)
                                       (make-posn 4 1)
                                       (make-posn 3 1)
                                       (make-posn 2 1)
                                       (make-posn 1 1)
                                       (make-posn 0 1))
                                 (make-posn 5 1))
              (list (make-posn 10 1)
                    (make-posn 9 1)
                    (make-posn 8 1)
                    (make-posn 7 1)
                    (make-posn 6 1)))



(define (remove-other-segs lop posn)
  (cond
    [(empty? lop) empty]
    [(cons? lop)
     (if (posn=? (first lop) posn)
         empty
         (cons
          (first lop)
          (remove-other-segs (rest lop) posn)))]
    [else lop]))            



; update-player : player -> player
; Re-draws player where it currently is for every time the world is updated

(check-expect (update-player (make-player (make-posn 12 39)))
              (make-player (make-posn 12 39)))


(define (update-player p)
  (make-player (make-posn (posn-x (player-ppos p))
                          (posn-y (player-ppos p)))))

; move-shoot-player: world keyevent -> world
; changes direction of the player and shoots bullet

(check-expect (move-shoot-player
               (make-world cent1
                           (make-player (make-posn 12 39))
                           bullet1
                           lom2) "left")
              (make-world cent1
                          (make-player
                           (make-posn 11 39)) bullet1
                                              lom2))
(check-expect (move-shoot-player
               (make-world cent1
                           (make-player (make-posn 12 39))
                           bullet1
                           lom2) "right")
              (make-world cent1
                          (make-player (make-posn 13 39))
                          bullet1
                          lom2))
(check-expect (move-shoot-player
               (make-world cent1
                           (make-player (make-posn 12 39))
                           bullet1
                           lom2) "up")
              (make-world cent1
                          (make-player (make-posn 12 39))
                          bullet1
                          lom2))
(check-expect (move-shoot-player world0 " ")
              (make-world (list (make-cent "down" "right" (list (make-posn 2 0)
                                                                (make-posn 1 0)
                                                                (make-posn 0 0)) 0))
                          (make-player (make-posn 12 39))
                          (make-bullet (make-posn 12 38))
                          lom2))



(define (move-shoot-player w ke)
  (cond
    [(and (not (hit-left-player? (world-player w))) (string=? ke "left"))
     (make-world (world-loc w)
                 (make-player
                  (make-posn (- (posn-x (player-ppos (world-player w))) 1)
                             (posn-y (player-ppos (world-player w)))))
                 (world-bullet w)
                 (world-lom w))]
    [(and (not (hit-right-player? (world-player w))) (string=? ke "right"))
     (make-world (world-loc w)
                 (make-player
                  (make-posn (+ (posn-x (player-ppos (world-player w))) 1)
                             (posn-y (player-ppos (world-player w)))))
                 (world-bullet w)
                 (world-lom w))]
    [(and (string=? ke " ") (not (on-screen? (world-bullet w))))
     (make-world (world-loc w)
                 (world-player w)
                 (make-bullet
                  (make-posn
                   (posn-x (player-ppos (world-player w)))
                   (- (posn-y (player-ppos (world-player w))) 1)))
                 (world-lom w))]
    [else w]))

; hit-right-player? Player -> Boolean
; checks if the player has hit a wall on the right

(check-expect (hit-right-player? player2)
              #true)
(check-expect (hit-right-player? player1)
              #false)

(define (hit-right-player? p)
  (= (posn-x (player-ppos p))
     (- GRID-WIDTH 1)))

; hit-left-player? : Player -> Boolean
; checks if the player has hit a wall on the left

(check-expect (hit-left-player? player2)
              #false)
(check-expect (hit-left-player? player1)
              #true)

(define (hit-left-player? p)
  (= (posn-x (player-ppos p))
     0))


; hit-right-down? Cent -> Boolean
; checks if the cent has hit a wall on the right and is heading down

(check-expect (hit-right-down? cent5)
              #true)
(check-expect (hit-right-down? cent4)
              #false)

(define (hit-right-down? c)
  (and (string=? "down" (cent-goal c))
       (= (posn-x (first (cent-cpos c)))
          (- GRID-WIDTH 1))))

; hit-left-down? Cent -> Boolean
; checks if the cent has hit a wall on the left and is heading down

(check-expect (hit-left-down? cent3)
              #false)
(check-expect (hit-left-down? cent6)
              #true)

(define (hit-left-down? c)
  (and (string=? "down" (cent-goal c))
       (= (posn-x (first (cent-cpos c)))
          0)))

; hit-right-up? Cent -> Boolean
; checks if the cent has hit a wall on the right and is heading up

(check-expect (hit-right-up? cent13)
              #true)
(check-expect (hit-right-up? cent4)
              #false)

(define (hit-right-up? c)
  (and (string=? "up" (cent-goal c))
       (= (posn-x (first (cent-cpos c)))
          (- GRID-WIDTH 1))))

; hit-left-up? Cent -> Boolean
; checks if the cent has hit a wall on the left and is heading up

(check-expect (hit-left-up? cent3)
              #false)
(check-expect (hit-left-up? cent15)
              #true)

(define (hit-left-up? c)
  (and (string=? "up" (cent-goal c))
       (= (posn-x (first (cent-cpos c)))
          0)))

; get-dir: Cent -> String
; checks direction of cent (centipede)

(check-expect (get-dir cent1)
              "right")
(check-expect (get-dir cent2)
              "left")
(check-expect (get-dir (make-cent "down" "down"
                                  (list (make-posn GRID-WIDTH 2)
                                        (make-posn GRID-WIDTH 1)
                                        (make-posn (- GRID-WIDTH 1) 1)
                                        (make-posn (- GRID-WIDTH 2) 1)) 0))
              "down")

(define (get-dir c)
  (cent-dir c))


; posn=? : Posn Posn -> Boolean
; Posns equal?

(check-expect (posn=? (make-posn 15 15) (make-posn 15 15)) #true)
(check-expect (posn=? (make-posn 15 15) (make-posn 15 14)) #false)

(define (posn=? a b)
  (and (= (posn-x a) (posn-x b))
       (= (posn-y a) (posn-y b))))

; drop-last : LoPosn LOM -> LoPosn
; Drop the last segment.

(check-expect (drop-last (list (make-posn 4 1)
                               (make-posn 3 1)
                               (make-posn 2 1)
                               (make-posn 1 1)))
              (list (make-posn 4 1)
                    (make-posn 3 1)
                    (make-posn 2 1)))
(check-expect (drop-last (list (make-posn 8 10)
                               (make-posn 9 10)))
              (list (make-posn 8 10)))

(define (drop-last xs)
  (cond [(empty? (rest xs)) empty]
        [(cons? xs) (cons (first xs)
                          (drop-last (rest xs)))]))

; add-head : Dir LoPosn LOM Goal -> LoPosn
; Add the head to the snake in the direction it's moving.

(check-expect (add-head "right" (list (make-posn 5 1)
                                      (make-posn 4 1)
                                      (make-posn 3 1)
                                      (make-posn 2 1))
                        (list (make-mush (make-posn 15 15) 'LightSalmon))
                        "up")
              (list (make-posn 6 1)
                    (make-posn 5 1)
                    (make-posn 4 1)
                    (make-posn 3 1)
                    (make-posn 2 1)))
(check-expect (add-head "left" (list (make-posn 9 1)
                                     (make-posn 8 1)
                                     (make-posn 7 1)
                                     (make-posn 6 1))
                        (list (make-mush (make-posn 15 15) 'LightSalmon))
                        "down")
              (list (make-posn 8 1)
                    (make-posn 9 1)
                    (make-posn 8 1)
                    (make-posn 7 1)
                    (make-posn 6 1)))
(check-expect (add-head "right" (list (make-posn 9 3)
                                      (make-posn 8 3)
                                      (make-posn 7 3)
                                      (make-posn 6 3))
                        (list (make-mush (make-posn 9 3) 'LightSalmon))
                        "down")
              (list (make-posn 9 4)
                    (make-posn 9 3)
                    (make-posn 8 3)
                    (make-posn 7 3)
                    (make-posn 6 3)))

(define (add-head d ps alom goal)
  (cond [(and (string=? goal "down") (is-hit? (first ps) alom d))
         (cons (make-posn (posn-x (first ps)) (add1 (posn-y (first ps)))) ps)]
        [(and (string=? goal "up") (is-hit? (first ps) alom d))
         (cons (make-posn (posn-x (first ps)) (sub1 (posn-y (first ps)))) ps)]
        [(string=? d "up") 
         (cons (make-posn (posn-x (first ps))
                          (- (posn-y (first ps)) 1))
               ps)]
        [(string=? d "down")
         (cons (make-posn (posn-x (first ps))
                          (+ (posn-y (first ps)) 1))
               ps)]
        [(string=? d "left") 
         (cons (make-posn (- (posn-x (first ps)) 1)
                          (posn-y (first ps)))
               ps)]
        [(string=? d "right") 
         (cons (make-posn (+ (posn-x (first ps)) 1)
                          (posn-y (first ps)))
               ps)]))

;; is-hit? Posn LOM -> Boolean
;; is the new head going to hit a mushroom

(define (is-hit? ps alom d)
  (ormap (λ (m) (posn=? ps (mush-mpos m))) alom))

; MAKE TESTS FOR UP
; move-cent: Cent LOM -> Cent
; Moves the centipede one grid in its current direction.

(check-expect (move-cent cent1 (list (make-mush (make-posn 15 15) 'LightSalmon)))
              (make-cent "down" "right" (list (make-posn 5 1)
                                              (make-posn 4 1)
                                              (make-posn 3 1)
                                              (make-posn 2 1))
                         4))
(check-expect (move-cent cent7 (list (make-mush (make-posn 15 15) 'LightSalmon)))
              (make-cent "down" "left" (list (make-posn 6 1)
                                             (make-posn 7 1)
                                             (make-posn 8 1)
                                             (make-posn 9 1))
                         4))
#; (check-expect (move-cent cent9 (list (make-mush (make-posn 15 15) 'LightSalmon)))
                 (make-cent "down" "right" (list (make-posn 1 3)
                                                 (make-posn 1 2)
                                                 (make-posn 1 1)
                                                 (make-posn 2 1)) 5))
#; (check-expect (move-cent cent8 (list (make-mush (make-posn 15 15) 'LightSalmon)))
                 (make-cent "down" "left" (list (make-posn (- GRID-WIDTH 2) 2)
                                                (make-posn (- GRID-WIDTH 1) 2)
                                                (make-posn (- GRID-WIDTH 1) 1)) 5))
; making 4 cases for UP HEREE



(define (move-cent c alom)
  (make-cent (cent-goal c)
             (cent-dir c)
             (drop-last (add-head (cent-dir c)
                                  (cent-cpos c) alom (cent-goal c))) (cent-count c)))


; place-image/grid : Image Number Number Image -> Image
; Places first image on last image based on grid numbers.

(check-expect (place-image/grid (circle 10 'solid 'red)
                                12 20
                                BG)
              (place-image (circle 10 'solid 'red)
                           187.5 307.5
                           BG))

(define (place-image/grid img x y bg)
  (place-image img
               (+ (* x CELL-SIZE) (/ CELL-SIZE 2))
               (+ (* y CELL-SIZE) (/ CELL-SIZE 2))
               bg))
; hit-cent-posn: LoPosn Bullet -> Posn
; Tells you where bullet hit cent
(check-expect (hit-cent-posn (list (make-posn 3 3)
                                   (make-posn 4 4))
                             (make-bullet (make-posn 4 4)))
              (make-posn 4 4))
(check-expect (hit-cent-posn (list (make-posn 3 3))
                             (make-bullet (make-posn 4 4)))
              empty)
(check-expect (hit-cent-posn empty bullet2) empty)

(define (hit-cent-posn lop b)
  (if (empty? (filter (λ (x) (posn=? (bullet-bpos b) x)) lop))
      empty
      (first (filter (λ (x) (posn=? (bullet-bpos b) x)) lop))))

; update-alom : LOM LOC Bullet -> LOM
; updates the list of mushrooms if any collisions occur

(check-expect (update-alom lom2 (list (make-cent "down" "down"
                                                 (list (make-posn 4 0))
                                                 1)) bullet2)
              (cons (make-mush (make-posn 2 4) 'LightSalmon)
                    (cons (make-mush (make-posn 2 1) 'Salmon)
                          '())))
(check-expect (update-alom lom2 empty (make-bullet (make-posn 1 0)))
              lom2)
(check-expect (update-alom lom2 (list cent1) (make-bullet (make-posn 4 1)))
              (cons (make-mush (make-posn 4 1) 'LightSalmon)
                    (cons (make-mush (make-posn 2 4) 'LightSalmon)
                          (cons (make-mush (make-posn 2 1) 'LightSalmon)
                                '()))))

(define (update-alom alom aloc b)
  (cond [(empty? aloc) alom]
        [(collision-cent? b (first aloc))
         (new-mush  (hit-cent-posn (cent-cpos (first aloc)) b) alom)]
        [(bullet-mush-hit? alom b) (new-mush-color alom b)]
        [else (update-alom alom (rest aloc) b)]))

; bullet-mush-hit? : LOM Bullet -> Boolean
; checks if a bullet has hit a mushroom

(check-expect (bullet-mush-hit? (list mush2) bullet2)
              #true)
(check-expect (bullet-mush-hit? (list mush1) bullet2)
              #false)
(check-expect (bullet-mush-hit? empty bullet1)
              #false)

(define (bullet-mush-hit? alom b)
  (ormap (λ (m) (posn=? (mush-mpos m) (bullet-bpos b))) alom))

; new-mush-color : LOM Bullet -> LOM
; changes mushroom color based on previous color if hit by
; a bullet, and adds it back to the list of mushrooms.

(check-expect (new-mush-color (list mush1) bullet2)
              (list (make-mush (make-posn 12 5) 'LightSalmon)))
(check-expect (new-mush-color (list mush2) bullet2)
              (list (make-mush (make-posn 2 1) 'OrangeRed)))
(check-expect (new-mush-color (list (make-mush (make-posn 2 2) 'DarkRed))
                              (make-bullet (make-posn 2 2)))
              (list (make-mush (make-posn -5 -5) 'White)))
(check-expect (new-mush-color empty bullet1) empty)

(define (new-mush-color alom b)
  (cond
    [(empty? alom) empty]
    [(cons? alom) (if (posn=? (mush-mpos (first alom)) (bullet-bpos b))
                      (cons
                       (cond [(symbol=? (mush-state (first alom)) 'LightSalmon)
                              (make-mush (mush-mpos (first alom)) 'Salmon)]
                             [(symbol=? (mush-state (first alom)) 'Salmon)
                              (make-mush (mush-mpos (first alom)) 'OrangeRed)]
                             [(symbol=? (mush-state (first alom)) 'OrangeRed)
                              (make-mush (mush-mpos (first alom)) 'DarkRed)]
                             [(symbol=? (mush-state (first alom)) 'DarkRed)
                              (make-mush (make-posn -5 -5) 'White)])
                       (new-mush-color (rest alom) b))
                      (cons
                       (first alom) (new-mush-color (rest alom) b)))]))

; draw-world : World -> Image
; draw the world onto the scene

(check-expect (draw-world world2)
              (place-images (list RIGHT-HEAD
                                  (square CELL-SIZE 'solid 'green)
                                  (square CELL-SIZE 'solid 'green)
                                  PLAYER
                                  BULLET
                                  (circle MUSHROOM-RADIUS 'solid 'LightSalmon))
                            (list (make-posn 187.5 307.5)
                                  (make-posn 172.5 307.5)
                                  (make-posn 157.5 307.5)
                                  (make-posn 187.5 592.5)
                                  (make-posn 97.5 307.5)
                                  (make-posn 202.5 217.5)) BG)) 

(define (draw-world w)
  (draw-loc (world-loc w)
            (draw-player (world-player w)
                         (draw-bullet (world-bullet w)
                                      (draw-mush (world-lom w) BG)))))

; draw-bullet: Bullet Image -> Image
; draws bullet onto background

(check-expect (draw-bullet bullet2 BG)
              (place-image BULLET 37.5 22.5 BG))

(define (draw-bullet b bg)
  (place-image/grid BULLET
                    (posn-x (bullet-bpos b))
                    (posn-y (bullet-bpos b))
                    bg))

; move-bullet: Bullet -> Bullet
; moves bullet up the screen after it is shot

(check-expect (move-bullet bullet2)
              (make-bullet (make-posn 2 0)))

(define (move-bullet b)
  (make-bullet (make-posn (posn-x (bullet-bpos b))
                          (- (posn-y (bullet-bpos b)) 1))))

; draw-loc: LOC Image -> Image
; draws list of centipedes onto the scene

(check-expect (draw-loc (list cent1) BG)
              (place-images (list RIGHT-HEAD
                                  (square CELL-SIZE 'solid 'green)
                                  (square CELL-SIZE 'solid 'green)
                                  (square CELL-SIZE 'solid 'green))
                            (list (make-posn 67.5 22.5)
                                  (make-posn 52.5 22.5)
                                  (make-posn 37.5 22.5)
                                  (make-posn 22.5 22.5))
                            BG))

(check-expect (draw-loc empty BG) BG)
#;(check-expect (draw-loc (list cent1 cent2) BG)
                (place-images (list RIGHT-HEAD
                                    (square CELL-SIZE 'solid 'green)
                                    (square CELL-SIZE 'solid 'green)
                                    (square CELL-SIZE 'solid 'green)
                                    (square CELL-SIZE 'solid 'green)
                                    (square CELL-SIZE 'solid 'green)
                                    (square CELL-SIZE 'solid 'green))
                              (list (make-posn 67.5 22.5)
                                    (make-posn 52.5 22.5)
                                    (make-posn 37.5 22.5)
                                    (make-posn 22.5 22.5)
                                    (make-posn 142.5 22.5)
                                    (make-posn 127.5 22.5)
                                    (make-posn 112.5 22.5))
                              BG))

(define (draw-loc aloc bg)
  (cond
    [(empty? aloc) bg]
    [else (draw-loc (rest aloc) (overlay
                                 (draw-cent (first aloc) bg)
                                 (draw-loc (rest aloc) bg)))]))



; draw-cent : Cent Image -> Image
; draws the centipede onto the scene

(check-expect (draw-cent cent1 BG)
              (place-images (list RIGHT-HEAD
                                  (square CELL-SIZE 'solid 'green)
                                  (square CELL-SIZE 'solid 'green)
                                  (square CELL-SIZE 'solid 'green))
                            (list (make-posn 67.5 22.5)
                                  (make-posn 52.5 22.5)
                                  (make-posn 37.5 22.5)
                                  (make-posn 22.5 22.5))
                            BG))

(define (draw-cent c bg)
  (draw-head c bg))

; draw-head : Cent Image -> Image
; draws head of cent to body of cent

(check-expect (draw-head cent1 BG)
              (place-images (list RIGHT-HEAD
                                  (square CELL-SIZE 'solid 'green)
                                  (square CELL-SIZE 'solid 'green)
                                  (square CELL-SIZE 'solid 'green))
                            (list (make-posn 67.5 22.5)
                                  (make-posn 52.5 22.5)
                                  (make-posn 37.5 22.5)
                                  (make-posn 22.5 22.5))
                            BG))
(check-expect (draw-head cent7 BG)
              (place-images (list LEFT-HEAD
                                  (square CELL-SIZE 'solid 'green)
                                  (square CELL-SIZE 'solid 'green)
                                  (square CELL-SIZE 'solid 'green))
                            (list (make-posn 112.5 22.5)
                                  (make-posn 127.5 22.5)
                                  (make-posn 142.5 22.5)
                                  (make-posn 157.5 22.5))
                            BG))

(define (draw-head c bg)
  (cond
    [(string=? "right" (cent-dir c))
     (place-image/grid RIGHT-HEAD
                       (posn-x (first (cent-cpos c)))
                       (posn-y (first (cent-cpos c)))
                       (draw-body (rest (cent-cpos c)) bg))]
    [(string=? "left" (cent-dir c))
     (place-image/grid LEFT-HEAD
                       (posn-x (first (cent-cpos c)))
                       (posn-y (first (cent-cpos c)))
                       (draw-body (rest (cent-cpos c)) bg))]
    [(string=? "down" (cent-dir c))
     (place-image/grid DOWN-HEAD
                       (posn-x (first (cent-cpos c)))
                       (posn-y (first (cent-cpos c)))
                       (draw-body (rest (cent-cpos c)) bg))]
    [(string=? "up" (cent-dir c))
     (place-image/grid UP-HEAD
                       (posn-x (first (cent-cpos c)))
                       (posn-y (first (cent-cpos c)))
                       (draw-body (rest (cent-cpos c)) bg))]))


; draw-body: LoPosns Image -> Image
; draws body of centipede (its segments) onto the background

(check-expect (draw-body (list (make-posn 4 1)
                               (make-posn 3 1)
                               (make-posn 2 1)
                               (make-posn 1 1)) BG)
              (place-images (list (square CELL-SIZE 'solid 'green)
                                  (square CELL-SIZE 'solid 'green)
                                  (square CELL-SIZE 'solid 'green)
                                  (square CELL-SIZE 'solid 'green))
                            (list (make-posn 67.5 22.5)
                                  (make-posn 52.5 22.5)
                                  (make-posn 37.5 22.5)
                                  (make-posn 22.5 22.5))
                            BG))

(define (draw-body loc bg)
  (cond
    [(empty? loc) bg]
    [(cons? loc) (place-image/grid CENTIPEDE-CELL
                                   (posn-x (first loc))
                                   (posn-y (first loc))
                                   (draw-body (rest loc) bg))]))

; draw-player: Player Image -> Image
; draws player onto the background image

(check-expect (draw-player
               (make-player
                (make-posn 1 1)) (empty-scene 200 200))
              (place-image/grid PLAYER
                                1
                                1
                                (empty-scene 200 200)))

(define (draw-player p bg)
  (place-image/grid PLAYER
                    (posn-x (player-ppos p))
                    (posn-y (player-ppos p))
                    bg))

; mush-img : Mushroom -> Image
; draws the image of the given mushroom

(check-expect (mush-img (make-mush (make-posn 12 13) 'LightSalmon))
              (circle MUSHROOM-RADIUS 'solid 'LightSalmon))

(define (mush-img m)
  (circle MUSHROOM-RADIUS 'solid (mush-state m)))

; draw-mush : LOM Image -> Image
; draws mushrooms onto the background image

(check-expect (draw-mush lom1 BG)
              (place-images (list (circle MUSHROOM-RADIUS 'solid 'LightSalmon)
                                  (circle MUSHROOM-RADIUS 'solid 'LightSalmon))
                            (list (make-posn 217.5 232.5)
                                  (make-posn 127.5 277.5))
                            BG))

(define (draw-mush alom bg)
  (cond [(empty? alom) BG]
        [(cons? alom) (place-image/grid (circle MUSHROOM-RADIUS 'solid (mush-state (first alom)))
                                        (posn-x (mush-mpos (first alom)))
                                        (posn-y (mush-mpos (first alom)))
                                        (draw-mush (rest alom) bg))]))










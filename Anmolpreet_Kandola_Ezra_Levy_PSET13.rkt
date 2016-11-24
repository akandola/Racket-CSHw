;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Anmolpreet_Kandola_Ezra_Levy_PSET13) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; PSET 13 - Accumulators

; Anmolpreet Kandola and Ezra Levy
; Anmolpreet: 001621744
; Ezra: 001624514

; ++++++++++++++++++++++++++++++++++++++++++

; Problem 1

; make-palindrome : String -> String
; makes a palindrome of the given string
; Precondition: string is not empty

(check-expect (make-palindrome "fundies") "fundieseidnuf")
(check-expect (make-palindrome "adogapanic") "adogapanicinapagoda")
(check-expect (make-palindrome "atoy") "atoyota")
(check-error (make-palindrome "") "empty string given")

(define (make-palindrome str)
  (cond [(string=? str "") (error "empty string given")]
        [else (string-append str
                        (substring (implode (reverse (explode str)))
                                   1
                                   (string-length str)))]))

; is-palindrome? : String -> Boolean
; determines if given string is palindrome

(check-expect (is-palindrome? "fundies") false)
(check-expect (is-palindrome? "fundieseidnuf") true)
(check-expect (is-palindrome? "adogapanicinapagoda") true)
(check-error (is-palindrome? "") "empty string given")

(define (is-palindrome? str)
  (cond [(string=? str "") (error "empty string given")]
        [else (string=? (make-palindrome (substring str 0
                                                    (ceiling (/ (string-length str) 2))))
                        str)]))

; ++++++++++++++++++++++++++++++++++++++++++

; Problem 2

; prime? : Natural -> Boolean
; is the given number a prime number?
; Precondition: number is 0 or above and
; is a whole number

(check-expect (prime? 0) false)
(check-expect (prime? 1) false)
(check-expect (prime? 3) true)
(check-expect (prime? 2) true)
(check-expect (prime? 4) false)
(check-expect (prime? 6) false)

(define (prime? nat)
  (cond
    [(or (= nat 0) (= nat 1)) false]
    [else (prime-helper nat (sub1 nat))]))

; prime-helper: Natural Number -> Boolean
; calculates if there is a factor of the number
; other than 1 that is less than its square root
; precondition: the natural is greater than 1
; (because those 2 cases are already covered in prime?


(check-expect (prime-helper 3 2) true)
(check-expect (prime-helper 2 1) true)
(check-expect (prime-helper 4 3) false)
(check-expect (prime-helper 6 5) false)

(define (prime-helper nat num)
  (cond
    [(= 1 num) #true]
    [(= 0 (modulo nat num)) #false]
    [else (prime-helper nat (sub1 num))]))

; list-primes: Natural -> [list-of Natural]
; takes a Natural, returns a list of numbers
; from 0 to the Natural (including Natural)
; which are prime
; Precondition: Natural is a number 0 or above and
; is a whole number

(check-expect (list-primes 0) empty)
(check-expect (list-primes 1) empty)
(check-expect (list-primes 2) (list 2))
(check-expect (list-primes 7) (list 2 3 5 7))
(check-expect (list-primes 10) (list 2 3 5 7))
(check-expect (list-primes 15) (list 2 3 5 7 11 13))

(define (list-primes nat)
  (list-primes-helper (build-list (add1 nat) (λ (x) x))))

; list-primes-helper: [list-of Natural] -> [list-of Natural]
; takes a list of Naturals from 0
; to the given Natural, filtering to keep
; naturals which are prime
; Precondition: Natural is a number 0 or above and
; is a whole number

(check-expect (list-primes-helper (list 0)) empty)
(check-expect (list-primes-helper (list 0 1)) empty)
(check-expect (list-primes-helper (list 0 1 2)) (list 2))
(check-expect (list-primes-helper
               (list 0 1 2 3 4 5 6 7))
              (list 2 3 5 7))
(check-expect (list-primes-helper
               (list 0 1 2 3 4 5 6 7 8 9 10))
              (list 2 3 5 7))
(check-expect (list-primes-helper
               (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
               (list 2 3 5 7 11 13))

(define (list-primes-helper lonat)
  (filter prime? lonat))

; ++++++++++++++++++++++++++++++++++++++++++

; Problem 3

; A Game is a [List-of Player]
; constraint : A Game must have four players,
;              and all players must have the same number of cards

; A Player is a (make-player String [List-of Card] Number)
(define-struct player (name loc score))

; A Card is a (make-card Value Symbol)
(define-struct card (value suit))

; A Value is one of:
; - Number (constraint: Natural number between 2 and 10)
; - jack
; - queen
; - king
; - ace
; note : listed in increasing order of value, ace being the most valuable

(define jack 11)
(define queen 12)
(define king 13)
(define ace 14)

; A Suit is one of:
; - club
; - diamond
; - heart
; - spade

; Suit Value : Clubs < Diamonds < Hearts < Spades

(define club 1)
(define diamond 2)
(define heart 3)
(define spade 4)

; constraint : there is only one of every combination of value and suit
; in order to avoid ties and to reflect the reality of playing with a deck of cards

; trick-winner : [List-of Card] -> Card

(check-expect (trick-winner (list (make-card 10 spade)
                                  (make-card jack heart)
                                  (make-card 2 club)
                                  (make-card 5 diamond))) (make-card 11 3))
(check-expect (trick-winner (list (make-card 10 spade)
                                  (make-card 10 heart)
                                  (make-card 2 club)
                                  (make-card 5 diamond))) (make-card 10 4))
(check-expect (trick-winner (list (make-card 10 spade)
                                  (make-card jack heart)
                                  (make-card 2 club)
                                  (make-card jack diamond))) (make-card 11 3))
;(check-expect (trick-winner '()) '()) ; Empty case??

(define (trick-winner loc)
  (local [;; [List-of Card] Card -> Card
          ;; accumulator : the winningest card so far
          (define (win aloc acc)
            (cond [(empty? aloc) acc]
                  [else (cond [(< (card-value (first aloc)) (card-value acc))
                               (win (rest aloc) acc)]
                              [(> (card-value (first aloc)) (card-value acc))
                               (win (rest aloc) (first aloc))]
                              [(< (card-suit (first aloc)) (card-value acc))
                               (win (rest aloc) acc)]
                              [else (win (rest aloc) (first aloc))])]))]
    (win loc (first loc))))

; play-game : Game -> Game
; plays the game (based on the rules outlined above) and returns the winning player

(check-expect (play-game (list (make-player "Anmol" (list (make-card king spade)
                                                          (make-card 4 heart)) 0)
                               (make-player "Alec" (list (make-card 3 heart)
                                                         (make-card jack spade)) 0)
                               (make-player "Calvin" (list (make-card ace spade)
                                                           (make-card ace club)) 0)
                               (make-player "Hobbes" (list (make-card 8 heart)
                                                           (make-card king diamond)) 0)))
              (list (make-player "Anmol" '() 0)
                    (make-player "Alec" '() 0)
                    (make-player "Calvin" '() 2)
                    (make-player "Hobbes" '() 0)))
(check-expect (play-game (list (make-player "Anmol" (list (make-card king spade)
                                                          (make-card ace heart)) 0)
                               (make-player "Alec" (list (make-card 3 heart)
                                                         (make-card jack spade)) 0)
                               (make-player "Calvin" (list (make-card ace spade)
                                                           (make-card ace club)) 0)
                               (make-player "Hobbes" (list (make-card 8 heart)
                                                           (make-card king diamond)) 0)))
              (list (make-player "Anmol" '() 1)
                    (make-player "Alec" '() 0)
                    (make-player "Calvin" '() 1)
                    (make-player "Hobbes" '() 0)))

(define (play-game lop)
  (local [(define (win2 alop cards)
            (cond [(empty? cards) alop]
                  [else (win2 (map (λ(x) (who-won? x (first cards))) alop) (rest cards))]))]
    (win2 lop (winners lop))))


;; winners : [List-of Player] -> [List-of Card]
;; creates a list of winning cards
(check-expect (winners (list (make-player "Anmol" (list (make-card king spade)
                                                        (make-card ace heart)) 0)
                             (make-player "Alec" (list (make-card 3 heart)
                                                       (make-card jack spade)) 0)
                             (make-player "Calvin" (list (make-card ace spade)
                                                         (make-card ace club)) 0)
                             (make-player "Hobbes" (list (make-card 8 heart)
                                                         (make-card king diamond)) 0)))
              (list (make-card ace spade)
                    (make-card ace heart)))

(define (winners lop)
  (map (λ(x) (trick-winner x))
       (trick-extract lop)))

; who-won? : Player Card -> Player
; determines which player had the winning card for each hand and adds to their score

(check-expect (who-won? (make-player "Calvin" (list (make-card ace spade)
                                                      (make-card ace club)) 0)
                          (make-card ace spade))
                (make-player "Calvin" (list (make-card ace club)) 1))
(check-expect (who-won? (make-player "Anmol" (list (make-card king spade)
                                                     (make-card ace heart)) 0)
                          (make-card ace spade))
                (make-player "Anmol" (list (make-card ace heart)) 0))
(check-expect (who-won? (make-player "Calvin" (list (make-card ace spade)
                                                      (make-card ace club)) 0)
                          (make-card ace club))
                (make-player "Calvin" (list (make-card ace club)) 0))

(define (who-won? pl c)
  (if (and (= (card-value c) (card-value (first (player-loc pl))))
           (= (card-suit c) (card-suit (first (player-loc pl)))))
      (make-player (player-name pl) (rest (player-loc pl)) (add1 (player-score pl)))
      (make-player (player-name pl) (rest (player-loc pl)) (player-score pl))))

; trick-extract : [List-of Player] -> [List-of [List-of Card]]
; creates a list of tricks from a given list of players

(check-expect (trick-extract (list (make-player "Anmol" (list (make-card king spade)
                                                              (make-card ace heart)) 0)
                                   (make-player "Alec" (list (make-card 3 heart)
                                                             (make-card jack spade)) 0)
                                   (make-player "Calvin" (list (make-card ace spade)
                                                               (make-card ace club)) 0)
                                   (make-player "Hobbes" (list (make-card 8 heart)
                                                               (make-card king diamond)) 0)))
              (list (list (make-card king spade)
                          (make-card 3 heart)
                          (make-card ace spade)
                          (make-card 8 heart))
                    (list (make-card ace heart)
                          (make-card jack spade)
                          (make-card ace club)
                          (make-card king diamond))))

(define (trick-extract alop)
  (list (map (λ(x) (first x)) (map (λ(x) (player-loc x)) alop))
        (map (λ(x) (first (rest x))) (map (λ(x) (player-loc x)) alop))))

; ++++++++++++++++++++++++++++++++++++++++++

; Problem 4

; A User is a (make-user String [list-of String])
; interp: A user has a handle and a list of followers' handles
(define-struct user (handle tweeps))

(define Fred (make-user "fjones" (list "vel1" "shady" "daph32" "scoob"
                                       "otherperson")))
(define Velma (make-user "vel1" (list "fjones" "shady" "scoob" "most3")))
(define Shaggy (make-user "shady" (list "scoob" "daph32" "vel1")))
(define Daphne (make-user "daph32" (list "fjones" "vel1" "scoob" "shady")))
(define Scooby (make-user "scoob" (list "shady")))

; A Network is 
; - a [list-of User]
; examples:
(define network1 (list Fred Velma Shaggy Scooby Daphne))
(define network2 (list Fred Daphne))
(define network3 (list Shaggy Scooby Velma))
; list-handles: Network -> [list-of String]
; takes in a network and returns list of handles in network
(check-expect (list-handles empty) empty)
(check-expect (list-handles network1) (list "fjones"
                                            "vel1"
                                            "shady"
                                            "scoob"
                                            "daph32"))
(check-expect (list-handles network2) (list "fjones"
                                            "daph32"))
(check-expect (list-handles network3) (list "shady"
                                            "scoob"
                                            "vel1"))

(define (list-handles net)
  (list-handles-helper net empty))

; list-handles-helper: Network [list-of String] -> [list-of String]
; adds handles from network into a list

(check-expect (list-handles-helper empty empty) empty)
(check-expect (list-handles-helper network1 empty)
              (list "fjones"
                    "vel1"
                    "shady"
                    "scoob"
                    "daph32"))
(check-expect (list-handles-helper network2 empty) (list "fjones"
                                                         "daph32"))
(check-expect (list-handles-helper network3 empty) (list "shady"
                                                         "scoob"
                                                         "vel1"))

(define (list-handles-helper net lohandle)
  (cond
    [(empty? net) empty]
    [(member? (user-handle (first net)) lohandle)
     (list-handles-helper (rest net) lohandle)]
    [else (cons (get-handle (first net))
           (list-handles-helper (rest net)
                                (cons (user-handle (first net)) lohandle)))]))


; get-handle: User -> String
; gets a user's handle

(check-expect (get-handle Fred) "fjones")
(check-expect (get-handle Velma) "vel1")
(check-expect (get-handle Scooby) "scoob")

(define (get-handle u)
  (user-handle u))

; most-followers: Network -> String
; takes in a network and returns handle
; of user with most followers

(check-expect (most-followers empty) "")
(check-expect (most-followers network1) "fjones")
(check-expect (most-followers network2) "fjones")
(check-expect (most-followers network3) "vel1")
(check-expect (most-followers (list
                               (make-user "ay" empty) 
                               Scooby Shaggy))
              "shady")

(define (most-followers net)
  (cond
    [(empty? net) ""]
    [else (most-followers-helper net empty)]))

; most-followers-helper: Network [List-of String]-> String
; arranges list by most followers
; precondition: net is not empty 

(check-expect (most-followers-helper network1 empty) "fjones")
(check-expect (most-followers-helper network2 empty) "fjones")
(check-expect (most-followers-helper network3 empty) "vel1")
(check-expect (most-followers-helper (list
                                      (make-user "ay" empty) 
                                      Scooby Shaggy) empty)
             "shady")

(define (most-followers-helper net lohandle)
  (cond
    [(empty? net) (user-handle (first lohandle))]
    [(empty? lohandle) (most-followers-helper (rest net)
                                              (cons (first net) empty))]
    [else (cond
            [(empty? (user-tweeps (first net)))
             (most-followers-helper (rest net) lohandle)]
            [(> (length (user-tweeps (first net)))
                (length (user-tweeps (first lohandle))))
             (most-followers-helper (rest net) (append (list (first net))
                                                       lohandle))]
            [else (most-followers-helper
                   (rest net)
                   (append lohandle (list (first net))))])]))
              

; friends?: Network -> Boolean
; determines if network contains two users
; who follow one another

(define Fred1 (make-user "fjones1" (list "op3")))
(define otherperson3 (make-user "op3" empty))
(define Velma1 (make-user "vel2" (list "fjones1")))
(define Daphne1 (make-user "daph321" (list "vel2")))
(define Shaggy1 (make-user "shady1" (list "scoob1")))
(define Scooby1 (make-user "scoob1" (list "shady1")))

(define network4 (list Shaggy1 Scooby1))
(define network5 (list Fred1 otherperson3))

(check-expect (friends? empty) false)
(check-expect (friends? network4) true)
(check-expect (friends? network5) false)

(define (friends? net)
  (cond
    [(empty? net) false]
    [else (friends?-helper net (map (λ (x) (user-handle x)) net))]))

; friends?-helper: Network [List-of String] -> Boolean
; determines if the network contains two useres
; who follow one another

(check-expect (friends?-helper network4 (list "shady2" "scoob1")) true)
(check-expect (friends?-helper network5 (list "fjones1" "op3")) false)

(define (friends?-helper net lohandle)
  (cond
    [(empty? lohandle) false]
    [else (or (friends?-helper2 (user-handle (first net))
                                (user-tweeps (first net))
                                net)
              (friends?-helper net (rest lohandle)))]))

; friends?-helper2: String [List-of String] Network -> Boolean
; compares handle to a list of followers for one user's handle

(check-expect (friends?-helper2 "fjones1" (list "fjones1"
                                               "op3") network5) false)
(check-expect (friends?-helper2 "shady1" (list "shady1"
                                                "scoob1") network4) true)

(define (friends?-helper2 handle lotweeps net)
  (cond
    [(empty? lotweeps) false]
    [else (or (friends?-helper3 handle (user-tweeps (find-user net (first lotweeps))))
              (friends?-helper2 handle (rest lotweeps) net))]))

; friends?-helper3: String [List-of String] -> Boolean
; checks if given handle is in list of tweeps

(check-expect (friends?-helper3 "fjones1" (list "lol"
                                                "lol2")) false)
(check-expect (friends?-helper3 "fjones1" (list "fjones1"
                                                "lol2")) true)


(define (friends?-helper3 handle lotweeps)
  (ormap (λ (x) (string=? handle x)) lotweeps))

; find-user: Network String -> User
; returns user from network with the given handle
; precondition: always returns a user aka user is in the network

(check-expect (find-user network1 "fjones") Fred)
(check-expect (find-user network2 "daph32") Daphne)
(check-expect (find-user network4 "shady1") Shaggy1)

(define (find-user net tweep)
  (if (string=? tweep (user-handle (first net)))
      (first net)
      (find-user (rest net) tweep)))
           
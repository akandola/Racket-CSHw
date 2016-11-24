;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Anmolpreet_Kandola_Ezra_Levy_PSET9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; PSET 9 - Abstraction

; Anmolpreet Kandola and Ezra Levy
; Anmolpreet: 001621744
; Ezra: 001624514

; ++++++++++++++++++++++++++++++++++++++++++

; Problem 1 (Excercise 222)

; Lon -> Lon
; add 1 to each number on l

(check-expect (add1* empty) empty)
(check-expect (add1* (list 0)) (list 1))
(check-expect (add1* (list 1 2 3)) (list 2 3 4))
(define (add1* l)
  (nums + 1 l))
     
; Lon -> Lon
; adds 5 to each number on l

(check-expect (plus5 empty) empty)
(check-expect (plus5 (list 0)) (list 5))
(check-expect (plus5 (list 1 2 3)) (list 6 7 8))
(define (plus5 l)
  (nums + 5 l))

; Operation Number [List-of Numbers] -> [List-of Numbers]
; carries out Operation using Number on list of numbers
(check-expect (nums + 1 empty) empty)
(check-expect (nums + 1 (list 1 2 3)) (list 2 3 4))
(check-expect (nums + 5 (list 1 4 5)) (list 6 9 10))
(check-expect (nums * 2 (list 3 5 7)) (list 6 10 14))
(check-expect (nums - 3 (list 7 15 22)) (list 4 12 19))
(check-expect (nums / 5 (list 15 20 25)) (list 3 4 5))

(define (nums op num lon)
  (cond
    [(empty? lon) empty]
    [else
     (cons (op (first lon) num)
           (nums op num (rest lon)))]))

; Lon -> Lon
; subtract 2 from each number on l

(check-expect (min2 empty) empty)
(check-expect (min2 (list 0)) (list -2))
(check-expect (min2 (list 1 2 3)) (list -1 0 1))

(define (min2 l)
  (nums - 2 l))

; Problem 2 (Exercise 229)

; A [Maybe X] is one of: 
; – #false 
; – X

; A [Maybe String] is one of:
; - #false
; - String

; A [Maybe [List-of String]] is one of:
; - #false
; - [List-of String]

; A [List-of [Maybe String]] is one of:
; - empty
; - [List-of [Maybe String]]

; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of the list los if it contains s 
; #false otherwise

; Takes in a String and [List-of String] and returns a Maybe
; which is either #false or a [List-of String]
(check-expect (occurs "a" empty) #false)
(check-expect (occurs "a" (list "b" "a" "d")) (list "d"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)
(define (occurs s los)
  (cond
    [(empty? los) #false]
    [else
     (if (string=? s (first los))
         (rest los)
         (occurs s (rest los)))]))

; Problem 3 (Exercise 255)

; [List-of Number] -> [List-of Number]
; Converts a list of USD amounts into Euro amounts

(check-expect (convert-euro empty) empty)
(check-expect (convert-euro (list 1 100)) (list 1.22 122))

(define (convert-euro lon)
  (map rate lon))

; Number -> Number
; Converts one USD amount into Euro amount
; at a rate of 1.22 Euros per USD

(check-expect (rate 1) 1.22)
(check-expect (rate 100) 122)

(define (rate usd)
  (* usd 1.22))


; [List-of Number] -> [List-of Number]
; Converts a list of Farenheit temperatures into Celsius

(check-expect (convertFC empty) empty)
(check-expect (convertFC (list 32 -40)) (list 0 -40))

(define (convertFC lon)
  (map tempconvert lon))

; Number -> Number
; Converts one Farenheit temperature to Celsius

(check-expect (tempconvert 32) 0)
(check-expect (tempconvert -40) -40)

(define (tempconvert f)
  (* (- f 32) (/ 5 9)))

; [List-of Posn] -> [List-of [List-of Number Number]]
; takes a list of posns and separates them into a list of
; a pair of numbers

(check-expect (translate empty) empty)
(check-expect (translate (list (make-posn 1 1)
                               (make-posn 4 5)
                               (make-posn 3 3)))
              (list (list 1 1)
                    (list 4 5)
                    (list 3 3)))

(define (translate lop)
  (cond
    [(empty? lop) empty]
    [else
     (cons (breakposn (first lop))
           (translate (rest lop)))]))

; Posn -> [List-of Number Number]
; takes a posn and breaks it into a list of a pair
; of numbers

(check-expect (breakposn (make-posn 2 2))
              (list 2 2))

(define (breakposn pos)
  (list (posn-x pos)
        (posn-y pos)))

; Problem 4 (Exercise 257)

; An Inventory is a (make-inventory String String Number Number)
; An inventory record specifies the name of an item,
; a description, the acquisition price, and the recommended sales price.
(define-struct inventory (name description aqprice saleprice))

; ua is a number

; Number [List-of Inventory] -> [List-of Inventory]
; ua is a number and the function returns all inventory
; whose sales price is lower than the number (ua)

(check-expect (eliminate-exp 10 empty) empty)
(check-expect
 (eliminate-exp 15
                (list (make-inventory "Towel"
                                       "Wipes stuff."
                                       10 16)
                      (make-inventory "Sofa"
                                       "Somehow cheaper than a Towel."
                                       8 12)))
 (list (make-inventory "Sofa"
                        "Somehow cheaper than a Towel."
                        8 12)))


(define (eliminate-exp ua loi)
  (cond
    [(empty? loi) empty]
    [else
     (if (< (inventory-saleprice (first loi)) ua)
         (cons (first loi)
               (eliminate-exp ua (rest loi)))
         (eliminate-exp ua (rest loi)))]))


; String [List-of Inventory] -> [List of Inventory]
; ty is a String representing the name of the Inventory item
; returns all inventory without the name given
; in the input

(check-expect (recall "Towel" empty) empty)
(check-expect (recall "Towel" (list (make-inventory "Towel"
                                       "Wipes stuff."
                                       10 16)
                      (make-inventory "Sofa"
                                       "Somehow cheaper than a Towel."
                                       8 12)))
              (list (make-inventory "Sofa"
                                       "Somehow cheaper than a Towel."
                                       8 12)))
(define (recall ty loi)
  (local ((define (name-notty? inv)
            (not (string=? ty (inventory-name inv)))))
      (filter name-notty? loi)))

; [List-of String] [List-of String] -> [List-of String]
; returns a list of names in both lists

(check-expect (selection empty empty) empty)
(check-expect (selection (list "Abe") empty) empty)
(check-expect (selection empty (list "Zoey")) empty)
(check-expect (selection (list "Abe" "Zoey" "John" "Snow")
                        (list "Meg" "Zoolander" "Abe" "Zoey" "Harris"))
              (list "Abe" "Zoey"))

(define (selection lon1 lon2)
  (cond
    [(empty? lon2) empty]
    [else
     (if (check-name (first lon2) lon1)
         (cons (first lon2)
               (selection lon1 (rest lon2)))
         (selection lon1 (rest lon2)))]))

; [List-of String] String -> Boolean
; checks if the string is in the list 

(check-expect (check-name "Abe" empty) #false)
(check-expect (check-name "Abe" (list "Zoe" "Anmol" "Abe" "Dustin"))
              #true)

(define (check-name str lon3)
  (cond
    [(empty? lon3) #false]
    [else
     (or (string=? str (first lon3))
         (check-name str (rest lon3)))]))

; Problem 5

; [List-of [Number -> Number]]
; interp. a list of functions that take in a number and produce a number

(define (f1 x) (/ x 2))
(define (f2 x) (* 3 x))
(define (f3 x) (+ (- x 1) 10))
(define list1 (list f1 f2 f3))

; at-0 : [List-of [Number -> Number]] -> [List-of Number]
; produces a list of the results of evaluating a list of functions at 0
(check-expect (at-0 empty) empty)
(check-expect (at-0 list1)
              (list 0 0 9))

(define (at-0 alof)
  (cond [(empty? alof) '()]
        [else (cons ((first alof) 0)
                            (at-0 (rest alof)))]))

; Problem 6

(define list2 (list "a" "b" "c" "4"))
(define list3 (list "A" "b" "C" "?"))

; find-string : [List-of String] String -> Boolean
; is the given string a member of the list?
(check-expect (find-string list2 "a") #true)
(check-expect (find-string list2 "d") #false)
(check-expect (find-string list3 "a") #false)
(check-expect (find-string list3 "C") #true)

(define (find-string alos s)
  (cond [(empty? alos) #false]
        [else (or (string=? (first alos) s)
                  (find-string (rest alos) s))]))

; find-string-generic : [List-of String] String [X X -> Y] -> Boolean

(check-expect (find-string-generic list2 "a" string=?) #true)
(check-expect (find-string-generic list2 "d" string=?) #false)
(check-expect (find-string-generic list3 "a" string=?) #false)
(check-expect (find-string-generic list3 "C" string=?) #true)
(check-expect (find-string-generic list2 "A" string-ci=?) #true)
(check-expect (find-string-generic list2 "d" string-ci=?) #false)
(check-expect (find-string-generic list3 "a" string-ci=?) #true)
(check-expect (find-string-generic list3 "D" string-ci=?) #false)
(check-expect (find-string-generic list2 "4" string-ci=?) #true)
(check-expect (find-string-generic list3 "!" string-ci=?) #false)

(define (find-string-generic alos s op)
  (cond [(empty? alos) #false]
        [else (or (op (first alos) s)
                  (find-string-generic (rest alos) s op))]))

; find-string-case-sensitive : [List-of String] String -> Boolean
; does the given string exactly match any string in the list?

(check-expect (find-string-case-sensitive list2 "a") #true)
(check-expect (find-string-case-sensitive list2 "d") #false)
(check-expect (find-string-case-sensitive list3 "a") #false)
(check-expect (find-string-case-sensitive list3 "C") #true)

(define (find-string-case-sensitive alos s)
  (find-string-generic alos s string=?))

; find-string-case-insensitive : [List-of String] String -> Boolean
; does the given string exactly match any string in the list?
; But alphabetic characters do not have to be case-sensitive.

(check-expect (find-string-case-insensitive list2 "A") #true)
(check-expect (find-string-case-insensitive list2 "d") #false)
(check-expect (find-string-case-insensitive list3 "a") #true)
(check-expect (find-string-case-insensitive list3 "D") #false)
(check-expect (find-string-case-insensitive list2 "4") #true)
(check-expect (find-string-case-insensitive list3 "!") #false)

(define (find-string-case-insensitive alos s)
  (find-string-generic alos s string-ci=?))
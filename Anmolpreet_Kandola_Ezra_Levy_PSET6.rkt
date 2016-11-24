;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Anmolpreet_Kandola_Ezra_Levy_PSET6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; PSET 6

;Anmolpreet Kandola and Ezra Levy
;Anmolpreet: 001621744
;Ezra: 001624514

;_____________________________

; Problem 1

; An Item (physical mail) is one of:
; Letter
; Box


; A Letter is a (make-letter String Number)
; interp: String is address
;         Number is weight (in ounces)
(define-struct letter (address weight))
; - make-letter
; - letter-address
; - letter-weight
; - letter?

; A Box is a (make-box Number1 Number2 Number3 Number4)
; interp: Number1 is height (inches)
;         Number2 is width (inches)
;         Number3 is length (inches)
;         Number4 is weight (ounces)
(define-struct box (height width length weight))
; - make-box
; - box-height
; - box-width
; - box-length
; - box-weight
; - box?

; item-ok?
; Item -> Boolean
; Takes in a piece of mail, determines whether
; or not it fits the rules for either a letter
; or a box.

(check-expect (item-ok? (make-letter "Boston, MA" 3.2)) #true)
(check-expect (item-ok? (make-letter "New York, NY" 4.0)) #false)
(check-expect (item-ok? (make-box 15 12 14 350)) #true)
(check-expect (item-ok? (make-box 30 25 25 1000)) #false)

#;
(define (item-tmpl i)
  (cond
    [(letter? i) ... (letter-address i) ...
                 ... (letter-weight i) ...]
    [(box? i) ... (box-height i) ...
              ... (box-width i) ...
              ... (box-length i) ...
              ... (box-weight i) ...]))

(define (item-ok? i)
  (cond
    [(letter? i) (< (letter-weight i) 3.5)]
    [(box? i) (and (and (< (+ (box-height i)
                         (box-width i)
                         (box-length i))
                      61)
                        (< (* (box-height i)
                         (box-width i)
                         (box-length i))
                           7938))
                   (<= (box-weight i) 800))]))
                 

(check-expect (item-ok? (make-letter "Palo Alto, CA" 3)) #true)
(check-expect (item-ok? (make-letter "Seattle, WA" 4.5)) #false)
(check-expect (item-ok? (make-box 10 10 10 300)) #true)
(check-expect (item-ok? (make-box 20 22 24 650)) #false)

; A List of Items (LOI) is one of:
; - empty
; - (cons Item LOI)

; bad-items
; LOI -> LOI
; Takes in a list of items, returns all items
; which don't fit the rules for mail.

(check-expect (bad-items (cons (make-letter "Boston, MA" 3.2)
                               (cons (make-box 30 25 25 1000) empty)))
              (cons (make-box 30 25 25 1000) empty))

(check-expect (bad-items (cons (make-letter "Boston, MA" 3.2)
                               (cons (make-box 15 12 14 350) empty)))
              empty)

(check-expect (bad-items (cons (make-letter "New York, NY" 4.0)
                               (cons (make-box 15 12 14 350) empty)))
              (cons (make-letter "New York, NY" 4.0) empty))

(check-expect (bad-items (cons (make-letter "Seattle, WA" 4.5)
                               (cons (make-box 20 22 24 1000) empty)))
              (cons (make-letter "Seattle, WA" 4.5)
                    (cons (make-box 20 22 24 1000) empty)))

#;
(define (LOI-tmpl loi)
  (cond
    [(empty? loi) ...]
    [(cons? loi) ... (first loi) ...
                 ... (LOI-tmpl (rest loi))]))

(define (bad-items loi)
  (cond
    [(empty? loi) empty]
    [(cons? loi) (if (item-ok? (first loi))
                     (bad-items (rest loi))
                      (cons (first loi)
                            (bad-items (rest loi))))]))

(check-expect (bad-items (cons (make-letter "Boston, MA" 3.2)
                               (cons (make-box 30 25 25 1000) empty)))
              (cons (make-box 30 25 25 1000) empty))

(check-expect (bad-items (cons (make-letter "Boston, MA" 3.2)
                               (cons (make-box 15 12 14 350) empty)))
              empty)

; total-postage
; LOI -> Number
; Takes in a list of items and returns
; total postage price to mail them all out,
; including prices only for items which fit
; the rules for mail, and bad mail will not be counted.

(check-expect (total-postage empty) 0)
(check-expect (total-postage (cons (make-letter "Boston, MA" 3.2)
                                   (cons (make-box 12 14 17 500) empty)))
              75.5)
(check-expect (total-postage
               (cons (make-letter "New York, NY" 4.0)
                     (cons (make-box 10 10 10 300)
                           (cons (make-letter "Austin, TX" 2.5) empty))))
              
              45.5)     

#;
(define (LOI-tmpl loi)
  (cond
    [(empty? loi) ...]
    [(cons? loi) ... (first loi) ...
                 ... (LOI-tmpl (rest loi))]))

(define (total-postage loi)
  (cond
    [(empty? loi) 0]
    [(cons? loi) (if (item-ok? (first loi))
                     (+ (price-calc (first loi))
                        (total-postage (rest loi)))
                     (total-postage (rest loi)))]))

(check-expect (total-postage empty)
              0)

(check-expect (total-postage (cons (make-letter "Boston, MA" 3.2)
                                   (cons (make-box 30 25 25 1000) empty)))
              .5)

(check-expect (total-postage (cons (make-letter "Boston, MA" 3.2)
                                   (cons (make-box 15 15 15 750)
                                         (cons (make-letter "New York, NY" 4.0)
                                               empty))))
              113)


     

; price-calc
; Item -> Number
; Takes in a piece of mail which fits the rules
; and returns the cost of postage for the mail.

(check-expect (price-calc (make-letter "Newark, NJ" 3.0)) .5)
(check-expect (price-calc (make-box 12 13 14 600)) 90)
(check-expect (price-calc (make-box 15 15 15 760)) 114)

#;
(define (item-tmpl i)
  (cond
    [(letter? i) ... (letter-address i) ...
                 ... (letter-weight i) ...]
    [(box? i) ... (box-height i) ...
              ... (box-width i) ...
              ... (box-length i) ...
              ... (box-weight i) ...]))


(define (price-calc i)
  (cond
    [(letter? i) .5]
    [(box? i) (* (box-weight i) .15)]))

(check-expect (price-calc (make-letter "Boston, MA" 3.2)) .5)
(check-expect (price-calc (make-box 15 15 15 750)) 112.5)

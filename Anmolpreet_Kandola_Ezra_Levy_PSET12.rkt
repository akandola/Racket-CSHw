;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Anmolpreet_Kandola_Ezra_Levy_PSET12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; PSET 12 - Trees and S-Expressions

(require 2htdp/image)

; Anmolpreet Kandola and Ezra Levy
; Anmolpreet: 001621744
; Ezra: 001624514

; ++++++++++++++++++++++++++++++++++++++++++

; Problem 1

;;; An Atom is one of:
;;; - Number
;;; - Symbol
;;; - String
;;;
;;; An SExp is one of:
;;; - Atom
;;; - [List-of SExp]

#;
(define (atom-tmpl a)
  (cond [(number? a) ...]
        [(symbol? a) ...]
        [(string? a) ...]))

#;
(define (sexp-tmpl se)
  (cond [(atom? se) (atom-tmpl se)]
        [(list? se) (se-list-tmpl se)]))

;; atom?: SExp -> Boolean
;; Checks if SExp is an atom.

(check-expect (atom? 'hi) true)
(check-expect (atom? "well then") true)
(check-expect (atom? 12) true)
(check-expect (atom? (list "x" "y")) false)
(check-expect (atom? '()) false)

(define (atom? x)
  (or (string? x) (symbol? x) (number? x)))


;; text-atom: Atom -> String
;; Turns an atom into a string based on the input type
;; (either a string, symbol, or nubmer).

(check-expect (text-atom "when it hits you") "\"when it hits you\"")
(check-expect (text-atom 'textbook) "textbook")
(check-expect (text-atom 1323) "1323")

(define (text-atom x)
  (cond
    [(string? x) (string-append "\"" x "\"")]
    [(symbol? x) (string-append (symbol->string x))]
    [(number? x) (string-append (number->string x))]))

;; text-sexp: SExp -> String
;; Makes a string representation of a SExp

(check-expect (text-sexp 100) "100")
(check-expect (text-sexp 'yes) "yes")
(check-expect (text-sexp "this is a string") "\"this is a string\"")
(check-expect (text-sexp empty) "")
(check-expect (text-sexp (list 'a (list 123 "yes" 'no) 4))
              "(a (123 \"yes\" no ) 4 )")
(check-expect (text-sexp (list 'fine 56 "x" (list 42 '42 "42")))
              "(fine 56 \"x\" (42 42 \"42\" ) )")

(define (text-sexp s)
  (cond
    [(atom? s) (text-atom s)]
    [(empty? s) ""]
    [else (string-append "(" (foldr (λ (x y) (string-append (text-sexp x) " " y)) ")" s))]))



; ++++++++++++++++++++++++++++++++++++++++++

; Problem 2

(define-struct lego (label color width))
; A Lego is a structure:
;    (make-lego Number Symbol Number)
; interpretation: (make-lego l c w) is the lego brick
; with label l, color c, and width w (in pixels).

(define-struct bigger (lego left right))
; A LegoBldg (lego building) is one of:
; - Lego
; - (make-bigger Lego LegoBldg LegoBldg)
; interpretation: (make-bigger l lft rgt) makes a bigger
; lego building by putting a lego brick l on top of two lego
; buildings lft (left) and rgt (right).

;; count-bricks: LegoBldg -> Number
;; Returns number of lego bricks in the lego building.

(check-expect (count-bricks (make-lego 14 'green 1)) 1)
(check-expect (count-bricks (make-bigger (make-lego 2 'blue 4)
                                         (make-lego 3 'red 1)
                                         (make-lego 5 'yellow 3))) 3)
(check-expect (count-bricks (make-bigger (make-lego 1 'black 2)
                                         (make-bigger
                                          (make-lego 2 'orange 3)
                                          (make-lego 3 'orange 3)
                                          (make-lego 5 'salmon 4))
                                         (make-bigger
                                          (make-lego 1 'purple 3)
                                          (make-lego 7 'brown 3)
                                          (make-lego 22 'salmon 4)))) 7)
(check-expect (count-bricks (make-bigger (make-lego 1 'black 2)
                                         (make-lego 7 'brown 3)
                                         (make-bigger
                                          (make-lego 2 'orange 3)
                                          (make-bigger
                                           (make-lego 2 'orange 3)
                                           (make-lego 3 'orange 3)
                                           (make-lego 5 'salmon 4))
                                          (make-lego 5 'salmon 4)))) 7)


(define (count-bricks lgb)
  (cond
    [(lego? lgb) 1]
    [(bigger? lgb)
     (+ (count-bricks (bigger-lego lgb))
        (count-bricks (bigger-left lgb))
        (count-bricks (bigger-right lgb)))]))

; ++++++++++++++++++++++++++++++++++++++++++

; Problem 3

;; how-high: LegoBldg -> Number
;; Returns height of lego building in pixels.

(check-expect (how-high (make-lego 14 'green 1)) 10)
(check-expect (how-high (make-bigger (make-lego 2 'blue 4)
                                     (make-lego 3 'red 1)
                                     (make-lego 5 'yellow 3))) 20)
(check-expect (how-high (make-bigger (make-lego 1 'black 2)
                                     (make-bigger
                                      (make-lego 2 'orange 3)
                                      (make-lego 3 'orange 3)
                                      (make-lego 5 'salmon 4))
                                     (make-bigger
                                      (make-lego 1 'purple 3)
                                      (make-lego 7 'brown 3)
                                      (make-lego 22 'salmon 4)))) 30)
(check-expect (how-high (make-bigger (make-lego 1 'black 2)
                                     (make-bigger
                                      (make-lego 2 'orange 3)
                                      (make-lego 3 'orange 3)
                                      (make-lego 5 'salmon 4))
                                     (make-lego 7 'brown 3))) 30)
(check-expect (how-high (make-bigger (make-lego 1 'black 2)
                                     (make-lego 7 'brown 3)
                                     (make-bigger
                                      (make-lego 2 'orange 3)
                                      (make-bigger
                                       (make-lego 2 'orange 3)
                                       (make-lego 3 'orange 3)
                                       (make-lego 5 'salmon 4))
                                      (make-lego 5 'salmon 4)))) 40)

(define (how-high lgb)
  (cond
    [(lego? lgb) 10]
    [(bigger? lgb)
     (+ 10 (if (< (how-high (bigger-left lgb)) (how-high (bigger-right lgb)))
               (how-high (bigger-right lgb))
               (how-high (bigger-left lgb))))]))


; ++++++++++++++++++++++++++++++++++++++++++

; Problem 4

;; contains-colored-brick?: LegoBldg Symbol -> Boolean
;; Determines if the lego building has a lego
;; brick of the given color.

(check-expect (contains-colored-brick? (make-lego 14 'green 1) 'green) true)
(check-expect (contains-colored-brick? (make-lego 14 'green 1) 'red) false)
(check-expect (contains-colored-brick? (make-bigger (make-lego 2 'blue 4)
                                                    (make-lego 3 'red 1)
                                                    (make-lego 5 'yellow 3))
                                       'red) true)
(check-expect (contains-colored-brick? (make-bigger (make-lego 2 'blue 4)
                                                    (make-lego 3 'red 1)
                                                    (make-lego 5 'yellow 3))
                                       'orange) false)
(check-expect (contains-colored-brick? (make-bigger (make-lego 1 'black 2)
                                                    (make-bigger
                                                     (make-lego 2 'orange 3)
                                                     (make-lego 3 'orange 3)
                                                     (make-lego 5 'salmon 4))
                                                    (make-bigger
                                                     (make-lego 1 'purple 3)
                                                     (make-lego 7 'brown 3)
                                                     (make-lego 22 'salmon 4))) 'salmon) true)
(check-expect (contains-colored-brick? (make-bigger (make-lego 1 'black 2)
                                                    (make-bigger
                                                     (make-lego 2 'orange 3)
                                                     (make-lego 3 'orange 3)
                                                     (make-lego 5 'salmon 4))
                                                    (make-bigger
                                                     (make-lego 1 'purple 3)
                                                     (make-lego 7 'brown 3)
                                                     (make-lego 22 'salmon 4))) 'blue) false)

(define (contains-colored-brick? lgb c)
  (cond
    [(lego? lgb) (symbol=? (lego-color lgb) c)]
    [(bigger? lgb) (or (contains-colored-brick? (bigger-lego lgb) c)
                       (contains-colored-brick? (bigger-left lgb) c)
                       (contains-colored-brick? (bigger-right lgb) c))]))

; ++++++++++++++++++++++++++++++++++++++++++

; Problem 5

; A MaybeLego is one of:
; - false
; - Lego

;; find-colored-brick?: LegoBldg Symbol -> MaybeLego
;; returns the first lego of a given color if there
;; is one in the given LegoBldg, otherwise returns false.

(check-expect (find-colored-brick? (make-lego 14 'green 1) 'green) (make-lego 14 'green 1))
(check-expect (find-colored-brick? (make-lego 14 'green 1) 'red) false)
(check-expect (find-colored-brick? (make-bigger (make-lego 2 'blue 4)
                                                    (make-lego 3 'red 1)
                                                    (make-lego 5 'yellow 3))
                                       'red) (make-lego 3 'red 1))
(check-expect (find-colored-brick? (make-bigger (make-lego 2 'blue 4)
                                                    (make-lego 3 'red 1)
                                                    (make-lego 5 'yellow 3))
                                       'orange) false)
(check-expect (find-colored-brick? (make-bigger (make-lego 1 'black 2)
                                                    (make-bigger
                                                     (make-lego 2 'orange 3)
                                                     (make-lego 3 'orange 3)
                                                     (make-lego 5 'salmon 4))
                                                    (make-bigger
                                                     (make-lego 1 'purple 3)
                                                     (make-lego 7 'brown 3)
                                                     (make-lego 22 'salmon 4))) 'salmon)
              (make-lego 5 'salmon 4))
(check-expect (find-colored-brick? (make-bigger (make-lego 1 'black 2)
                                                    (make-bigger
                                                     (make-lego 2 'orange 3)
                                                     (make-lego 3 'orange 3)
                                                     (make-lego 5 'salmon 4))
                                                    (make-bigger
                                                     (make-lego 1 'purple 3)
                                                     (make-lego 7 'brown 3)
                                                     (make-lego 22 'salmon 4))) 'blue) false)


(define (find-colored-brick? lbg c)
  (cond [(lego? lbg) (if (symbol=? (lego-color lbg) c)
                         lbg
                         false)]
        [else (if (lego? (find-colored-brick? (bigger-left lbg) c))
                  (find-colored-brick? (bigger-left lbg) c)
                  (find-colored-brick? (bigger-right lbg) c))]))

; ++++++++++++++++++++++++++++++++++++++++++

; Problem 6

; lb->image : LegoBldg -> Image
; produces an image of the given legobldg

(check-expect (lb->image (make-lego 3 'red 1)) (rectangle 1 10 'solid 'red))
(check-expect (lb->image (make-bigger (make-lego 2 'blue 4)
                                      (make-lego 3 'red 1)
                                      (make-lego 5 'yellow 3)))
              (above (rectangle 4 10 'solid 'blue)
                     (beside/align "middle"
                                   (rectangle 1 10 'solid 'red)
                                   (rectangle 3 10 'solid 'yellow))))
(check-expect (lb->image (make-bigger (make-lego 4 'purple 80)
                                      (make-bigger (make-lego 2 'blue 60)
                                                   (make-lego 1 'yellow 40)
                                                   (make-lego 3 'red 40))
                                      (make-bigger (make-lego 6 'orange 60)
                                                   (make-lego 5 'green 40)
                                                   (make-lego 7 'red 40))))
              (above (rectangle 80 10 'solid 'purple)
                     (beside/align "top"
                                   (above (rectangle 60 10 'solid 'blue)
                                          (beside/align "top"
                                                        (rectangle 40 10 'solid 'yellow)
                                                        (rectangle 40 10 'solid 'red)))
                                   (above (rectangle 60 10 'solid 'orange)
                                          (beside/align "top"
                                                        (rectangle 40 10 'solid 'green)
                                                        (rectangle 40 10 'solid 'red))))))
(check-expect (lb->image (make-bigger (make-lego 4 'purple 80)
                                      (make-bigger (make-lego 2 'blue 60)
                                                   (make-lego 1 'yellow 40)
                                                   (make-lego 3 'red 40))
                                      (make-lego 6 'orange 60)))
              (above (rectangle 80 10 'solid 'purple)
                     (beside/align "top"
                                   (above (rectangle 60 10 'solid 'blue)
                                          (beside/align "top"
                                                        (rectangle 40 10 'solid 'yellow)
                                                        (rectangle 40 10 'solid 'red)))
                                   (rectangle 60 10 'solid 'orange))))

(define (lb->image lbg)
  (cond [(lego? lbg) (lego->image lbg)]
        [(bigger? lbg) (above (lego->image (bigger-lego lbg))
                              (beside/align "top"
                                            (lb->image (bigger-left lbg))
                                            (lb->image (bigger-right lbg))))]))

; lego->image : Lego -> Image
; produces an image of the given lego

(check-expect (lego->image (make-lego 3 'orange 3)) (rectangle 3 10 'solid 'orange))
(check-expect (lego->image (make-lego 1 'purple 70)) (rectangle 70 10 'solid 'purple))

(define (lego->image l)
  (rectangle (lego-width l) 10 'solid (lego-color l)))






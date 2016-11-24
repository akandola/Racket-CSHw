;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Anmolpreet_Kandola_Ezra_Levy_PSET11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; PSET 11 - Binary Trees

; Anmolpreet Kandola and Ezra Levy
; Anmolpreet: 001621744
; Ezra: 001624514

; ++++++++++++++++++++++++++++++++++++++++++

; Problem 1

;; A BTN is one of
;; - Number
;; - (make-node BTN BTN)
(define-struct node (left right))


;; btn-height: BTN -> Number
;; Determines maximum distance from the root to the leaf furthest
;; from the root. Distance in measured as adding 1 per each
;; internal node from the root to the leaf.


(check-expect (btn-height 42) 0)
(check-expect (btn-height (make-node 2 (make-node 4 9))) 2)
(check-expect (btn-height (make-node (make-node (make-node 4 4) 3) (make-node 2 2))) 3)
(check-expect (btn-height (make-node (make-node (make-node 3 2) 2)
                                     (make-node (make-node (make-node 1 3) 2) 2))) 4)
(check-expect (btn-height (make-node (make-node (make-node 4 4) 2)
                                     (make-node (make-node 1 1) 1))) 3)

(define (btn-height abt)
  (cond [(number? abt) 0]
        [else (if (< (btn-height (node-left abt)) (btn-height (node-right abt)))
                  (add1 (btn-height (node-right abt)))
                  (add1 (btn-height (node-left abt))))]))

; ++++++++++++++++++++++++++++++++++++++++++

; Problem 2

;; btn-sum : BTN -> Number
;; Determines sum of all leaves in the tree.

(check-expect (btn-sum 42) 42)
(check-expect (btn-sum (make-node 2 (make-node 4 9))) 15)
(check-expect (btn-sum (make-node (make-node (make-node 4 4) 3) (make-node 2 2))) 15)
(check-expect (btn-sum (make-node (make-node (make-node 3 2) 2)
                                  (make-node (make-node (make-node 1 3) 2) 5))) 18)
(check-expect (btn-sum (make-node (make-node (make-node 4 4) 2)
                                  (make-node (make-node 1 1) 1))) 13)

(define (btn-sum bt)
  (cond
    [(number? bt) bt]
    [(node? bt)
     (+ (btn-sum (node-left bt))
        (btn-sum (node-right bt)))]))

; ++++++++++++++++++++++++++++++++++++++++++

; Problem 3

; A leafy binary tree (LBT) is a binary tree with the symbol 'leaf at its leafs
; A LBT is one of:
; - 'leaf
; - (make-node LBT LBT)

; all-correct-height? : Number [List-of LBT] -> Boolean
; are all the trees in the given LBT of the correct height?
(check-expect (all-correct-height? 0 '()) true)
(check-expect (all-correct-height? 0 '(leaf)) true)
(check-expect (all-correct-height? 2 (leafy-btns 2)) true)

(define (all-correct-height? n alot)
  (andmap (λ(x) (= n (lbt-height x))) alot))

; lbt-height : LBT -> Number
; computes height of given lbt
(check-expect (lbt-height 'leaf) 0)
(check-expect (lbt-height (make-node 'leaf 'leaf)) 1)
(check-expect (lbt-height (make-node (make-node 'leaf 'leaf) 'leaf)) 2)
(check-expect (lbt-height (make-node 'leaf (make-node 'leaf 'leaf))) 2)

(define (lbt-height abt)
  (cond [(symbol? abt) 0]
        [else (if (< (lbt-height (node-left abt)) (lbt-height (node-right abt)))
                  (add1 (lbt-height (node-right abt)))
                  (add1 (lbt-height (node-left abt))))]))

; leafy-btns: Number -> [List-of LBT]
; Creates a list of all leafy binary trees with height n.
(check-expect (leafy-btns 0) (list 'leaf))
(check-expect (leafy-btns 1) (list (make-node 'leaf 'leaf)))
(check-expect (leafy-btns 2) (list (make-node (make-node 'leaf 'leaf) 'leaf)
                                   (make-node (make-node 'leaf 'leaf)
                                              (make-node 'leaf 'leaf))
                                   (make-node 'leaf (make-node 'leaf 'leaf))))

(define (leafy-btns num)
  (cond [(zero? num) '(leaf)]
        [(= 1 num) (list (make-node 'leaf 'leaf))]
        [else (append (cartesian-product (leafy-btns (sub1 num)) (leafy-bt num))
                      (cartesian-product (leafy-bt (sub1 num)) (leafy-btns (sub1 num))))]))


; leafy-bt : Number -> [List-of LBT]
; creates a list of all leafy binary trees with height less than n
(check-expect (leafy-bt 0) '())
(check-expect (leafy-bt 1) '(leaf))
(check-expect (leafy-bt 2) (list 'leaf (make-node 'leaf 'leaf)))

(define (leafy-bt n)
  (cond [(zero? n) '()]
        [(= 1 n) '(leaf)]
        [else (append '(leaf) (cartesian-product (leafy-bt (sub1 n)) (leafy-bt (sub1 n))))]))


; cartesian-product : [List-of LBT] [List-of LBT] -> [List-of LBT]
; produces list of all combinations of elements of two given LBTs
(check-expect (cartesian-product '() '()) '())
(check-expect (cartesian-product '(leaf) '(leaf)) (list (make-node 'leaf 'leaf)))
(check-expect (cartesian-product '(leaf) (list (make-node 'leaf 'leaf)))
              (list (make-node 'leaf (make-node 'leaf 'leaf))))
(check-expect (cartesian-product (list (make-node 'leaf 'leaf)) '(leaf))
              (list (make-node (make-node 'leaf 'leaf) 'leaf)))
(check-expect (cartesian-product (list (make-node 'leaf 'leaf))
                                 (list (make-node 'leaf 'leaf)))
              (list (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf))))

(define (cartesian-product alot1 alot2)
  (foldr (λ(x y) (append (cart-prod x alot2) y)) '() alot1))

; cart-prod : LBT [List-of LBT] -> [List-of LBT]
; produces list of all combinations of given LBT with given [List-of LBT]
(check-expect (cart-prod 'leaf (list (make-node 'leaf 'leaf)))
              (list (make-node 'leaf
                               (make-node 'leaf 'leaf))))
(check-expect (cart-prod 'leaf '(leaf)) (list (make-node 'leaf 'leaf)))
(check-expect (cart-prod 'leaf '()) '())

(define (cart-prod t alot2)
  (map (λ(x) (make-node t x)) alot2))



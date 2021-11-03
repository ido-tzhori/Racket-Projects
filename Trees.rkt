;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw 11 day 5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; HW 11
; Ido Tzhori / Gubi Ganguly

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 1
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; list-prefix? : [List-of Numbers] [List-of Numbers] -> Boolean
; Determines if the first list is a prefix of the second list

(check-expect (list-prefix? empty empty) #true)
(check-expect (list-prefix? (list 1 2 3 4) (list 4 3 2 1)) #false)
(check-expect (list-prefix? (list 1 2 3) (list 1 2 3 4 5)) #true)
(check-expect (list-prefix? empty (list 1 2 3)) #true)
(check-expect (list-prefix? (list 1 2 3) empty) #false)

(define (list-prefix? l1 l2)
  (cond
    [(and (cons? l1) (cons? l2)) (and (= (first l1) (first l2))
                                      (list-prefix? (rest l1) (rest l2)))]
    [(empty? l1) #true]
    [else #false]))

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 2
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; max-splice: [List-of Numbers] [List-of Numbers] -> [List-of Numbers]
; Creates the shortest list of numbers which begins with all elements of the first list
; if there is any overlap then it may cut numbers off the second list

(check-expect (max-splice '(1 2 3 4) '(2 3 4 5)) '(1 2 3 4 5))
(check-expect (max-splice '(1 2 3 4) '(2 2 3 4 5)) '(1 2 3 4 2 2 3 4 5))
(check-expect (max-splice empty empty) empty)
(check-expect (max-splice empty (list 1 2 3)) (list 1 2 3))
(check-expect (max-splice (list 1 2 3) empty) (list 1 2 3))

(define (max-splice l1 l2)
  (cond
    [(empty? l2) l1]
    [(cons? l2) (if (list-prefix? l1 l2)
                    l2
                    (cons (first l1) (max-splice (rest l1) l2)))]))

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 3
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; valid-results? : (X Y) [List-of Num] [List-of [Num -> Num]] [List-of Num] -> Boolean
; Do the outputs when the list of functions (second list) apply on the list of numbers (first list)
; equal the list of outputs (third list)?

(check-expect (valid-results? (list 1 3)
                              (list (lambda (x) (+ x 1))
                                    (lambda (x) (+ x 2)))
                              (list 2 5)) #true)
(check-expect (valid-results? (list 1 6 7)
                              (list (lambda (x) (+ x 1))
                                    (lambda (x) (- x 0))
                                    (lambda (x) (* 1999898 x)))
                              (list 2 6 42)) #false)

(check-expect (valid-results? empty empty empty) #true)

(define (valid-results? l1 l2 l3)
  (cond
    [(and (cons? l1) (cons? l2) (cons? l3)) (if (= ((first l2) (first l1)) (first l3))
                                                (valid-results? (rest l1) (rest l2) (rest l3))
                                                #false)]
    [else #true]))

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 4
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

(define-struct assignment [role person])

; assign : [List-of Symbols] [List-of Strings] -> [List-of Assignment]
; combines elements from the list of symbols and list of string, if list of strings is shorter
; then the unfilled roles are paired with #false

(check-expect (assign (list 'hi 'hello) (list "hello" "bye")) (list (make-assignment 'hi "hello")
                                                                    (make-assignment 'hello "bye")))
(check-expect (assign empty empty) empty)
(check-expect (assign (list 'hi 'hello) (list "hello")) (list (make-assignment 'hi "hello")
                                                              (make-assignment 'hello #false)))
(check-expect (assign (list 'hi 'hell) (list "hello" "bye" "what"))
              (list (make-assignment 'hi "hello")
                    (make-assignment 'hell "bye")))
(check-expect (assign empty (list "bob" "billy")) empty)
(check-expect (assign (list 'hi 'hello) empty) (list (make-assignment 'hi #false)
                                                     (make-assignment 'hello #false)))

(define (assign losym los)
  (cond
    [(empty? losym) empty]
    [(and (cons? losym) (empty? los)) (cons (make-assignment (first losym) #false)
                                            (assign (rest losym) los))]
    [(and (cons? losym) (cons? los)) (cons (make-assignment (first losym) (first los))
                                           (assign (rest losym) (rest los)))]))


;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 5
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

(define-struct bt [value left right])
; A BT (Binary Tree) is one of:
; - 'none
; - (make-bt Symbol BT BT)

(define bt1 'none)
(define bt2 (make-bt 'hi bt1 bt1))
(define bt3 (make-bt 'hey bt2 bt1))
(define bt4 (make-bt 'yo bt3 bt2))
(define bt5 (make-bt 'yo bt2 bt3))

; tree-equiv : BT BT -> Boolean
; Determines if two trees are equivalent. Two trees are considered equivalent if
; all the elements are the same in each of the subtree regardless of location

(check-expect (tree-equiv bt2 bt3) #false)
(check-expect (tree-equiv bt4 bt5) #true)
(check-expect (tree-equiv bt3 bt1) #false)
(check-expect (tree-equiv (make-bt 'hello
                                   (make-bt 'hi 'none bt2)
                                   (make-bt 'wassup 'none 'none))
                          (make-bt 'hello
                                   (make-bt 'wassup 'none 'none)
                                   (make-bt 'hi bt2 'none))) #true)
(check-expect (tree-equiv 'none 'none) #true)
(check-expect (tree-equiv (make-bt 'yo bt3 bt2) (make-bt 'yo bt3 bt3)) #false)
                                   

(define (tree-equiv tree1 tree2)
  (cond
    [(and (symbol? tree1) (symbol? tree2)) #true]
    [(and (symbol? tree1) (bt? tree2)) #false]
    [(and (bt? tree1) (symbol? tree2)) #false]
    [else (and (symbol=? (bt-value tree1) (bt-value tree2))
               (or (and (tree-equiv (bt-left tree1) (bt-left tree2))
                        (tree-equiv (bt-right tree1) (bt-right tree2)))
                   (and (tree-equiv (bt-right tree1) (bt-left tree2))
                        (tree-equiv (bt-left tree1) (bt-right tree2)))))]))

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 6
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


; find-subtree : BT BT -> Boolean
; Searches the first BT to see if there is a subtree equivalent to that of the second BT

(check-expect (find-subtree (make-bt 'hi 'none
                                     (make-bt 'hello 'none 'none))
                            (make-bt 'hello 'none 'none)) #true)
(check-expect (find-subtree 'none 'none) #true)
(check-expect (find-subtree 'none (make-bt 'hi 'none 'none)) #false)
(check-expect (find-subtree (make-bt 'wassup 'none 'none) 'none) #true)

(define (find-subtree tree1 tree2)
  (cond
    [(symbol? tree2) #true]
    [(and (symbol? tree1) (bt? tree2)) #false]
    [(bt? tree2) (or (and (symbol=? (bt-value tree1) (bt-value tree2))
                          (find-subtree (bt-left tree1) (bt-left tree2))
                          (find-subtree (bt-right tree1) (bt-right tree2)))
                     (find-subtree (bt-left tree1) tree2)
                     (find-subtree (bt-right tree1) tree2))]))

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 7
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

(define NONE 'none)
(define bt1-d (make-bt 'd NONE NONE))
(define bt1-e (make-bt 'e NONE NONE))
(define bt1-f (make-bt 'f NONE NONE))
(define bt1-g (make-bt 'g NONE NONE))
(define bt1-b (make-bt 'b bt1-d bt1-e))
(define bt1-c (make-bt 'c bt1-f bt1-g))
(define bt1-a (make-bt 'a bt1-b bt1-c))

(define bt2-d (make-bt 'd NONE NONE))
(define bt2-y (make-bt 'y NONE NONE))
(define bt2-f (make-bt 'f NONE NONE))
(define bt2-b (make-bt 'b bt2-d bt2-y))
(define bt2-x (make-bt 'x bt1-f NONE))
(define bt2-a (make-bt 'a bt2-b bt2-x))

;         bt1-a                     bt2-a
;          A                          A
;       /     \                    /     \
;      B       C                  B       X  
;     / \     / \                / \     / \
;    D   E   F   G              D   Y   F   n
;   / \ / \ / \ / \            / \ / \ / \
;   n n n n n n n n            n n n n n n 
 
;            result1:   A         result2:    A
;                    /     \               /     \
;                   B       n             B       n
;                  / \                   / \
;                 D   n                 n   n
;                / \
;               n   n
;

; max-common-tree: BT BT -> BT
; Returns a tree that shares the maximum number of nodes with both source trees

(check-expect (max-common-tree NONE NONE) NONE)
(check-expect (max-common-tree (make-bt 'a NONE NONE) (make-bt 'a NONE NONE)) (make-bt 'a NONE NONE))
(check-expect (max-common-tree bt1-a bt2-a) (make-bt 'a (make-bt 'b (make-bt 'd NONE NONE) NONE)
                                                     NONE))
(define (max-common-tree tree1 tree2)
  (if (and (bt? tree1) (bt? tree2) (symbol=? (bt-value tree1) (bt-value tree2))) 
      (make-bt (bt-value tree1) (max-common-tree (bt-left tree1) (bt-left tree2))
               (max-common-tree (bt-right tree1) (bt-right tree2)))
      NONE))


;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 8
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; A Dir is one of:
; - 'left
; - 'right

; valid-bst-path? BT Number [List-of Dir] -> Boolean
; Checks to see if search path from [List-of Dir] bring you to the correct node
; if [List-of Dir] is empty, it checks the root of the tree

(check-expect (valid-bst-path? (make-bt 10
                                        (make-bt 6 (make-bt 2 NONE NONE)
                                                 (make-bt 3 NONE NONE))
                                        (make-bt 8 (make-bt 4 NONE NONE)
                                                 (make-bt 5 NONE NONE)))
                               10 empty) #true)
(check-expect (valid-bst-path? (make-bt 10
                                        (make-bt 6 (make-bt 2 NONE NONE)
                                                 (make-bt 3 NONE NONE))
                                        (make-bt 8 (make-bt 4 NONE NONE)
                                                 (make-bt 5 NONE NONE)))
                               4 (list 'right 'left)) #true)
(check-expect (valid-bst-path? (make-bt 10
                                        (make-bt 6 (make-bt 2 NONE NONE)
                                                 (make-bt 3 NONE NONE))
                                        (make-bt 8 (make-bt 4 NONE NONE)
                                                 (make-bt 5 NONE NONE)))
                               3 (list 'left 'right)) #true)
(check-expect (valid-bst-path? (make-bt 10 (make-bt 6 (make-bt 2 NONE NONE)
                                                    (make-bt 3 NONE NONE))
                                        (make-bt 8 (make-bt 4 NONE NONE)
                                                 (make-bt 5 NONE NONE)))
                               3 (list 'left 'right 'left))
              #false)


(define (valid-bst-path? bst n lod)
  (cond
    [(and (empty? lod) (bt? bst)) (= (bt-value bst) n)]
    [(and (cons? lod) (bt? bst)) (if (symbol=? (first lod) 'left)
                                     (valid-bst-path? (bt-left bst) n (rest lod))
                                     (valid-bst-path? (bt-right bst) n (rest lod)))]
    [else #false]))




;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 9
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; merge: (X) [List-of-X] [List-of-X] [X X -> Boolean] -> [List-of-X]
; Takes in two lists of the same type and produces one list with the values of both lists ordered

(check-expect (merge empty empty <) empty)
(check-expect (merge empty (list 1 2 3) <) (list 1 2 3))
(check-expect (merge (list "a" "b" "c") empty string<?) (list "a" "b" "c"))
(check-expect (merge (list 2 4 6 8) (list 1 3 5 7) <) (list 1 2 3 4 5 6 7 8))
(check-expect (merge (list 2 4 6 8) (list 2 3 5 7 11) <) (list 2 2 3 4 5 6 7 8 11))
(check-expect (merge (list "a" "b" "f") (list "c" "d" "e") string<?) (list "a" "b" "c" "d" "e" "f"))

(define (merge l1 l2 lt)
  (cond
    [(and (empty? l1) (empty? l2)) empty]
    [(and (empty? l1) (cons? l2)) l2]
    [(and (empty? l2) (cons? l1)) l1]
    [(and (cons? l1) (cons? l2)) (if (lt (first l1) (first l2))
                                     (cons (first l1) (merge (rest l1) l2 lt))
                                     (cons (first l2) (merge l1 (rest l2) lt)))]))
















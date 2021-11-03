;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname homework5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 1
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 1 - Part A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct CharmBracelet [desc mat rest])
; A CharmBracelet is one of:
; - "clasp"
; - (make-CharmBracelet String Material CharmBracelet)
; Interpretation: A single charm link of a charm bracelet
(define CharmBracelet1 "clasp")
(define CharmBracelet2 (make-CharmBracelet "Unicorn" "silver" CharmBracelet1))
(define CharmBracelet3 (make-CharmBracelet "Double-Heart" "gold" CharmBracelet2))
(define CharmBracelet4 (make-CharmBracelet "Skull-n-Bones" "pewter" CharmBracelet3))
(define CharmBracelet5 (make-CharmBracelet "Unicorn" "silver" CharmBracelet4))
(define CharmBracelet6 (make-CharmBracelet "Double-Heart" "gold" CharmBracelet5))
(define CharmBracelet7 (make-CharmBracelet "Skull-n-Bones" "pewter" CharmBracelet6))
; CharmBracelet -> ???
(define (CharmBracelet-templ cb)
  (cond
    [(string? cb) (... cb ...)]
    [(CharmBracelet? cb) (... (CharmBracelet-desc cb) ...
                              (CharmBracelet-mat cb) ...
                              (CharmBracelet-rest cb) ...)]))

; A Material is one of:
; - "silver"
; - "gold"
; - "pewter"
; Interpretation: the material is a String which is used to make a single charm link
(define material-silver "silver")
(define material-gold "gold")
(define material-pewter "pewter")
; Material -> ???
(define (material-templ mat)
  (cond
    [(string=? mat "silver") ...]
    [(string=? mat "gold") ...]
    [(string=? mat "pewter") ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 1 - Part B
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; bracelet-cost : CharmBracelet -> NNInt
; Returns a NNInt that represents the cost of a CharmBracelet (sum of charm link materials' costs)
(check-expect (bracelet-cost CharmBracelet1) 0)
(check-expect (bracelet-cost CharmBracelet2) 12)
(check-expect (bracelet-cost CharmBracelet3) 27)
(check-expect (bracelet-cost CharmBracelet4) 37)
(check-expect (bracelet-cost CharmBracelet5) 49)
(check-expect (bracelet-cost CharmBracelet6) 64)
(check-expect (bracelet-cost CharmBracelet7) 74)

(define (bracelet-cost cb)
  (cond
    [(string? cb) 0]
    [(CharmBracelet? cb)
     (+ (cost-of-materials cb) (bracelet-cost (CharmBracelet-rest cb)))]))

; cost-of-materials : CharmBracelet -> NNInt
; Returns the cost of one material of a charm link
(check-expect (cost-of-materials CharmBracelet1) 0)
(check-expect (cost-of-materials CharmBracelet2) 12)
(check-expect (cost-of-materials CharmBracelet3) 15)
(check-expect (cost-of-materials CharmBracelet4) 10)

(define (cost-of-materials cb)
  (cond
    [(and (string? cb) (string=? "clasp" cb)) 0]
    [(string=? "gold" (CharmBracelet-mat cb)) 15]
    [(string=? "silver" (CharmBracelet-mat cb)) 12]
    [(string=? "pewter" (CharmBracelet-mat cb)) 10]))



;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 2
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 2 - Part A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct bead [color size rest])

; A FancyBracelet is one of:
; - CharmBracelet
; - (make-bead color size FancyBracelet)
; Interpretation: A FancyBracelet is a of bracelet that is built using both
; CharmBracelet and beads
(define FancyBracelet1 CharmBracelet1)
(define FancyBracelet2 CharmBracelet2)
(define FancyBracelet3 CharmBracelet3)
(define FancyBracelet4 (make-bead "red" 10 FancyBracelet3))
(define FancyBracelet5 (make-bead "green" 20 FancyBracelet4))
(define FancyBracelet6 (make-bead "blue" 30 FancyBracelet5))
; FancyBracelet -> ???
(define (FancyBracelet-templ fb)
  (cond
    [(CharmBracelet? fb) (CharmBracelet-templ fb)]
    [(bead? fb) (... (bead-color fb) ... (bead-size fb) ... (bead-rest fb) ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 2 - Part B
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; count-charms : FancyBracelet -> NNInt
; Returns the amount of charms (not beads) in a FancyBracelet
(check-expect (count-charms FancyBracelet1) 0)
(check-expect (count-charms FancyBracelet2) 1)
(check-expect (count-charms FancyBracelet3) 2)
(check-expect (count-charms FancyBracelet4) 2)
(check-expect (count-charms FancyBracelet5) 2)
(check-expect (count-charms FancyBracelet6) 2)

(define (count-charms fb)
  (cond
    [(string? fb) 0]
    [(bead? fb) (count-charms (bead-rest fb))]
    [(CharmBracelet? fb) (add1 (count-charms (CharmBracelet-rest fb)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 2 - Part C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; upgrade-bracelet : FancyBracelet Color String -> FancyBracelet
; Returns a new FancyBracelet that removes all beads of color Color, and
; exchanges them for silver CharmBracelets with the given figure from string
(check-expect (upgrade-bracelet FancyBracelet1 "red" CharmBracelet2) FancyBracelet1)
(check-expect (upgrade-bracelet FancyBracelet2 "red" CharmBracelet2) CharmBracelet2)
(check-expect (upgrade-bracelet FancyBracelet6 "red" "Doge")
              (make-bead
               "blue"
               30
               (make-bead
                "green"
                20
                (make-CharmBracelet
                 "Doge"
                 "silver"
                 (make-CharmBracelet
                  "Double-Heart"
                  "gold"
                  (make-CharmBracelet "Unicorn" "silver" "clasp"))))))
(check-expect (upgrade-bracelet FancyBracelet6 "green" "Fundies")
              (make-bead
               "blue"
               30
               (make-CharmBracelet
                "Fundies"
                "silver"
                (make-bead
                 "red"
                 10
                 (make-CharmBracelet
                  "Double-Heart"
                  "gold"
                  (make-CharmBracelet "Unicorn" "silver" "clasp"))))))

(define (upgrade-bracelet fb c cb)
  (cond [(string? fb) fb]
        [(CharmBracelet? fb) (make-CharmBracelet (CharmBracelet-desc fb)
                                                 (CharmBracelet-mat fb)
                                                 (upgrade-bracelet (CharmBracelet-rest fb) c cb))]
        [(bead? fb) (if (string=? c (bead-color fb))
                        (make-CharmBracelet cb
                                            "silver"
                                            (upgrade-bracelet (bead-rest fb) c cb))
                        (make-bead (bead-color fb)
                                   (bead-size fb)
                                   (upgrade-bracelet (bead-rest fb) c cb)))]))



;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 3
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

(define-struct student [firstname lastname gpa on-coop])
; A Student is a (make-student String String Number Boolean)
; Interpretation: A (make-student fn ln g c) represents a
; Northeastern student whose first name is fn and last name is ln, with 
; cumulative grade point average g, and for whom c is #true if they are
; currently doing a coop experience this term and #false otherwise.
(define student1 (make-student "Jane" "Smith" 4.0 #true))
(define student2 (make-student "Ashok" "Singhal" 0.0 #false))
; Student -> ???
(define (student-templ st)
  (... (student-firstname st) ...
       (student-lastname st) ...
       (student-gpa st) ...
       (student-on-coop st) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 3 - Part A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A LOS (ListOfStudents) is one of:
; -  empty
; -  (cons Numbers LOS)
; Interpretation: an ordered sequence of students
; Examples:
(define los1 empty)
(define los2 (cons student1 los1))
(define los3 (cons student2 los2))
(define los4 (cons (make-student "Ido" "Tzhori" 4.0 #true) los3))
(define los5 (cons (make-student "Anthony" "Vo" 4.0 #false) los4))
(define los6 (cons (make-student "Bad" "Student" 0.0 #false) los5))
; los-temp : LOS -> ???
(define (los-temp los)
  (cond
    [(empty? los) ...]
    [(cons? los) (... (student-templ (first los)) ... (los-temp (rest los)) ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 3 - Part B
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; count-coop-students : LOS -> NNInt
; Returns the amount of students currently on co-op
(check-expect (count-coop-students los1) 0)
(check-expect (count-coop-students los2) 1)
(check-expect (count-coop-students los3) 1)
(check-expect (count-coop-students los4) 2)
(check-expect (count-coop-students los5) 2)
(check-expect (count-coop-students los6) 2)

(define (count-coop-students los)
  (cond
    [(empty? los) 0]
    [(cons? los) (if (student-on-coop (first los))
                     (+ 1 (count-coop-students (rest los)))
                     (count-coop-students (rest los)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 3 - Part C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; exchange-coop-students : LOS -> LOS
; returns a ListOfStudents that flips each Student's coop status
(check-expect (exchange-coop-students los1) empty)
(check-expect (exchange-coop-students los2)
              (cons (make-student "Jane" "Smith" 4 #false) '()))
(check-expect (exchange-coop-students los3)
              (cons
               (make-student "Ashok" "Singhal" 0 #true)
               (cons (make-student "Jane" "Smith" 4 #false) '())))
(check-expect (exchange-coop-students los4)
              (cons
               (make-student "Ido" "Tzhori" 4 #false)
               (cons
                (make-student "Ashok" "Singhal" 0 #true)
                (cons (make-student "Jane" "Smith" 4 #false) '()))))
(check-expect (exchange-coop-students los5)
              (cons
               (make-student "Anthony" "Vo" 4 #true)
               (cons
                (make-student "Ido" "Tzhori" 4 #false)
                (cons
                 (make-student "Ashok" "Singhal" 0 #true)
                 (cons (make-student "Jane" "Smith" 4 #false) '())))))
(check-expect (exchange-coop-students los6)
              (cons
               (make-student "Bad" "Student" 0 #true)
               (cons
                (make-student "Anthony" "Vo" 4 #true)
                (cons
                 (make-student "Ido" "Tzhori" 4 #false)
                 (cons
                  (make-student "Ashok" "Singhal" 0 #true)
                  (cons (make-student "Jane" "Smith" 4 #false) '()))))))

(define (exchange-coop-students los)
  (cond
    [(empty? los) empty]
    [(cons? los) (cons (make-student
                        (student-firstname (first los))
                        (student-lastname (first los))
                        (student-gpa (first los))
                        (not (student-on-coop (first los))))
                       (exchange-coop-students (rest los)))]))

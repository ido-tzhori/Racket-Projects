;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |HW 7 Final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/string)

; Homework 7
; Ido Tzhori & Nachiket Ganguly

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 2
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; A [List-of SMP] is one of:
; - empty
; - (cons SMP [List-of SMP])
; Interpretation:  A list of items, where every item is a SMP (social media post)

; A SMP is one of:
; - Tweet
; - Facebook
; - Medium
; Interpretation: A social media post holds information about the type
; (tweet, facebook, medium) of social media item it is

(define-struct tweet [text likes retweets])
; A Tweet is a (make-tweet String NNInt NNInt)
; Interpretation: A Tweet holds relevant information about a Tweet social media post
;                 where text is the string representing the text the user typed.
;                 The text of the tweet cannot be longer than 280 characers (X<=280 if X= # of
;                 characters in tweet). The likes represent the number of likes the tweet received,
;                 the retweets represent the number of times people retweeted the tweet
  
(define-struct facebook [text likes])
; A Facebook is a (make-facebook String NNInt)
; Interpretation: A Facebook holds relevant information about a Facebook social media post
;                 where text is the string representinging the text that the user types
;                 and the likes represent the number of likes the facebook received

(define-struct medium [text views])
; A Medium is a (make-medium String NNInt)
; Interpretation: A Medium holds relevant information about a Medium social media post
;                 where text is the string represetning the text the user types and the views
;                 represent the number of times the Medium post was looked at


;;;;;;;;;;;;;; Templates

(define (tweet-templ tw)
  (... (tweet-text tw) ... (tweet-likes tw) ... (tweet-retweets tw)))

(define (facebook-templ fb)
  (... (facebook-text tw) ... (facebook-likes tw)))

(define (medium-templ md)
  (... (medium-text md) ... (medium-views md)))

(define (post-templ smp)
  (cond [(tweet? smp) (... tweet-templ smp ...)]
        [(facebook? smp) (... facebook-templ smp ...)]
        [(medium? smp) (... medium-templ smp ...)]))

(define (list-of-smp-templ losmp)
  (cond
    [(empty? losmp) ...]
    [(cons? losmp) (... (post-templ (first losmp)) ...
                        (list-of-smp-templ (rest losmp)))]))

;;;;;;;;;;;;;; Examples

(define smp1 (make-tweet "wassup" 23 5499))
(define losmp1 (list smp1))

(define smp2 (make-facebook "its going well how about you" 45))
(define losmp2 (list smp1 smp2))

(define smp3 (make-medium "thats good to hear" 34))
(define losmp3 (list smp1 smp2 smp3))

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 3
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

(check-expect (retweet-lists losmp3) (list smp1))
(check-expect (retweet-lists losmp2) (list smp1))
(check-expect (retweet-lists (list smp1 smp2 smp3 (make-tweet "hello" 0 3)))
              (list smp1 (make-tweet "hello" 0 3)))
(check-expect (retweet-lists empty) empty)

; retweet-list: [List-of SMP] -> [List-of SMP]
; Produces a list of social media posts that contain only the tweets that have at least one retweet

(check-expect (retweet-lists empty) empty)
(check-expect (retweet-lists losmp3) (list smp1))
(check-expect (retweet-lists losmp2) (list smp1))
(check-expect (retweet-lists (list smp1 smp2 smp3 (make-tweet "hello" 0 3)))
              (list smp1 (make-tweet "hello" 0 3)))

(define (retweet-lists losmp)
  (filter retweeted? losmp))

; retweeted? : SMP -> Boolean
; Determines whether or not a given SMP has been retweeted

(check-expect (retweeted? smp1) #true)
(check-expect (retweeted? smp2) #false)
(check-expect (retweeted? (make-tweet "boo" 3 0)) #false)
(check-expect (retweeted? (make-medium "hello" 4)) #false)

(define (retweeted? smp)
  (cond
    [(tweet? smp) (retweet? smp)]
    [(facebook? smp) #false]
    [(medium? smp) #false]))


; retweet? Tweet -> Boolean
; Determines if a given Tweet was retweeted

(check-expect (retweet? smp1) #true)
(check-expect (retweet? (make-tweet "hi" 3 0)) #false)

(define (retweet? tw)
  (> (tweet-retweets tw) 0))

;;;;;;;;

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 4
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; utter-failures: [List-of SMP] -> [List-of SMP]
; given a [List-of SMP], returns a new list that only contains SMPs that have 0 views, 0 shares
; 0 likes, or 0 retweets

(check-expect (utter-failures losmp3) empty)
(check-expect (utter-failures (list (make-tweet "siodf" 0 0))) (list (make-tweet "siodf" 0 0)))
(check-expect (utter-failures (cons (make-tweet "hi" 0 0)
                                    (cons (make-facebook "helllo" 0)
                                          (cons (make-medium "iushdfs" 0) empty))))
              (list (make-tweet "hi" 0 0) (make-facebook "helllo" 0) (make-medium "iushdfs" 0)))

(define (utter-failures losmp)
  (filter failure? losmp))

; failure?: SMP -> Boolean
; Determines whether or not a given SMP is an utter failure (0 views, shares, likes, or retweets)

(check-expect (failure? smp1) #false)
(check-expect (failure? smp3) #false)
(check-expect (failure? (make-tweet "hi" 0 0)) #true)
(check-expect (failure? (make-facebook "hello" 0)) #true)

(define (failure? smp)
  (cond
    [(tweet? smp) (tweet-fail? smp)]
    [(facebook? smp) (facebook-fail? smp)]
    [(medium? smp) (medium-fail? smp)]))

; tweet-fail? Tweet -> Boolean
; Determines if a Tweet had 0 likes and 0 retweets

(check-expect (tweet-fail? smp1) #false)
(check-expect (tweet-fail? (make-tweet "0" 0 0)) #true)

(define (tweet-fail? tw)
  (and (= (tweet-retweets tw) 0) (= (tweet-likes tw) 0)))

; facebook-fail? Facebook -> Boolean
; Determines if a Facebook had 0 likes

(check-expect (facebook-fail? smp2) #false)
(check-expect (facebook-fail? (make-facebook "0" 0)) #true)

(define (facebook-fail? fb)
  (= (facebook-likes fb) 0))

; medium-fail? Medium -> Boolean
; Determines if a Medium had 0 views

(check-expect (medium-fail? smp3) #false)
(check-expect (medium-fail? (make-medium "0" 0)) #true)

(define (medium-fail? md)
  (= (medium-views md) 0))
;;;;;;;;;
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 5
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; loSMP-engagement: [List-of SMP] -> NNN

(check-expect (total-engagement empty) 0)
(check-expect (total-engagement losmp1) 5522)
(check-expect (total-engagement losmp2) (+ 5522 45))
(check-expect (total-engagement losmp3) (+ 5522 45 34))

(define (total-engagement losmp)
  (cond
    [(empty? losmp) 0]
    [(cons? losmp) (foldr + 0 (engagement-map losmp))]))
  
; engagement-map: [List-of SMP] -> [List-of number]
; Every element of the new [List-of SMP] is the engagement of the given SMP

(check-expect (engagement-map empty) empty)
(check-expect (engagement-map losmp1) (list 5522))
(check-expect (engagement-map losmp2) (list 5522 45))
(check-expect (engagement-map losmp3) (list 5522 45 34))

(define (engagement-map losmp)
  (map engagement losmp))

; engagement: SMP -> NNN
; Determines the total engagement of a given SMP
(check-expect (engagement smp1) 5522)
(check-expect (engagement smp2) 45)
(check-expect (engagement smp3) 34)

(define (engagement smp)
  (cond
    [(tweet? smp) (twitter-engagement smp)]
    [(facebook? smp) (facebook-likes smp)]
    [(medium? smp) (medium-views smp)]))

; twitter-engagement: tweet -> NNN
; Adds the likes and retweet of a tweet
(check-expect (twitter-engagement (make-tweet "wassup" 0 0)) 0)
(check-expect (twitter-engagement (make-tweet "wassup" 23 5499)) 5522)
(check-expect (twitter-engagement (make-tweet "hello" 25 75)) 100)

(define (twitter-engagement tw)
  (+ (tweet-likes tw) (tweet-retweets tw)))


;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 6
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; crosspost: [List-of SMP] -> [List-of SMP]

; Interp: consumes a list of social media items, and produces a list that has two items for each
; item in the given list. Tweets must be limited to 280 characters

(check-expect (crosspost empty) empty)
(check-expect (crosspost losmp1) (list (make-facebook "wassup" 0) (make-medium "wassup" 0)))
(check-expect (crosspost losmp2) (list (make-facebook "wassup" 0) (make-medium "wassup" 0)
                                       (make-tweet "its going well how about you" 0 0)
                                       (make-medium "its going well how about you" 0)))
(check-expect (crosspost losmp3) (list (make-facebook "wassup" 0) (make-medium "wassup" 0)
                                       (make-tweet "its going well how about you" 0 0)
                                       (make-medium "its going well how about you" 0)
                                       (make-facebook "thats good to hear" 0)
                                       (make-tweet "thats good to hear" 0 0)))

(define (crosspost losmp)
  (cond
    [(empty? losmp) empty]
    [(cons? losmp) (append (create-2-posts (first losmp))
                           (crosspost (rest losmp)))]))

; create-2-posts: SMP -> [List-of SMP]
; Interp: Builds the other two posts based of the SMP entered
(check-expect (create-2-posts smp1) (list (make-facebook "wassup" 0) (make-medium "wassup" 0)))
(check-expect (create-2-posts smp2) (list (make-tweet "its going well how about you" 0 0)
                                          (make-medium "its going well how about you" 0)))
(check-expect (create-2-posts smp3) (list (make-facebook "thats good to hear" 0)
                                          (make-tweet "thats good to hear" 0 0)))

(define (create-2-posts smp)
  (cond
    [(tweet? smp) (list (tweet->facebook smp) (tweet->medium smp))]
    [(facebook? smp) (list (facebook->tweet smp) (facebook->medium smp))]
    [(medium? smp) (list (medium->facebook smp) (medium->tweet smp))]))


; This is 282 character long

(define long-tweet "Otters otters otters otters otters otters otters otters otters otters
 otters otters otters otters otters otters otters otters otters otters otters otters otters
 otters otters otters otters otters otters otters otters otters otters otters otters otters
 otters otters otters otters")


; facebook->tweet : Facebook -> Tweet
; Converts a facebook to an at most 280 character tweet
(check-expect (facebook->tweet (make-facebook "crazy wheather today right?" 45))
              (make-tweet "crazy wheather today right?" 0 0))
(check-expect (facebook->tweet smp2)
              (make-tweet "its going well how about you" 0 0))
(check-expect (facebook->tweet (make-facebook "dogs are cute" 25))
              (make-tweet "dogs are cute" 0 0))
(define (facebook->tweet fb)
  (make-tweet (text-280 (facebook-text fb)) 0 0))


; facebook->tweet : Facebook -> Medium
; Converts a facebook to a medium

(check-expect (facebook->medium (make-facebook "its going well how about you" 0))
              (make-medium "its going well how about you" 0))
(check-expect (facebook->medium smp2)
              (make-medium "its going well how about you" 0))
(check-expect (facebook->medium (make-facebook "crazy day today" 23))
              (make-medium "crazy day today" 0))

(define (facebook->medium fb)
  (make-medium (facebook-text fb) 0))

; tweet->facebook : Tweet -> Facebook
; Converts a tweet to a facebook
(check-expect (tweet->facebook (make-tweet "wassup" 23 5499)) (make-facebook "wassup" 0))
(check-expect (tweet->facebook smp1) (make-facebook "wassup" 0))
(check-expect (tweet->facebook (make-tweet "good day" 2 22)) (make-facebook "good day" 0))

(define (tweet->facebook tw)
  (make-facebook (tweet-text tw) 0))


; tweet->medium : Tweet -> Medium
; Converts a tweet to a medium
(check-expect (tweet->medium (make-tweet "hello" 23 5499)) (make-medium "hello" 0))
(check-expect (tweet->medium smp1) (make-medium "wassup" 0))
(check-expect (tweet->medium (make-tweet "don't forget to vote!" 500 200000))
              (make-medium "don't forget to vote!" 0))

(define (tweet->medium tw)
  (make-medium (tweet-text tw) 0))

; medium->tweet : Medium -> Tweet
; Converts a medium to an at most 280 character tweet
(check-expect (medium->tweet (make-medium "thats bad to hear" 14))
              (make-tweet "thats bad to hear" 0 0))
(check-expect (medium->tweet smp3)
              (make-tweet "thats good to hear" 0 0))
(check-expect (medium->tweet (make-medium "wheather is crazy right?" 34))
              (make-tweet "wheather is crazy right?" 0 0))

(define (medium->tweet md)
  (make-tweet (text-280 (medium-text md)) 0 0))

; medium->facebook : Medium -> Facebook
; Converts a medium to a facebook
(check-expect (medium->facebook (make-medium "saw a cool car today" 33))
              (make-facebook "saw a cool car today" 0))
(check-expect (medium->facebook smp3)
              (make-facebook "thats good to hear" 0))
(check-expect (medium->facebook (make-medium "good morning america" 44))
              (make-facebook "good morning america" 0))

(define (medium->facebook md)
  (make-facebook (medium-text md) 0))

; text-280: String -> String
; Cuts of all characters of a string passed the 280th character

(check-expect (text-280 "hello") "hello")
(check-expect (text-280 "hi") "hi")
(check-expect (text-280 long-tweet) (substring long-tweet 0 279))

(define (text-280 str)
  (if (> (string-length str) 280)
      (substring str 0 279)
      str))

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 7
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; append-apply-to-all: {X Z} [X -> List-of-Z] [List-of X]  -> [List-of-Z]
; 

(check-expect (apply-to-all explode (list "dog" "cat" "hello" "poakwd"))
              (list "d" "o" "g" "c" "a" "t" "h" "e" "l" "l" "o" "p" "o" "a" "k" "w" "d"))
(check-expect (apply-to-all string->list (list "dog" "cate"))
              (list #\d #\o #\g #\c #\a #\t #\e))
(check-expect (apply-to-all string-split (list "doggy door" "kitty cat" "hello world" "poakwd"))
              (list "doggy" "door" "kitty" "cat" "hello" "world" "poakwd"))

(define (apply-to-all f l)
  (local [(define (list-of-results fn x)
            (fn x))]
    (cond
      [(empty? l) empty]
      [(cons? l) (append (list-of-results f (first l))
                         (apply-to-all f (rest l)))])))


;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 8
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; crosspost/v2: [List-of SMP] -> [List-of SMP]
; Interp: consumes a list of social media items, and produces a list that has two items for each
; item in the given list. Tweets must be limited to 280 characters

(check-expect (crosspost/v2 empty) empty)
(check-expect (crosspost/v2 losmp1) (list (make-facebook "wassup" 0) (make-medium "wassup" 0)))
(check-expect (crosspost/v2 losmp2) (list (make-facebook "wassup" 0) (make-medium "wassup" 0)
                                          (make-tweet "its going well how about you" 0 0)
                                          (make-medium "its going well how about you" 0)))
(check-expect (crosspost/v2 losmp3) (list (make-facebook "wassup" 0) (make-medium "wassup" 0)
                                          (make-tweet "its going well how about you" 0 0)
                                          (make-medium "its going well how about you" 0)
                                          (make-facebook "thats good to hear" 0)
                                          (make-tweet "thats good to hear" 0 0)))

(check-expect (crosspost/v2 (list (make-tweet "what" 120938 1903) (make-medium "7" 7)))
              (list (make-facebook "what" 0) (make-medium "what" 0) (make-facebook "7" 0)
                    (make-tweet "7" 0 0)))

(define (crosspost/v2 losmp)
  (cond
    [(empty? losmp) empty]
    [(cons? losmp) (apply-to-all create-2-posts losmp)]))

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 9
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; items-since-tweet: [List-of SMP] String -> [List-of SMP]
; Produces a list of social media posts that were made a single tweet that was chosen
; by the text of the tweet

(check-expect (items-since-tweet losmp3 "wassup") losmp3)
(check-expect (items-since-tweet (list smp3 smp2 (make-tweet "h" 2 3) smp1 smp2 smp3) "h")
              (list (make-tweet "h" 2 3) (make-tweet "wassup" 23 5499)
                    (make-facebook "its going well how about you" 45)
                    (make-medium "thats good to hear" 34)))
(check-expect (items-since-tweet empty "empty") empty)

(define (items-since-tweet losmp x)
  (cond
    [(empty? losmp) empty]
    [(cons? losmp) (if (text? (first losmp) x)
                       (cons (first losmp) (rest losmp))
                       (items-since-tweet (rest losmp) x))]))

; text?: SMP String -> Boolean
; determines whether or not the text matches the given text. Produces false for facebook or medium

(check-expect (text? smp1 "wassup") #true)
(check-expect (text? smp1 "hi") #false)
(check-expect (text? smp2 "doesnt matter") #false)
(check-expect (text? smp3 "catdog") #false)

(define (text? smp x)
  (cond
    [(tweet? smp) (string=? x (tweet-text smp))]
    [(facebook? smp) #false]
    [(medium? smp) #false]))

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 10
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; items-since-10-likes: [List-of SMP] -> [List-of SMP]
; Creates a list of of all SMPs that were made after the first facebook post with 10 likes or more
; the list includes the first facebook post with 10 likes

(check-expect (items-since-10-likes empty) empty)
(check-expect (items-since-10-likes losmp3) (list (make-facebook "its going well how about you" 45)
                                                  (make-medium "thats good to hear" 34)))

(check-expect (items-since-10-likes (list smp1 smp3 (make-facebook "h" 4) smp2 (make-medium "j" 3)))
              (list (make-facebook "its going well how about you" 45) (make-medium "j" 3)))


(define (items-since-10-likes losmp)
  (cond
    [(empty? losmp) empty]
    [(cons? losmp) (if (facebook-10? (first losmp))
                       (cons (first losmp) (rest losmp))
                       (items-since-10-likes (rest losmp)))]))

; facebook-10?: SMP -> Boolean
; Determines whether or not a given smp is a facebook and has greater than 10 likes

(check-expect (facebook-10? smp2) #true)
(check-expect (facebook-10? smp1) #false)
(check-expect (facebook-10? smp3) #false)
(check-expect (facebook-10? (make-facebook "01000001 00101011" 9)) #false)

(define (facebook-10? smp)
  (cond
    [(tweet? smp) #false]
    [(facebook? smp) (<= 10 (facebook-likes smp))]
    [(medium? smp) #false]))

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 11
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; suffix-from-2500: [List-of numbers] -> [List-of numbers]
; consumes a list of numbers, and produces the suffix of that list that begins from the first 2500
; that occurs in the given list. The produced list must include the first 2500.

(check-expect (suffix-from-2500 empty) empty)
(check-expect (suffix-from-2500 (list 1 2 3 2500 4 3 2 1)) (list 2500 4 3 2 1))
(check-expect (suffix-from-2500 (list 2500 4 3 2 1)) (list 2500 4 3 2 1))
(check-expect (suffix-from-2500 (list 4 3 2 1)) empty)

(define (suffix-from-2500 losmp)
  (cond
    [(empty? losmp) empty]
    [(cons? losmp) (if (2500? (first losmp))
                       (cons (first losmp) (rest losmp))
                       (suffix-from-2500 (rest losmp)))]))

; 2500?: Number -> Boolean
; Determines whether or not a given number is 2500

(check-expect (2500? 32) #false)
(check-expect (2500? 2499) #false)
(check-expect (2500? 2500) #true)

(define (2500? num)
  (= num 2500))

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 12
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

(check-expect (suffix-abs empty text? "") empty)
(check-expect (suffix-abs losmp3 text? "wassup") losmp3)
(check-expect (suffix-abs (list (make-tweet "gubi" 3 11) smp2 smp3 smp1 smp1) text? "gubi")
              (list (make-tweet "gubi" 3 11) smp2 smp3 smp1 smp1))

(check-expect (suffix-abs empty facebook-x? 10) empty)
(check-expect (suffix-abs (list smp1 smp3 smp1 (make-facebook "3" 4) smp2 smp3 smp3 smp3)
                          facebook-x? 10)
              (list
               (make-facebook "its going well how about you" 45)
               (make-medium "thats good to hear" 34)
               (make-medium "thats good to hear" 34)
               (make-medium "thats good to hear" 34)))
(check-expect (suffix-abs (list smp3 smp1 smp2 smp2) facebook-x? 10)
              (list smp2 smp2))

(check-expect (suffix-abs empty x? 2500) empty)
(check-expect (suffix-abs (list 1 2 2500 23 4 5) x? 2500) (list 2500 23 4 5))
(check-expect (suffix-abs (list 1 2 3) x? 2500) empty)

; {X} [List-of SMP] [X X -> Boolean] X -> [List-of SMP]
; Creates a [List-of SMP] based on a function that runs on the list and an x (string or number)

(define (suffix-abs losmp fn x)
  (cond
    [(empty? losmp) empty]
    [(cons? losmp) (if (fn (first losmp) x)
                       (cons (first losmp) (rest losmp))
                       (suffix-abs (rest losmp) fn x))]))

; facebook-x?: SMP Number -> Boolean
; Determines whether or not a given smp is a facebook and has greater than a certain number of likes

(check-expect (facebook-x? smp2 32) #true)
(check-expect (facebook-x? smp1 54) #false)
(check-expect (facebook-x? smp3 32) #false)
(check-expect (facebook-x? (make-facebook "01000001 00101011" 9) 10) #false)

(define (facebook-x? smp num)
  (cond
    [(tweet? smp) #false]
    [(facebook? smp) (<= num (facebook-likes smp))]
    [(medium? smp) #false]))

; x?: number Number -> Boolean
; Determines whether or not a given number is the same as another number

(check-expect (x? 2500 0) #false)
(check-expect (x? 2500 2499) #false)
(check-expect (x? 2500 2500) #true)

(define (x? num x)
  (= num x))

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;; Exercise 13
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; items-since-tweet/v2: [List-of SMP] String -> [List-of SMP]
; Produces a list of social media posts that were made a single tweet that was chosen
; by the text of the tweet

(check-expect (items-since-tweet/v2 empty "empty") empty)
(check-expect (items-since-tweet/v2 losmp3 "wassup") losmp3)
(check-expect (items-since-tweet/v2 (list smp3 smp2 (make-tweet "h" 2 3) smp1 smp2 smp3) "h")
              (list (make-tweet "h" 2 3) (make-tweet "wassup" 23 5499)
                    (make-facebook "its going well how about you" 45)
                    (make-medium "thats good to hear" 34)))

(define (items-since-tweet/v2 losmp x)
  (suffix-abs losmp text? x))


; items-after-10-likes/v2: [List-of SMP] Number -> [List-of SMP]
; consumes a list of social media items (in order), and produces all the social media posts that
; were made after the first Facebook post that received 10 or more likes.

(check-expect (items-since-10-likes/v2 empty 10) empty)
(check-expect (items-since-10-likes/v2 losmp3 10) (list
                                                   (make-facebook "its going well how about you" 45)
                                                   (make-medium "thats good to hear" 34)))
(check-expect (items-since-10-likes/v2 (list smp1 smp3 (make-facebook "h" 4)
                                             smp2 (make-medium "j" 3))
                                       10)
              (list (make-facebook "its going well how about you" 45)
                    (make-medium "j" 3)))


(define (items-since-10-likes/v2 losmp x)
  (suffix-abs losmp facebook-x? x))

; suffix-from-2500/v2: [List-of SMP] Number -> [List-of SMP]
; consumes a list of numbers, and produces the suffix of that list that begins from the first x
; that occurs in the given list. The produced list must include the first 2500.

(check-expect (suffix-from-2500/v2 empty 2500) empty)
(check-expect (suffix-from-2500/v2 (list 1 2 3 2500 4 3 2 1) 2500) (list 2500 4 3 2 1))
(check-expect (suffix-from-2500/v2 (list 2500 4 3 2 1) 2500) (list 2500 4 3 2 1))
(check-expect (suffix-from-2500/v2 (list 4 3 2 1) 2500) '())

(define (suffix-from-2500/v2 losmp x)
  (suffix-abs losmp x? x))


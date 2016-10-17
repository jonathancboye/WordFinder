;FILE: project_1
;AUTHOR: Jonathan Carpenter
;EMAIL: carpenter.102@wright.edu
;CLASS: CS3180
;PROJECT: Big Project 1

#lang racket
;Reads in words from the current-input-port
(define (read-in-wordlist)
  (if (eof-object? (peek-byte (current-input-port)))
      '()
      (cons(string-upcase (read-line (current-input-port)))
           (read-in-wordlist))))

;Set current input port to linuxwords.txt
(current-input-port  (open-input-file "linuxwords.txt"))

;Return the n-th element from a list
(define (n-th num l)
  (if (equal? num 0) (car l)
        (n-th (- num 1) (cdr l))))

;Return a random vowel
(define (random-vowel)
  (n-th (random 5) '("A" "E" "I" "O" "U")))

;Return a random consonant
(define (random-consonant)
  (n-th (random 21) '("B" "C" "D" "F" "G" "H" "J" "K" "L" "M" "N" "P" "Qu" "R" "S" "T" "V" "W" "X" "Y" "Z"))) 

;Returns a random list of size n composed of elements in another list
(define (random-list n l)
  (if (<= n 0) '()
      (cons (n-th (random (length l)) l)
            (random-list (- n 1) l))))
     
;A row seperating string
(define row-seperator "|---||---||---||---||---|")

;Display a row of letters
(define (display-row row)
  (foldl string-append ""
         (map (lambda (x) (if (equal? x "Qu")
                              (string-append "|"x" |")
                              (string-append "| " x " |"))) row)))

;Display boggle board grid
(define (display-grid grid)
  (cond ((not (null? grid))
         (displayln row-seperator)
         (displayln (display-row (car grid)))
         (display-grid (cdr grid)))
        (else (displayln row-seperator))))

;Remove one letter from a list
(define (remove-one-letter-from-list letter l)
  (cond ((null? l) '())
        ((equal? letter (car l)) (cdr l))
        (else (cons (car l)
                    (remove-one-letter-from-list letter (cdr l))))))

;Return true if list1 is a sub-list of list2
(define (sub-list? list1 list2)
  (cond ((null? list1) #t)
        ((and (and(equal? (car list1) "Q")
                  (not (null? (cdr list1))))
              (equal? (cadr list1) "U")
              (member "Qu" list2))
         (sub-list? (cddr list1)
                    (remove-one-letter-from-list "Qu" list2)))
        ((member (car list1) list2)
        (sub-list? (cdr list1)
                   (remove-one-letter-from-list (car list1)
                                                list2)))
       (else #f)))

;See if a word can be constructed with a list of letters
(define (can-make-word? word letters)
  (if (sub-list? (map string (string->list word))
                 letters)
                 #t
                 #f))

;Finds all the words on the board in the given list of words and returns a them in a list
(define (find-words letters list-of-words)
  (cond ((null? list-of-words) '())
        ((and (>= (string-length (car list-of-words)) 4)
              (can-make-word? (car list-of-words) letters))
         (cons (car list-of-words)
               (find-words letters (cdr list-of-words))))
        (else (find-words letters (cdr list-of-words)))))

;Count vowels in a list of strings
(define (vowel-count l)
  (foldl + 0 (map (lambda (x)
                    (if (member x '("A" "E" "I" "O" "U")) 1 0))
                  l)))

;Returns a combined list of vowels and consonants in random order
(define (make-a-row vowels consonants size)
  (cond ((and (null? vowels)
              (null? consonants)) '())
        ((<= size 0) '())
        ((null? vowels) (cons (car consonants)
                              (make-a-row vowels (cdr consonants) (- size 1))))
        ((null? consonants) (cons (car vowels)
                                  (make-a-row (cdr vowels) consonants (- size 1))))
        (else
         (if (equal? (random 2) 1)
             (cons (car vowels)
                   (make-a-row (cdr vowels) consonants (- size 1)))
             (cons (car consonants)
                   (make-a-row vowels (cdr consonants) (- size 1)))))))

;Returns the first n elements from a list
(define (first-n n l)
  (cond ((null? l) '())
        ((<= n 0) '())
        (else (cons (car l) (first-n (- n 1) (cdr l))))))

;Returns a list l, and breaks it into a list of rows of a given size
(define (make-a-grid rows size l)
  (cond ((null? l) '())
        ((<= rows 0) '())
        ((>= (length l) size) (cons (first-n 5 l)
                                    (make-a-grid (- rows 1)
                                                 size
                                                 (cdr (cddddr l)))))))
;Display score
(define (display-score a-solution score)
  (cond ((null? a-solution) (display "Total ")
                            (display score)
                            (display " Points")
                            (newline))
        (else(display (- (string-length (car a-solution)) 3))
             (display " ")
             (display (car a-solution))
             (newline)
             (display-score (cdr a-solution) (+ score (- (string-length (car a-solution)) 3))))))

(define (instructors-player gameGrid) () )

;======================
;======= MAIN =========
;======================
;seed random number generator
(random-seed (current-seconds))

;A random list of vowels between 7 and 12
(define random-vowels
  (map (lambda (x) (random-vowel)) (range (+ (random 6) 7))))

;A random list of consonants that is 25 - size of random-vowels
(define random-consonants
  (map (lambda (x) (random-consonant))
       (range (+ (- 25 (length random-vowels))))))

(define wordlist (read-in-wordlist))

(define all-letters (make-a-row random-vowels random-consonants 25))

(define 5-by-5-grid (make-a-grid 5 5 all-letters))

(instructors-player 5-by-5-grid)

(display-grid 5-by-5-grid)

(define solution (find-words all-letters wordlist))

(display-score solution 0)


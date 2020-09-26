
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; Helper functions
(define ones (lambda () (cons 1 ones)))


;; put your code below

;; problem 1
;; (sequence 2 3 11)
(define (sequence spacing low high)
    (if (> low high)
        null
        (cons low (sequence spacing (+ low spacing) high))))

;; problem 2
;; (string-append-map (list "dan" "dog" "curry" "dog2") ".jpg")
(define (string-append-map xs suffix)
    (map (lambda (x)
            (string-append x suffix)) xs))


;; problem 3
;; (list-nth-mod (list 0 1 2 3 4) 2)
(define (list-nth-mod xs n)
    (if (< n 0) 
        (error "list-nth-mod: negative number")
        (if (null? xs)
            (error "list-nth-mod: empty list")
            (car (list-tail xs (remainder n (length xs)))))))


;; problem 4
(define (stream-for-k-steps stream k)
    (if (= k 0)
        null
        (let ([pr (stream)])
            (cons (car pr) (stream-for-k-steps (cdr pr) (- k 1))))))

;; problem 5
;; (stream-for-k-steps funny-number-stream 16)  return '(1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16)
(define funny-number-stream
    (letrec ([f (lambda(x) (cons (cond [(= (remainder x 5) 0) (- 0 x)]
                                       [#t x]) (lambda() (f (+ 1 x)))))])
                                       (lambda() (f 1))))

;; problem 6
(define dan-then-dog
    (letrec ([f (lambda(x) (cons (cond [(= (remainder x 2) 0) "dan.jpg"]
                                       [#t "dog.jpg"])
                                       (lambda() (f (+ 1 x)))))])
    (lambda() (f 1))))



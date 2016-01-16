
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([x (s)])
        (cons (car x) (stream-for-n-steps (cdr x) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (= 0 (remainder x 5))
                          (- x)
                          x)
                          (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (s)
                (cons s (lambda () (f (if (equal? s "dog.jpg") "dan.jpg" "dog.jpg")))))])
    (lambda() (f "dan.jpg"))))

(define (stream-add-zero s)
  (lambda ()
    (let ([x (s)])
          (cons (cons 0 (car x)) (stream-add-zero(cdr x))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (i)
                (cons (cons (list-nth-mod xs i) (list-nth-mod ys i))
                            (lambda () (f (+ i 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (i)
              (if (>= i (vector-length vec))
                  #f
                  (let ([x (vector-ref vec i)])
                    (cond [(not (pair? x)) (f (+ i 1))]
                          [(equal? (car x) v) x]
                          [#t (f (+ i 1))]))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [pos 0]
           [f (lambda (v)
                (let ([ans1 (vector-assoc v cache)])
                      (if ans1
                          (cdr ans1)
                          (let ([ans2 (assoc v xs)])
                            (begin
                              (vector-set! cache pos (cons v ans2))
                              (set! pos (let ([new-pos (+ pos 1)])
                                          (if (= new-pos n)
                                              0
                                              new-pos)))
                              ans2)))))])
    f))
                
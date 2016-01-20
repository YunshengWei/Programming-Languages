;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist rl)
  (if (null? rl)
      (aunit)
      (apair (car rl) (racketlist->mupllist (cdr rl)))))

;; Problem 2

(define (mupllist->racketlist ml)
  (if (aunit? ml)
      null
      (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))))

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(fun? e)
         (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
               (if (and (int? v1) (int? v2))
                   (if (> (int-num v1) (int-num v2))
                       (eval-under-env (ifgreater-e3 e) env)
                       (eval-under-env (ifgreater-e4 e) env))
                   (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (eval-under-env (mlet-body e)
                         (cons (cons (mlet-var e)
                               (eval-under-env (mlet-e e) env)) env))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
               (if (not (closure? v1))
                   (error "MUPL call applied to non-closure")
                   (let* ([ce (closure-env v1)]
                          [cf (closure-fun v1)]
                          [fn (fun-nameopt cf)]
                          [pn (fun-formal cf)]
                          [new-env1 (if fn (cons (cons fn v1) ce) ce)]
                          [new-env2 (cons (cons pn v2) new-env1)])
                          (eval-under-env (fun-body cf) new-env2))))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst))
            (cdr (car lstlst))
            (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4 e3)))))

;; Problem 4

(define mupl-map
  (fun #f "fn"
       (fun "map-aux" "list"
            (ifaunit (var "list")
                    (aunit)
                    (apair (call (var "fn") (fst (var "list")))
                           (call (var "map-aux") (snd (var "list"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (fun #f "list"
                  (call (call (var "map") (fun #f "x" (add (var "x") (var "i"))))
                        (var "list"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec ([helper
            (lambda (e)
              (cond [(var? e) (cons e (set (var-string e)))]
                    [(int? e) (cons e (set))]
                    [(add? e)
                     (let ([v1 (helper (add-e1 e))]
                           [v2 (helper (add-e2 e))])
                       (cons (add (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
                    [(ifgreater? e)
                     (let ([v1 (helper (ifgreater-e1 e))]
                           [v2 (helper (ifgreater-e2 e))]
                           [v3 (helper (ifgreater-e3 e))]
                           [v4 (helper (ifgreater-e4 e))])
                       (cons (ifgreater (car v1) (car v2) (car v3) (car v4))
                             (set-union (cdr v1) (cdr v2) (cdr v3) (cdr v4))))]
                    [(fun? e)
                     (let* ([v (helper (fun-body e))]
                            [fv (set-remove (cdr v) (fun-formal e))])
                       (cons (fun-challenge (fun-nameopt e) (fun-formal e) (car v) fv) fv))]
                    [(call? e)
                     (let ([v1 (helper (call-funexp e))]
                           [v2 (helper (call-actual e))])
                       (cons (call (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
                    [(mlet? e)
                     (let ([v1 (helper (mlet-e e))]
                           [v2 (helper (mlet-body e))])
                       (cons (mlet (mlet-var e) (car v1) (car v2))
                             (set-union (cdr v1) (set-remove (cdr v2) (mlet-var e)))))]
                    [(apair? e)
                     (let ([v1 (helper (apair-e1 e))]
                           [v2 (helper (apair-e2 e))])
                       (cons (apair (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
                    [(fst? e)
                     (let ([v (helper (fst-e e))])
                       (cons (fst (car v)) (cdr v)))]
                    [(snd? e)
                     (let ([v (helper (snd-e e))])
                       (cons (snd (car v)) (cdr v)))]
                    [(aunit? e) (cons e (set))]
                    [(isaunit? e)
                     (let ([v (helper (isaunit-e e))])
                       (cons (isaunit (car v)) (cdr v)))]))])
    (car (helper e))))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(fun-challenge? e)
         (letrec ([g (lambda (l)
                             (if (null? l)
                                 null
                                 (let ([t (assoc (car l) env)])
                                   (if t
                                       (cons t (g (cdr l)))
                                       (g (cdr l))))))])
           (closure (g (set->list (fun-challenge-freevars e))) e))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
               (if (and (int? v1) (int? v2))
                   (if (> (int-num v1) (int-num v2))
                       (eval-under-env-c (ifgreater-e3 e) env)
                       (eval-under-env-c (ifgreater-e4 e) env))
                   (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (eval-under-env-c (mlet-body e)
                           (cons (cons (mlet-var e)
                                       (eval-under-env-c (mlet-e e) env)) env))]
        [(call? e)
         (let ([v1 (eval-under-env-c (call-funexp e) env)]
               [v2 (eval-under-env-c (call-actual e) env)])
               (if (not (closure? v1))
                   (error "MUPL call applied to non-closure")
                   (let* ([ce (closure-env v1)]
                          [cf (closure-fun v1)]
                          [fn (fun-challenge-nameopt cf)]
                          [pn (fun-challenge-formal cf)]
                          [new-env1 (if fn (cons (cons fn v1) ce) ce)]
                          [new-env2 (cons (cons pn v2) new-env1)])
                          (eval-under-env-c (fun-challenge-body cf) new-env2))))]
        [(apair? e)
         (apair (eval-under-env-c (apair-e1 e) env) (eval-under-env-c (apair-e2 e) env))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env-c (isaunit-e e) env))
             (int 1)
             (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

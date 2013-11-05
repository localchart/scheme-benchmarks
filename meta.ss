;; a meta-circular interpreter (reflection tower) is an
;; interpreter which can interpret itself to interpret
;; itself to interpret itself ...

;; This version saves indentation by defining 'cond'.

;; author: Yin Wang (yinwang0@gmail.com)


(define Y
  '(lambda (f)
     ((lambda (u) (u u))
      (lambda (x) (f (lambda (t) ((x x) t)))))))


(define interp-text
  `(,Y
    (lambda (interp)
      (lambda (exp)
        (lambda (env)
          (lambda (k)
            (cond
             [(number? exp) (k exp)]
             [(boolean? exp) (k exp)]
             [(string? exp) (k exp)]
             [(symbol? exp) (k (env exp))]
             [(eq? 'cond (car exp))
              ((((,Y (lambda (loop)
                       (lambda (cls)
                         (lambda (env)
                           (lambda (k)
                             (((interp (car (car cls))) env)
                              (lambda (t)
                                (if t
                                    (((interp (car (cdr (car cls)))) env) k)
                                    (((loop (cdr cls)) env) k)))))))))
                 (cdr exp)) env) k)]
             [(eq? 'eq? (car exp))
              (((interp (car (cdr exp))) env)
               (lambda (v1)
                 (((interp (car (cdr (cdr exp)))) env)
                  (lambda (v2) (k (eq? v1 v2))))))]
             [(eq? '= (car exp))
              (((interp (car (cdr exp))) env)
               (lambda (v1)
                 (((interp (car (cdr (cdr exp)))) env)
                  (lambda (v2) (k (= v1 v2))))))]
             [(eq? '* (car exp))
              (((interp (car (cdr exp))) env)
               (lambda (v1)
                 (((interp (car (cdr (cdr exp)))) env)
                  (lambda (v2) (k (* v1 v2))))))]
             [(eq? 'cons (car exp))
              (((interp (car (cdr exp))) env)
               (lambda (v1)
                 (((interp (car (cdr (cdr exp)))) env)
                  (lambda (v2) (k (cons v1 v2))))))]
             [(eq? 'quote (car exp)) (k (car (cdr exp)))]
             [(eq? 'sub1 (car exp))
              (((interp (car (cdr exp))) env) (lambda (v) (k (sub1 v))))]
             [(eq? 'number? (car exp))
              (((interp (car (cdr exp))) env) (lambda (v) (k (number? v))))]
             [(eq? 'boolean? (car exp))
              (((interp (car (cdr exp))) env) (lambda (v) (k (boolean? v))))]
             [(eq? 'string? (car exp))
              (((interp (car (cdr exp))) env) (lambda (v) (k (string? v))))]
             [(eq? 'symbol? (car exp))
              (((interp (car (cdr exp))) env) (lambda (v) (k (symbol? v))))]
             [(eq? 'zero? (car exp))
              (((interp (car (cdr exp))) env) (lambda (v) (k (zero? v))))]
             [(eq? 'null? (car exp))
              (((interp (car (cdr exp))) env) (lambda (v) (k (null? v))))]
             [(eq? 'car (car exp))
              (((interp (car (cdr exp))) env) (lambda (v) (k (car v))))]
             [(eq? 'cdr (car exp))
              (((interp (car (cdr exp))) env) (lambda (v) (k (cdr v))))]
             [(eq? 'if (car exp))
              (((interp (car (cdr exp))) env)
               (lambda (t)
                 (if t
                     (((interp (car (cdr (cdr exp)))) env) k)
                     (((interp (car (cdr (cdr (cdr exp))))) env) k))))]
             [(eq? 'lambda (car exp))
              (k (lambda (a)
                   (lambda (k)
                     (((interp (car (cdr (cdr exp))))
                       (lambda (x^)
                         (if (eq? x^ (car (car (cdr exp)))) a (env x^))))
                      k))))]
             [(eq? 'rho (car exp))
              (k (lambda (a)
                   (lambda (k)
                     (((interp (car (cdr (cdr exp))))
                       (lambda (x^)
                         (cond
                          [(eq? x^ (car (cdr (cdr (car (cdr exp))))))
                           (lambda (a) (lambda (k^) (k a)))]
                          [(eq? x^ (car (cdr (car (cdr exp))))) env]
                          [(eq? x^ (car (car (cdr exp)))) a]
                          [#t (env x^)])))
                      k))))]
             [#t
              (((interp (car exp)) env)
               (lambda (v1)
                 (((interp (car (cdr exp))) env)
                  (lambda (v2)
                    ((v1 v2) k)))))])))))))


(define sub1
  (lambda (x)
    (- x 1)))


;;;;;;;;; nested evaluators ;;;;;;;;;;

; level 0 is eval, our base evaluator
(define interp0 eval)

; level 1 uses eval to interpret an interpreter text together with the
; input program
(define interp1
  (lambda (e)
    (eval `(((,interp-text (quote ,e)) (lambda (x) x)) (lambda (v) v)))))

; level 2 uses interp1 to interpret an interpreter text together with the
; input program
(define interp2
  (lambda (e)
    (interp1 `(((,interp-text (quote ,e)) (lambda (x) x)) (lambda (v) v)))))

; and so on ...
(define interp3
  (lambda (e)
    (interp2 `(((,interp-text (quote ,e)) (lambda (x) x)) (lambda (v) v)))))


; We can extract the above pattern into a general nesting facility, which
; takes a text of interpreter and a number n, and generates an interpreter
; nested to level n.
(define nest-interp
  (lambda (interp n)
    (cond
     [(zero? n) eval]
     [else
      (lambda (e)
        ((nest-interp interp (sub1 n))
         `(((,interp (quote ,e)) (lambda (x) x)) (lambda (v) v))))])))



;; ------------------------- tests -----------------------------

(define fact5
  `((,Y
     (lambda (fac)
       (lambda (n)
         (if (zero? n) 1 (* n (fac (sub1 n)))))))
    5))


(define member-test
  `(((,Y
      (lambda (member?)
        (lambda (a)
          (lambda (lat)
            (if
             (null? lat) #f
             (if (eq? a (car lat)) #t
                 ((member? a) (cdr lat))))))))
     'a) '(b a c)))


(define rho-test '(* 2 ((rho (x e k) (* 3 (k 4))) 5)))


(define prod-test-rho
  `((,Y
     (rho (prod _ __)
       (rho (ls _ k)
         (cond
          [(null? ls) 1]
          [(zero? (car ls)) (k 0)]
          [else (* (car ls) (prod (cdr ls)))]))))
    '(1 2 3 0 5 6)))


;; ------------------------- benchmark 1 --------------------------

;; interpret (fact 5) using 4 levels of interpretation
(time ((nest-interp interp-text 4) fact5))

;; interpret (fact 5) using 5 levels of interpretation
(time ((nest-interp interp-text 5) fact5))

;; ------------------------- results 1 -----------------------------
;; Ran on Macbook Pro 13" (with 2-core i5 Haswell processor)

;; Racket 5.3.6
;; Level 4: cpu time: 3924 ms, gc time: 344 ms
;; Level 5: cpu time: 427058 ms, gc time: 151906 ms

;; Petite Chez Scheme 8.4
;; Level 4: cpu time: 15049 ms gc time: 375 ms
;; Level 5: (pointless to run)

;; Chez Scheme 8.4 (optimize-level 3)
;; Ran on a different machine: 64-core AMD Opteron 6276, 512G RAM
;; Level 4: cpu time: 4098 ms, gc time 251 ms
;; Level 5: cpu time: 966614 ms, gc time: 728245 ms

;; Larceny 0.97
;; Level 4: cpu time: 3203 ms, gc time: 268 ms
;; Level 5: (over 40 minutes... didn't want to wait)

;; Larceny 0.98b1
;; Level 4: Larceny Panic: Sys/semispace.c;145: Assertion failed.


;; ------------------------ benchmark 2 --------------------------
;; Similar as before, but run member-test instead of fact-5
;; (time ((nest-interp interp-text 4) member-test))
;; (time ((nest-interp interp-text 5) member-test))

;; Racket 5.3.6
;; Level 4: cpu time: 1976 ms,  gc time: 117 ms
;; Level 5: cpu time: 192348 ms, gc time: 45944 ms

;; Chez Scheme 8.4
;; Level 4: cpu time: 1644 ms, gc time: 114 ms
;; Level 5: cpu time: 219323 ms, gc time: 101489 ms


;; ------------------------ benchmark 3 --------------------------
;; (time ((nest-interp interp-text 4) rho-test))
;; (time ((nest-interp interp-text 5) rho-test))

;; Racket 5.3.6
;; Level 4: cpu time: 412 ms, gc time: 123 ms
;; Level 5: cpu time: 26052 ms, gc time: 5447 ms

;; Chez Scheme 8.4
;; Level 4: cpu time: 483 ms, gc time: 250 ms
;; Level 5: cpu time: 19897 ms, gc time: 2999 ms


;; ------------------------ benchmark 4 --------------------------
;; (time ((nest-interp interp-text 4) prod-test-rho))

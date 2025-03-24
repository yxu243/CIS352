  #lang racket

;; Exercise 3: A CE (Control and Environment) interpreter for Scheme

;; NOTE: You *MAY* collaborate with other students on this exercise,
;; but *not* the later project. Feel free to work with up to two other
;; students (groups of three) and submit the same code as them. Use
;; that code to begin your solution for p3.

;; Name: ______yuhong xu__________________________
;; Collaborator 1: ________________________________
;; Collaborator 2: ________________________________
(provide interp-ce)

; Interp-ce must correctly interpret any valid scheme-ir program and yield the same value
; as DrRacket, except for closures which must be represented as `(closure ,lambda ,environment).
; (+ 1 2) can return 3 and (cons 1 (cons 2 '())) can yield '(1 2). For programs that result in a 
; runtime error, you should return `(error ,message)---giving some reasonable string error message.
; Handling errors and some trickier cases will give bonus points. 
(define (interp-ce exp)
  (define (interp env exp)
    (pretty-print exp)
    (pretty-print env)
    (match exp
      [(or (? number?) (? boolean?)) exp]
      [`(let ([,x ,rhs]) ,body)
       (interp env `((lambda (,x) ,body) ,rhs))]
      [`(lambda (,x) ,body)
       `(closure (lambda (,x) ,body) ,env)]
      [(? symbol? x)
       ;; look it up
       (hash-ref env x)]
      [`(if ,ge ,te ,fe)
       (if (interp env ge)
           ;; guard was true, evaluate the true branch
           (interp env te)
           (interp env fe))]
      ;; not required, but you may want to add this
      #;[`(apply-prim ,op ,x)
       'todo]
      [`(,op ,e0 ,e1)
       (define (op->rkt op)
         (match op ['+ +] ['- -] ['* *]))
       (let ([v0 (interp env e0)]
             [v1 (interp env e1)]
             [op+ (op->rkt op)])
         (op+ v0 v1))]
      [`(,ef ,ea)
       ;; 
       (define clo-for-ef (interp env ef))
       (match clo-for-ef
         [`(closure (lambda (,x) ,e-b) ,env+)
          (let ([v (interp env ea)])
            (interp (hash-set env+ x v) e-b))])]))
        
         
  ;; TODO: update?
  (define starting-env (hash))
  (interp starting-env exp))
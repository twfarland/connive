#lang racket

; shorter core syntax for racket
; http://github.com/twfarland/connive

(provide macro := :== && ~ <- ?? ++)

; simple macros
(define-syntax-rule (macro-aux id ((param ...) expr) ...)
  (define-syntax id
    (syntax-rules ()
      ((id param ...) expr)
      ...)))

(define-syntax macro-acc
  (syntax-rules ()
    ((macro-acc id (pairs ...) p b) (macro-aux id pairs ... (p b)))
    ((macro-acc id (pairs ...) p b rest ...) (macro-acc id (pairs ... (p b)) rest ...))))

(define-syntax macro
  (syntax-rules ()
    ((macro id tpl-expr ...) (macro-acc id () tpl-expr ...))))

; short definitions
(macro :=
       (id body ...) (define id body ...))

; pattern-matching function definitions
(macro :==-acc
       (id ps (pair ...) patt expr) (:= id (λ ps (match* ps (patt expr) pair ...)))
       (id ps (pair ...) patt expr rest ...) (:==-acc id ps (pair ... (patt expr)) rest ...))

(macro :== 
       (id ps patt-expr ...) (:==-acc id ps () patt-expr ...))

; all-purpose conditional
(macro ??-acc
       ((pair ...) pred res edge) (cond pair ... (pred res) (else edge))
       ((pair ...) pred res rest ...) (??-acc (pair ... (pred res)) rest ...))

(macro ?? 
       (expr ...) (??-acc () expr ...))

; general shorthand
(macro && 
       (x y) (and x y))
                          
(:= ~ curry)
(:= <- compose)
(:= ++ append)


    

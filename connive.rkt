#lang racket

; shorter core syntax for racket
; http://github.com/twfarland/connive

(provide macro := :=! :== && ~ <- ?? ++ lyt <~ .. zip-with any?)

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

(macro :=!
       (id body ...) (set! id body ...))

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

; shorter let ('lyt' is 'let' in a kiwi acccent)
(macro lyt-acc
       ((body ...) (ps ...) k v) (let (ps ... (k v)) body ...)
       ((body ...) (ps ...) k v rest ...) (lyt-acc (body ...) (ps ... (k v)) rest ...))

(macro lyt
       ((ps ...) body ...) (lyt-acc (body ...) () ps ...))

(macro lyt-acc*
       ((body ...) (ps ...) k v) (let* (ps ... (k v)) body ...)
       ((body ...) (ps ...) k v rest ...) (lyt-acc* (body ...) (ps ... (k v)) rest ...))

(macro lyt*
       ((ps ...) body ...) (lyt-acc* (body ...) () ps ...))

; basic range
(:= (.. from to)
    (?? (= from to) (list to) (cons from (.. (+ from 1) to))))

; list comprehension
(macro <-
       (res)               res 
       (res k ls)          (map (λ (k) (<- res)) ls)
       (res k ls guard)    (foldl (λ (k acc) (if guard (cons res acc) acc)) empty ls)
       (res k ls rest ...) (apply ++ (map (λ (k) (<- res rest ...)) ls)))
       
; list utils
(:== any? (f? ls)
    (_ empty) #f
    (_ (cons x xs)) (or (f? x) (any? f? xs)))
    
(:== zip-with (f xs ys)
    (_ empty empty) empty
    (_ (cons x xs) (cons y ys)) (cons (f x y) (zip-with f xs ys)))

; general shorthand
(macro && (x y) (and x y))                          
(:= ~ curry)
(:= <~ compose)
(:= ++ append)

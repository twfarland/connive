#Connive

Convenience syntax for Racket.

No license. Do what you want with this.
Creator: [Tim Farland](http://timfarland.com)

### Simple macros

    (macro id
           (param1 param2) (replacement param1 param2)
           (param ...)     (replacement param ...))

### Terse definition

    (:= x 1)    

    (:= (double x) (* x 2))

### All-purpose, bracketless conditional

    (?? #f 1
        #f 2
           3)

### Shorter let

    (lyt (x 2 y 3) (* x y))

### Short curry, compose, and, append

    (:= x2 (~ * 2))

    (:= fg (<- f g))

    (&& #t #t)

    (++ (list 1 2 3) (list 4 5 6))

### Pattern-matching function definition

    (:== map (f ls)
         (_ null) null
         (f (cons x xs)) (cons (f x) (map f xs)))
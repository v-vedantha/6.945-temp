(load "load.scm")

(define (increment-by-1 x) (+ 1 x))
(define (decrement-by-1 x) (- x 1))

(define (remove-argument-i i f)
    (assert (exact-nonnegative-integer? i))
    (define (remove-args . args)
        (values (list-remove args i)))
    (restrict-arity remove-args (alter-arity (get-arity f) increment-by-1))
)

(define (get-applier f)
    (lambda (args) (apply f args)
    ))


(define (discard-argument i)
    (lambda (f) 
        (compose (get-applier f) (remove-argument-i i f)
                )))

(((discard-argument 2) (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd)

(define (insert-argument-i i args f)
    (assert (exact-nonnegative-integer? i))
    (define (insert-args x) (list-insert args i x))
    (restrict-arity insert-args (alter-arity (get-arity f) decrement-by-1))
)

(define ((curry-argument i) f)
    (lambda args
        (compose (get-applier f) (insert-argument-i i args f)))
)

((((curry-argument 2) (lambda (x y z w) (list 'foo x y z w)) ) 'a 'b 'c) 'd)

(define (permute-arguments . permspec)
    (let ((permute (make-permutation permspec)))
        (define apply-permutation (lambda args (permute args)))
        (lambda (f)
            (compose (get-applier f) (restrict-arity apply-permutation (get-arity f)))
        )
    )
)

(((permute-arguments 1 2 0 3) (lambda (x y z w) (list 'foo x y z w))) 'a 'b 'c 'd)

; Only works if arguments are in sorted order.
(define (discard-arguments . arguments)
    (assert (or (= (length arguments) 1 ) (< (car arguments) (cadr arguments))))
    (if (= (length arguments) 1)
        (discard-argument (car arguments))
        (compose (discard-argument (car arguments)) (discard-arguments (cadr arguments)))
    )
)

(((discard-arguments 2 ) (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd)

(((discard-arguments 1 2) (lambda (y z) (list 'foo y z))) 'a 'b 'c 'd)
;(((discard-arguments 2 1) (lambda (y z) (list 'foo y z))) 'a 'b 'c 'd)

; I could not figure out curry arguments with compose
; This works for increasing lists of arguments.
; Doesn't work with no arguments though
(define ((curry-arguments . indices) f)
  (lambda args
    (define (the-combination . curries)
    (assert (or (= (length indices) 1 ) (< (car indices) (cadr indices))))
        (if (= (length curries) 1)
            (apply f (list-insert args (car indices) (car curries)))
            ((apply ((curry-arguments (cadr indices)) f) 
                                (list-insert args (car indices) (car curries))) (cadr curries))
        )
    )
    (restrict-arity the-combination (length indices))
  )
)
    

((((curry-argument 2) (lambda (x y z w) (list 'foo x y z w)) ) 'a 'b 'c) 'd)
((((curry-arguments 1 2) (lambda (x y z w) (list 'foo x y z w)) ) 'a 'b ) 'd 'c)
; ((((curry-arguments 2 1) (lambda (x y z w) (list 'foo x y z w)) ) 'a 'b ) 'd 'c) -> Fails

; My new combination will be called replicate arguments
; It takes an argument then replicates it to call a function

(define ((replicate arg) f)
    (define (the-replicator n arg)
        (if (= n 0)
            (list )
            (cons arg (the-replicator (- n 1) arg))
        )
    )
    (let ((n (procedure-arity-min (get-arity f))))
        (pp n)
        (apply f (the-replicator n arg))
    )
)

((replicate 2) (lambda (x y) (+ x y))) ; 4

(define (compose-multiple . args)
    (if (= 0 (length args))
        (lambda x x)
        (compose (car args) (cadr args))
    )
)

((compose-multiple (lambda (x) (list 1 x)) (lambda (y) (list 2 y))) 1)
((compose-multiple) 10)

; Test of the get-arity extension (In function-combinators.scm)
((compose cos +) 3 4)





(load "~/6.945/sdf/manager/load" )
(manage 'new 'combining-arithmetics)
(define (boolean-plus . args)
    (or (car args) (cadr args)))


(define (boolean-minus . args)
    (if (eq? 1 (length args)) (not (car args)) (error "cannot subtract multiple booleans")))
(define (boolean-times . args)
    (and (car args) (cadr args)))


(define (boolean-not . args)
    (not (car args)))

(define boolean-arithmetic
    (make-arithmetic 'boolean boolean? '()
        (lambda (name)
            (case name
                ((additive-identity) #f)
                ((multiplicative-identity) #t)
                (else (default-object))))
        (lambda (operator)
            (let ((procedure
                (case operator
                    ((+) 
                        boolean-plus)
                    ((-) boolean-minus)
                    ((*) boolean-times)
                    ((negate) boolean-not)
                    (else
                        (lambda args
                            (error "Operator undefined in Boolean"
                                operator))))))
            (simple-operation operator boolean? procedure)))))

(install-arithmetic! boolean-arithmetic)
; Some examples
(+ #t #t)
(* #t #f)
(- #t)

; RESTART mit-scheme or run manage before this code
(manage 'new 'combining-arithmetics)

(register-predicate! vector? 'vector) ; I have no clue what this does, but a bunch of other files did this and it doesn't work without

(define (ensure-vector-lengths-match vecs)
  (let ((first-vec-length (vector-length (car vecs))))
    (if (any (lambda (v)
               (not (n:= (vector-length v)
                         first-vec-length)))
             vecs)
        (error "Vector dimension mismatch:" vecs))))

(define (vector-element-wise element-procedure)
  (lambda vecs    ; Note: this takes multiple vectors
    (ensure-vector-lengths-match vecs)
    (apply vector-map element-procedure vecs)))

(define (numeric-= . args)
    (apply (operation-procedure (arithmetic-operation '= numeric-arithmetic)) args)
)
(define (numeric-+ . args)
    (apply (operation-procedure (arithmetic-operation '+ numeric-arithmetic)) args)
)

(define (make-sum-vector +)
    (lambda (v)
        (let ((len (vector-length v)))
          (let loop ((i 0) (current-sum (+))) ; Should be const of addition for the component
                (if (numeric-= i len) ; Numeric equals works in this case. I guess we should have a separate way to do this
                    current-sum
                    (loop (numeric-+ i 1) (+ current-sum (vector-ref v i)))) ; Also need a better value for + here
           )
        )
    )
)

; Strictly speaking the way I've written this, the makers are not needed
; But if we wanted to support just base operands we'd need these, I'm keeping them because they are small
(define (make-dot-product + *)
    (let ((vector-sum (make-sum-vector +)))
        (lambda (v1 v2)
            (vector-sum (vector-map * v1 v2))    
        )
    )
)

(define (make-magnitude * + sqrt)
    (lambda (v)
        (sqrt ((make-dot-product + *) v v))
    )
)

(define (make-scalar-product-left *)
    (lambda (b v)
        (vector-map (lambda (x) (* b x)) v)
    )
)

(define (make-scalar-product-right *)
    (lambda (v b)
        (vector-map (lambda (x) (* b x)) v)
    )
)



(define (vector-extender base-arithmetic)
    (let ((magnitude (make-magnitude * + sqrt)) ; These ops should use the + - * that we make earlier if we want to use the base arithmetic (see comment below)
            (left-scalar-product (make-scalar-product-left *))
            (right-scalar-product (make-scalar-product-right *))
            (dot-product (make-dot-product + *)))
        (make-arithmetic 'vector
            (conjoin (arithmetic-domain-predicate base-arithmetic) vector?) ; As long as one of the args is a vector we are good to go; normnally numeric arithmetic will capture things without a vector
            ; The issue is that this will capture things which dont have vectors, only bases, but technically this shouldn't happen
            (list base-arithmetic) ; The old arithmetic
            (lambda (name base-constant) base-constant)  ; How do you make a 0 vector of a specific length. Does not make sense so this means no default args
            (lambda (operator base-operation)
                (case operator
                    ((+)
                        (make-operation operator
                            (all-args (operator-arity operator) vector?)
                            (vector-element-wise +)))
                    ((-)
                        (make-operation operator
                            (all-args (operator-arity operator) vector?)
                            (vector-element-wise -)))
                    ((negate)
                        (make-operation operator
                            (all-args (operator-arity operator) vector?)
                            (vector-element-wise negate)))
                    ((magnitude)
                        (make-operation operator
                            (all-args (operator-arity operator) vector?)
                            magnitude))
                    ((*)
                        (operation-union operator
                            (make-operation operator
                                (all-args (operator-arity operator) vector?)
                                dot-product)
                            (make-operation operator
                                (match-args  (arithmetic-domain-predicate base-arithmetic) vector?)
                                left-scalar-product)
                            (make-operation operator
                                (match-args vector? (arithmetic-domain-predicate base-arithmetic))
                                right-scalar-product)))
                    (else
                        (make-operation operator
                            (any-arg (operator-arity operator) ; If anything is a vector just do this
                                    vector? vector?)
                            (lambda args (error "Cannot deal with this vector op" )))))))))

(install-arithmetic!  (extend-arithmetic vector-extender numeric-arithmetic))

(+ #(1 2 3) #(4 5 6))
(+ #(1 2 3))
(* #(1 2 3) #(4 5 6))
(magnitude #(1 1))
(negate #(1 1))
(* 2 #(1 2))
(* #(1 2) 2)

; Note about base arithmetics for 3.2a
; This works with the installed arithmetic, essentially totally ignoring the base arithmetic;
; If you wanted to use the base arithmetic, you would redefine each of the operations +  as (operation-procedure (arithmetic-domain-predicate '+)) and so on
; I've written my code with the makers the pset describes, so not much changes in the massive case statement since they will adapt to the new + and * and -
; In addition, this will improve the code in the following ways:
; Vector sum can now just get the base-const of + for its initial value, which means that vector const of + and * need not be defined anymore which is clearer
; Also you would need to use my numeric-= method in sum (which I already do, but now its necessary) since if the base arithmetic is symbol, you don't want to symbollically increment i, you actually want to do it
; Other than that the code remains mostly the same
; Since vector has no 


; 3.3 
; If you do vectors over functions then the second should work because you have a vector of functions
; and you are playing with them. The first one will fail because magnitude of a function is not defined.

; However if you have functions over vector, then magnitude of a function makes sense because you can compute the function
; which gives you a vector, then magnitude over the result. But the second one will fail since you cant magnitude over a vector of functions

; To fix this I think the trick is to extend functions over vectors. And then vectors over functions of vectors.
; But since you already have vectors extended over numerics, then to do the dispatch you will need to look at the elements within the vector
; to decide which vector arithmetic to dispatch, which I don't think we currently do.


; 3.4
; From my understanding, to deal with functions that return functions, you would need to have a system for extending functions over themselves
; Call the this the function-extender-extender. Then the dispatch store would need to execute this, whenever it sees a function that returns a function
; rather than the normal function extender. This once again doesn't really make sense since you can't tell what a function returns.

; I think the generic system can handle this since vectors can be extended over functions and functions are also extended over vectors since the system is closed.

; For the case in the example above, you had a magnitude on a vector of functions, which means that when applied it would return a function, which could be applied to 3.
; But here, you have the vector, being straight up applied to the 3, and we don't really have a concept for that.
; The fix for this would be that you need to modify (vector) so that if it takes functions as arguments, then it returns 
; a function that takes a single argument and fans it outo each element. 

; This means that the function extender needs to deal with vector as a possible operation, and that this should override how the default vector
; deals with functions as arguments.
; But since vector isn't an arithmetic (i'm guessing) this is pretty hard to implement. I'm really not sure how to start but the meat would be
#| case (op)
    ((vector) )
                                (make-operation operator
                                    (all-args (operator-arity op) function?)
                                    (lambda args (fan-out-args ... something. I'm a little lost here on the syntax))))

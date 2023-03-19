
(load "~/6.945/sdf/manager/load" )
(manage 'new 'generic-procedures)

;3.14
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
; But since vector isn't an arithmetic (i'm guessing) this is pretty hard to implement.; Vector arith taken from vector-arith.scm

(register-predicate! vector? 'vector)

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

(define ((vector-sum-maker +) v)
  (let ((n (vector-length v)))
    (let lp ((i 1) (sum (vector-ref v 0)))
      (if (n:= i n)
          sum
          (lp (n:+ i 1)
              (+ sum (vector-ref v i)))))))

(define (dot-product-maker + *)
  (let ((vector-sum (vector-sum-maker +)))
    (define (dot-product v1 v2)
      (vector-sum
       (vector-map * v1 v2)))
  dot-product))

(define (left-scalar-product-maker *)
  (define (scalar-product c v)
    (vector-map (lambda (x) (* c x))
                v))
  scalar-product)

(define (right-scalar-product-maker *)
  (define (scalar-product v c)
    (vector-map (lambda (x) (* x c))
                v))
  scalar-product)

(define (vector-magnitude-maker + * sqrt)
  (let ((dot-product (dot-product-maker + *)))
    (define (vector-magnitude v)
      (sqrt (dot-product v v)))
    vector-magnitude))

(define (vector-extender component-arithmetic)
  (let ((component-predicate
         (arithmetic-domain-predicate component-arithmetic))
        (component-proc
         (lambda (operator)
           (operation-procedure
            (arithmetic-operation operator component-arithmetic)))))
    (let ((+ (component-proc '+))
          (- (component-proc '-))
          (* (component-proc '*))
          (negate (component-proc 'negate))
          (sqrt (component-proc 'sqrt)))
      (let ((dot-product (dot-product-maker + *))
            (left-scalar-product (left-scalar-product-maker *))
            (right-scalar-product (right-scalar-product-maker *))
            (magnitude (vector-magnitude-maker + * sqrt)))
        (make-arithmetic 'vector
          (conjoin component-predicate vector?)
          (list component-arithmetic)
          (lambda (name component-constant)
            (default-object))
          (lambda (operator component-operation)
            (case operator
              ((+)
               (make-operation operator
                               (all-args (operator-arity operator)
                                         vector?)
                               (vector-element-wise +)))
              ((-)
               (make-operation operator
                               (all-args (operator-arity operator)
                                         vector?)
                               (vector-element-wise -)))
              ((*)
               (operation-union
                operator
                (make-operation operator
                                (all-args (operator-arity operator)
                                          vector?)
                                dot-product)
                (make-operation operator
                                (match-args component-predicate vector?)
                                left-scalar-product)
                (make-operation operator
                                (match-args vector? component-predicate)
                                right-scalar-product)))
              ((negate)
               (make-operation operator
                               (all-args (operator-arity operator)
                                         vector?)
                               (vector-element-wise negate)))
              ((magnitude)
               (make-operation operator
                               (all-args (operator-arity operator)
                                         vector?)
                               magnitude))
              (else
               (make-operation operator
                               (any-arg (operator-arity operator)
                                        vector?
                                        component-predicate)
                               (lambda args
                                 (error "Don't know how to "
                                        operator args)))))))))))

; My code

(define (matrix? input)
    (if (vector? input)
        (let ((n (vector-length input)))
            (let loop ((i 0) (is-still-matrix #t) (current-m 0))
                (if (n:= i n)
                    is-still-matrix
                    (if  (vector? (vector-ref input i))
                        (if (n:= i 0)
                            (loop (n:+ i 1) #t (vector-length (vector-ref input i)))
                            (if (= current-m (vector-length (vector-ref input i)))
                                (loop (n:+ i 1) #t current-m)
                                #f))
                        #f))))
        #f))

(define (get-matrix-dims input)
    (let ((n (vector-length input)))
        (if (n:= 0 n)
            (list 0 0)
            (list n (vector-length (vector-ref input 0))))))

(define (get-generic-dims input)
    (if (matrix? input)
        (get-matrix-dims input)
        (if (vector? input)
            (list (vector-length input) 1)
            (list 1 1)
        )
    ))

(define (ensure-matrix-dims-match matrices)
    (let ((first-matrix-dims (get-matrix-dims (car matrices))))
        (if (any (lambda (m)
               (not (equal? (get-matrix-dims m)
                         first-matrix-dims)))
             matrices)
        (error "Matrix dimension mismatch:" matrices))))

(define (ensure-matrix-dims-can-multiply matrices)
    (if (= (length matrices) 2)
        (let ((first-matrix-dims (get-matrix-dims (car matrices)))
              (second-matrix-dims (get-matrix-dims(cadr matrices))))
            (= (cadr first-matrix-dims) (car second-matrix-dims))      
        )
        #f))
;(ensure-matrix-dims-match (list #(#(1 1) #(1 1)) #(#(1 1 1) #(1 1 1))))


(define (matrix-element-wise element-procedure)
  (lambda matrices    ; Note: this takes multiple vectors
    (ensure-matrix-dims-match matrices)
    (apply vector-map element-procedure matrices)))


(define (get-column matrix col-index)
    (let ((n (vector-length matrix)))
        (if (n:= n 0)
            (list )
            (let ((current-col (make-vector n)))
                (let loop ((i 0) )
                    (if (n:= i n)
                        current-col
                        (begin (vector-set! current-col i (vector-ref (vector-ref matrix i) col-index))
                                (loop (n:+ i 1)))))))))


(define (min a b) (if (< a b) a b))
(define (max a b) (if (> a b) a b))

(define (make-matrix n m)
    (if (n:= n 1)
        (make-vector m)
        (if (n:= m 1)
            (make-vector n)

            ; The actual case
            (let ((out (make-vector n)))
                (let loop ((i 0) )
                    (if (n:= i n)
                        out
                        (begin (vector-set! out i (make-vector m))
                                (loop (n:+ i 1)))))))))

(define (set-element! matrix element x y)
    (if (not (matrix? matrix))
        (vector-set! matrix (max x y) element)
        (let ((row (vector-ref matrix x)))
            (begin (vector-set! row y element)
                    (vector-set! matrix x row)
            )
        )
    )
)
(define test (make-matrix 2 3))
(set-element! test 7 1 1)

(define (get-nth-row matrix n)
    (if (matrix? matrix)
        (vector-ref matrix n)
        (if (vector? matrix)
            (vector-ref matrix n)
            matrix)
    )
)
(define (get-nth-col matrix n)
    (if (matrix? matrix)
        (get-column matrix n)
        (if (vector? matrix)
            matrix ; This might be wrong
            matrix)
    )
)

(define (get-out-dims A B)
        (let ((matrix-dims-A (get-generic-dims A)) (matrix-dims-B (get-generic-dims B)))
            (let ((nA (car matrix-dims-A))
                    (mA (cadr matrix-dims-A))
                    (nB (car matrix-dims-B))
                    (mB (cadr matrix-dims-B)))
                (if (and (matrix? A) (matrix? B))
                    (if (n:= nB mA)
                        (list nA mB)
                        (error "Shapes don't multiply")
                    )
                    (if (and (matrix? A) (vector? B))
                        (if (n:= nB mA)
                            (list nA mB)
                            (error "Shapes don't multiply"))
                        (if (matrix? A)
                            (list nA nB)
                            (if (and (matrix? B) (vector? A))
                                (if (n:= nA mA)
                                    (list 1 nB)
                                    (error "Shapes don't multiply"))
                                (list nA nB)
                            )
                        )
                    )
                )
            )
        )
)


(define (matrix-multiply A B)
    (let ((out-dims (get-out-dims A B)))
        (let ((nA (car out-dims)) (mB (cadr out-dims)))
            (let ((out-matrix (make-matrix nA mB)))
                (begin 
                    (let outer-loop ((x 0))
                        (if (not (n:= x nA))
                            (begin (let ((A-vector (get-nth-row A x)))
                                    (let inner-loop ((y 0))
                                        (if (not (n:= y mB))
                                            (let ((B-vector (get-nth-col B y)))
                                                (begin (set-element! out-matrix (* A-vector B-vector) x y)
                                                    (inner-loop (n:+ y 1))
                                                )
                                            )
                                        )
                                    )
                                )
                                (outer-loop (n:+ x 1))
                            )
                        )
                    )                        
                    out-matrix
                )
            )                        
        )
    )
)




(register-predicate! matrix? 'matrix)

(define (matrix-extender component-arithmetic)
  (let ((component-predicate
         (arithmetic-domain-predicate component-arithmetic))
        (component-proc
         (lambda (operator)
           (operation-procedure
            (arithmetic-operation operator component-arithmetic)))))
    (let ((+ (component-proc '+))
          (- (component-proc '-))
          (* (component-proc '*))
          (negate (component-proc 'negate)))
        (make-arithmetic 'matrix
          matrix?
          (list component-arithmetic)
          (lambda (name component-constant)
            (default-object))
          (lambda (operator component-operation)
            (case operator
              ((+)
               (make-operation operator
                               (all-args (operator-arity operator)
                                         matrix?)
                               (matrix-element-wise +)))
              ((-)
               (make-operation operator
                               (all-args (operator-arity operator)
                                         matrix?)
                               (matrix-element-wise -)))
              ((*)
                (make-operation operator
                                (any-arg (operator-arity operator)
                                          matrix? matrix?)
                                matrix-multiply))
              ((negate)
               (make-operation operator
                               (all-args (operator-arity operator)
                                         matrix?)
                               (matrix-element-wise negate)))
              (else
               (make-operation operator
                               (any-arg (operator-arity operator)
                                        matrix?
                                        component-predicate)
                               (lambda args
                                 (error "Don't know how to "
                                        operator args))))))))))


(define g (make-generic-arithmetic make-default-dispatch-store))
      (add-to-generic-arithmetic! g numeric-arithmetic)
      (extend-generic-arithmetic! g vector-extender)
      (extend-generic-arithmetic! g matrix-extender)
(add-to-generic-arithmetic! g
(symbolic-extender numeric-arithmetic))

(install-arithmetic! g)
(define a #(#(1 2) #(2 1)))
(define b #(#(1 2 2) #(2 1 2)))
(define c #(1 2 3))
(define d 1)
(negate a)
(* a d)
(* a b)
(* a c)
(+ a a)
(- a a)
(define a #(#('a '2) #('2 '1)))
(define b #(#('1 '2 '2) #('2 '1 '2)))
(define c 'c)
(* a b)
(* a c)
; (* b b) -> Errors out because shapes don't multiply

; Other tests for my mini functions
(matrix? #(#(1 1) #(1 1)))
(matrix? #(#(1 1 1) #(1 1)))
(get-matrix-dims #(#(1 1 1) #(1 1 1)))

(ensure-matrix-dims-match (list #(#(1 1 1) #(1 1 1)) #(#(1 1 1) #(1 1 1))))
(ensure-matrix-dims-can-multiply (list #(#(1 1 1) #(1 1 1)) #(#(1 1 1) #(1 1 1) #(1 1 1))))
((matrix-element-wise (vector-element-wise +)) #(#(1 1) #(1 1)) #(#(1 1) #(1 1)))
((matrix-element-wise (vector-element-wise -)) #(#(1 1) #(1 1)) #(#(1 1) #(1 1)))
((matrix-element-wise (vector-element-wise negate)) #(#(1 1) #(1 1)))
(get-column #(#(1 2) #(1 1)) 1)
(matrix-multiply (list a b))
(matrix-multiply (list b c))
(matrix-multiply a c)


; 3.6.3 I imagine this is because we don't have a way for doing in place operations, we just create 
; A straight up new matrix for every matrix multiply even if the output shape remains the same.
; I imagine this means for applying gauss jordan elimination where we take a matrix and subtract two rows from each other
; There are what seem to be ~n total matrix multiplies that need to happen, which if we store all the intermediate resutls
; is n*n * n total intermediates, which is not quite factorial. I'm honestly not sure where they got the factorial from
;


(define full-generic-arithmetic
(let ((g (make-generic-arithmetic make-simple-dispatch-store)))
(add-to-generic-arithmetic! g numeric-arithmetic)
(extend-generic-arithmetic! g function-extender)
(add-to-generic-arithmetic! g
(symbolic-extender numeric-arithmetic))
g))

(define trie-full-generic-arithmetic
(let ((g (make-generic-arithmetic make-trie-dispatch-store)))
(add-to-generic-arithmetic! g numeric-arithmetic)
(extend-generic-arithmetic! g function-extender)
(add-to-generic-arithmetic! g
(symbolic-extender numeric-arithmetic))
g))

(define (make-cached-trie-dispatch-store)
(cache-wrapped-dispatch-store (make-trie-dispatch-store)
implementation-type-name))
(define cached-trie-full-generic-arithmetic
(let ((g (make-generic-arithmetic make-cached-trie-dispatch-store)))
(add-to-generic-arithmetic! g numeric-arithmetic)
(extend-generic-arithmetic! g function-extender)
(add-to-generic-arithmetic! g
(symbolic-extender numeric-arithmetic))
g))
(define (test-stormer-counts)
    (define (F t x) (- x))
    (define numeric-s0
        (make-initial-history 0 .01 (sin 0) (sin -.01) (sin
        -.02)))
    (with-predicate-counts
        (lambda ()
        (x 0 ((evolver F 'h stormer-2) numeric-s0 1)))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

; Try this with each trie and non trie
 (with-timings
           (lambda () (fib 20))
           (lambda (run-time gc-time real-time)
             (write (internal-time/ticks->seconds run-time))
             (write-char #\space)
             (write (internal-time/ticks->seconds gc-time))
             (write-char #\space)
             (write (internal-time/ticks->seconds real-time))
             (newline)))


; Running this with the trie is 30% slower than a normal dispatch. 
; One possibly explanation is that when the normal dispatch store is looking for the operations required in
; fib, mostly plus, -, and <, these operations might have their integer argument handles stored at the beginning of the list,
; So it could take as little as 3 lookups to dispatch, while the trie would need to look at many more possible arguments, maybe checking
; symbolic, vector, then numeric. And even for the second level, you might have to check vector? again. 
; Tries generally will be faster, unless you have this special case where the correct handlers are checked first

; With caching tries are now 2x faster than the normal approach. This might be because the correct handles weren't exactly
; at the beginnin in the normal case, but with the cache they are literally the only instructions used, so the end up
; getting checked first.




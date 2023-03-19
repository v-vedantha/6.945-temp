(load "p0utils.scm")

(modulo 13 8) ; -> 5
(remainder 13 8) ; -> 5
(modulo -13 8) ; -> 3
(remainder -13 8) ; -> -5
(modulo -13 -8) ; -> -5
(remainder -13 -8) ; -> -5

; P1
; It seems modulo's return takes the sign of the second argument, while remainder takes the sign of the first. They both are the "same" result though. For modular arthmetic I think we'd rather have the module function, since the second argument will always be positive, so the result will always be non negative.

(define +mod
    (lambda (a b n) 
        (modulo (+ a b) n)
    )
)

(define -mod
    (lambda (a b n)
        (modulo (- a b ) n)
    )
)


(define *mod
    (lambda (a b n)
        (modulo (* a b ) n)
    )
)

(+mod 7 5 8) ; -> 4
(+mod 10 10 3) ; -> 2
(-mod 5 12 2) ; -> 1
(*mod 6 6 9) ; -> 0
(+mod 99 99 100) ; -> ?
(*mod 50 -3 100) ; -> ?

(define modular
    (lambda (modulus op)
        (lambda (a1 a2)
        (modulo (op a1 a2) modulus))))


((modular 17 +) 13 11) ; -> 7
((modular 17 -) 13 11) ; -> 2
((modular 17 *) 13 11) ; -> 7

; P2
; The time complexity is n, and the space complexity is O(n) since you have to do n multiplies in different frames.


(define (exptmod p)
    (let ((mod* (modular p *)))
        (define (square x)
            (mod* x x))
        (define (em base exponent)
            (if (= exponent 0)
                1
                (if (= (modulo exponent 2) 0)
                    (mod* 1 (em (square base) (quotient exponent 2)))
                    (mod* base (em (square base) (quotient exponent 2)))
                )
            )
        )
        em
    )
)

; This basically works by taking the binary representation of the power, and squaring the base
; repeatedly and multiplying th ebinary representation is 1 at that location
; THe algorithm runs in log(exponent) time and space
; I would say this works recursively.

((exptmod 10) 2 0) ; -> 1
((exptmod 10) 2 3) ; -> 8
((exptmod 10) 3 4) ; -> 1
((exptmod 100) 2 15) ; -> 68
((exptmod 100) -5 3) ; -> 75

(define (random-k-digit-number k)
    (if (= k 1)
        (random 10)
        (+ (* 10 (random-k-digit-number (- k 1))) (random 10))
    )
)

(random-k-digit-number 1) ; ->  4 (1 digit)
(random-k-digit-number 3) ; -> 260 (1-3 digits)
(random-k-digit-number 3) ; -> 59 (is it different?)
(random-k-digit-number 50) ; -> 53594797459469133942681108851704207322413136759482 (1-50 digits)

(define (count-digits n)
    (if (= n 0)
        0
        (+ 1 (count-digits (quotient n 10)))
    )
)
(count-digits 3) ; -> 1
(count-digits 2007) ; -> 4
(count-digits 123456789) ; -> 9

; The algorithm works by generating random k digit numbers where k is the number of digits in r
; IT continues till it finds a number less than k and returns it
(define (big-random r)
    (
        let ((possible-random (random-k-digit-number (count-digits r))))
        (
            if (< possible-random r)
            possible-random
            (big-random r)
        )
    )
)
(big-random 100) ; -> ?? (1-2 digit number)
(big-random 100) ; -> ?? (is it different?)
(big-random 1) ; -> 0
(big-random 1) ; -> 0 (should be always 0)
(big-random (expt 10 40)) ; -> 5302530329763853082227460557313404987770 (roughly 40-digit number)


( (exptmod 17) 2 17) ; -> 2
( (exptmod 31) 30 31) ; -> 30
( (exptmod 71) 18 71) ; -> 18

(define prime-test-iterations 20)
;The order of growth of slow-prime is order (n) for time, but for space its constant since tail call optimization makes it so only one function stack is used. This is because the function works recursively.


;If you only had to check up to square root of n, then this would mean the order of growth in time is now only square root of n. However, for composite numbers, both algorithms take the same amount of time to eliminate because both would have found the factor below the square root of n.


;If you only need to check odds it does not improve your time complexity since you still check half the numbers.

;The order of growth of my implementation of prime is much smaller. Specifically since my exponent mod takes log N time, and I calculate this a constant number of times, the implemenation only takes log n time. Since the exponent takes log n space, then my space growth is log n as well.

;My implementation for this is a recursive algorithm. It works as follows
; For n iterations (recursively): Check that fermats last theorem lets this number be prime
; If all of them pass, return true, but if one fails return false.
(define (prime? p)
    (define (test-n p n)
        (define (test-once p)
            (let ((test-rand (big-random p)))
                (if (= ((exptmod p) test-rand p) test-rand )
                    #t
                    #f
                )
            )
        )
        ( if (= n 0)
            #t
            (if (test-once p)
                (test-n p (- n 1))
                #f
            )
            
        )
    )
    ( if  ( = p 1) #f (test-n p prime-test-iterations))
)


(prime? 2) ; -> #t
(prime? 4) ; -> #f
(prime? 1) ; -> #f
(prime? 0) ; -> #f
(prime? 200) ; -> #f
(prime? 199) ; -> ?t

; This works exactly like big-random
(define (random-k-digit-prime k)
    (
        let ((possible-prime (random-k-digit-number k)))
        (
            if (prime? possible-prime)
            possible-prime
            (random-k-digit-prime k)
        )
    )
)
;  My k digit prime number procedure can fail by generating numbers that aren't prime, or by generating numbers much smaller than k, or by taking an extremely long time. The first ones is pretty bad because then the encryption technique would be broken if don't generate a prime, and if the prime is too small then its also not strong. The last case is rare, but it just means we will be slow, not insecure.
(random-k-digit-prime 1)
(random-k-digit-prime 2)
(random-k-digit-prime 10)
(count-digits (random-k-digit-prime 100)) ; Not always 100.
(count-digits (random-k-digit-prime 100))

(define (ax+by=1 a b)
    (let ((q (quotient a b)) (r (remainder a b)))
    (if (= r 1)
        (list 1 (- q))
        
        (let ((result (ax+by=1 b r)))
            (list (cadr result) ( - (car result) (* q (cadr result))))
        )
    )
    )
)

; This works exactly as rescribed in the pset

(ax+by=1 17 13) ; -> (-3 4) 17*-3 + 13*4 = 1
(ax+by=1 7 3) ; -> (1 -2) 7*1 + 3*-2 = 1
(ax+by=1 10 27) ; -> (-8 3)
(define (inversemod n)
    (lambda (e)
        (if (= (gcd n e) 1)
            (modulo (car (ax+by=1 e n)) n)
            (error "GCD of args is not 1")
        )
    )
    
)

((inversemod 11) 5) ; -> 9 5*9 = 45 = 1 (mod 11)
((inversemod 11) 9) ; -> 5
((inversemod 11) 7) ; -> 8 7*8 = 56 = 1 (mod 11)
((inversemod 12) 5) ; -> 5 5*5 = 25 = 1 (mod 12)
; ((inversemod 12) 8) ; -> error gcd(8,12)=4, so no inverse exists
((inversemod 101) 71)
(*mod 37 71 101) ; -> 1


; Advertized number (number) is P
; System-prime is p
; root as a
; This procedure works pretty logicall, it gets the information from the reciever, 
; Constructs the message, then passes it on.
(define (eg-send-message message receiver)
    (let ((public-key (eg-receiver-public-key receiver)) 
          (decriptor (eg-receiver-decryption-procedure receiver))
          (bytes-message (string->integer message)))
          (let ((system (eg-public-key-system public-key)) (number (eg-public-key-number public-key)))
                (let ((system-size (dh-system-size system)) (system-prime (dh-system-prime system)) (root (dh-system-primitive-root system)))
                    (let ((encoding-secret (random-k-digit-number system-size)) (mod-exp (exptmod system-prime)) (mod-* (modular system-prime *)))
                        (decriptor (eg-make-ciphertext (mod-exp root encoding-secret) (mod-* bytes-message (mod-exp number encoding-secret))))
                    )
                )
          )
    )
)

(define dh-system
  (public-dh-system 100))

(define Alyssa
  (eg-receiver dh-system))

(eg-send-message "Hi there." Alyssa)

(eg-send-message "Bye!!" Alyssa)

(eg-send-message "aabncdefghijklmnopqrstuvwxyjwasoifalwoisf" Alyssa)
; Adding another character makes this string impossible to decript. This could have something to do with the fact that 
; This string is just under the p-value we are using, and one more character pushes it over, so technically it is equivalent
; to encrypting (that value - p) which means nothing

(define (Eve2 receiver)
  (let ((receiver-public-key
	 (eg-receiver-public-key receiver))
	(receiver-decryption-procedure
	 (eg-receiver-decryption-procedure receiver))
     (my-dh-system (public-dh-system 100)))
     (let ((my-receiver (eg-receiver my-dh-system)))
        (let ((my-public-key (eg-receiver-public-key my-receiver)) (my-decryptor (eg-receiver-decryption-procedure my-receiver)))
            (let ((my-spying-procedure
                    (lambda (ciphertext)
                        (write (my-decryptor ciphertext))
                        (newline)
                        (eg-send-message (my-decryptor ciphertext) receiver))))

                (eg-make-receiver my-public-key
                        my-spying-procedure)
            )
        )
    ))
)

(define Alyssa (Eve2 Alyssa))

(eg-send-message "Bye!!" Alyssa)
; The trick here is that Eve acs like Alyssa to Ben and like Ben to Alyssa in that they publish to ben
; Their own public key as Alyssa, then read bens message, and then forward it to alyssa. Neither of them ever know
; That there is an eve in the middle of their conversation

;;{{{ Basic operations of stream
;;{{{ Basic construction of stream
(define-syntax stream-cons
  (syntax-rules ()
    [(_ a b)
     (cons a (delay b))]))
(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (if (procedure? (cdr stream))
    (set-cdr! stream
      (force (cdr stream))))
  (cdr stream))
(define the-empty-stream '())
(define (stream-null? stream)
  (null? stream))
;;}}}
(define (stream-ref s n)
  (if (zero? n)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc . ss)
  (if (stream-null? (car ss))
    the-empty-stream
    (stream-cons (apply proc (map stream-car ss))
                 (apply stream-map
                        (cons proc (map stream-cdr ss))))))
(define (stream-for-each proc s)
  (unless (stream-null? s)
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter pred
                        (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define (display-stream s)
  (stream-for-each
    (lambda (x) (display x) (newline)) s))
;;{{{ Arithmetic operations of stream
(define (streamfy op)
  (lambda args
    (apply stream-map
           (cons op args))))
(define stream+ (streamfy +))
(define stream- (streamfy -))
(define stream* (streamfy *))
(define stream/ (streamfy /))
(define (stream*n s n)
  (stream-map (lambda (x) (* x n)) s))
(define (stream-shift s n)
  (if (negative? n)
    (let loop ([n n] [s s])
      (if (zero? n) s
        (loop (1+ n) (stream-cdr s))))
    (let loop ([n n])
      (if (zero? n) s
        (stream-cons 0 (loop (1- n)))))))
;;}}}
;;}}}
;;{{{ Basic operations of hexes
;;{{{ Auxiliary functions and basic construction of hexes
;;;{{{ Self-referential list
(define-syntax list*&
  (lambda (x)
    (syntax-case x ()
      [(_ head) #'(list* head)]
      [(name head remain ...)
       (with-syntax ([& (datum->syntax #'name '&)])
         #'(let* ([& (list '())] [head& head]
                  [remain& (list* remain ...)])
             (set-car! & head&)
             (set-cdr! & remain&)
             &))])))
(define-syntax list&
  (lambda (x)
    (syntax-case x ()
      [(name elems ...)
       (datum->syntax #'name (syntax->datum
         #'(list*& elems ... '())))])))
(define-syntax cons&
  (lambda (x)
    (syntax-case x ()
      [(name a b)
       (datum->syntax #'name (syntax->datum
         #'(list*& a b)))])))
;;;}}}
(define ($hex-to-uppercase c)
  (case c
    [(0 1 2 3 4 5 6 7 8 9) (integer->char (+ 48 c))]
    [(10 11 12 13 14 15) (integer->char (+ 55 c))]
    [else #\?]))
(define ($hex-to-lowercase c)
  (case c
    [(0 1 2 3 4 5 6 7 8 9) (integer->char (+ 48 c))]
    [(10 11 12 13 14 15) (integer->char (+ 87 c))]
    [else #\?]))
(define ($hexes-inv c)
  (case c [1 1] [3 11] [5 13] [7 7]
          [9 9] [11 3] [13 5] [15 15] [else #f]))
(define ($stream-carry s)
  (let loop ([c 0] [h s])
    (let-values ([(c r)
        (div-and-mod (+ c (stream-car h)) 16)])
      (stream-cons r (loop c (stream-cdr h))))))
(define ($stream-truncate! s)
  (let loop ([n s] [2n s])
    (let ([n0 (cdr n)] [n1 (cdr 2n)])
      (if (procedure? n1) (set-cdr! 2n zero-hexes)
        (let ([n2 (cdr n1)]) (cond
          [(procedure? n2) (set-cdr! n1 zero-hexes)]
          [(not (eqv? n0 n2)) (loop n0 n2)]))))))
(define (display-hexes s)
  (stream-for-each
    (lambda (x)
      (display ($hex-to-lowercase x))
      (flush-output-port)) s))
;;;; Note: If we use this definition, the pre-defined `zero-hexes`
;;;;       can be modified by `$stream-truncate!`.
;(define (ns n) (letrec ([r (stream-cons n r)]) r))
(define (ns n) (cons& n &))
(define zero-hexes (ns 0))
(define one-hexes (cons 1 zero-hexes))
(define minus-one-hexes (ns 15))
(define (n-hexes n)
  (let loop ([q n])
    (case q [0 zero-hexes] [-1 minus-one-hexes] [else
      (let-values ([(q r) (div-and-mod q 16)])
        (cons r (loop q)))])))
;;}}}
(define hexes-shift stream-shift)
(define (hexes+ h . args)
  (if (null? args) h
    ($stream-carry (apply stream+ (cons h args)))))
(define hexes-
  (case-lambda
    [(h)
     (let loop ([q 1] [h h])
       (let ([a (+ q 15 (- (stream-car h)))])
         (if (> a 15)
           (stream-cons (- a 16) (loop 1 (stream-cdr h)))
           (stream-cons a (loop 0 (stream-cdr h))))))]
    [(h1 h2) (hexes+ h1 (hexes- h2))]))
(define (hexes*n h n)
  (cond [(zero? n) zero-hexes]
        [(= n 1) h] [(= n -1) (hexes- h)]
        [else ($stream-carry (stream*n h n))]))
(define (hexes/2 h) (stream-cdr (hexes*n h 8)))
(define (hexes/4 h) (stream-cdr (hexes*n h 4)))
(define (hexes/8 h) (stream-cdr (hexes*n h 2)))
(define (hexes* h1 h2)
  (cond [(zero? (stream-car h1))
         (stream-cons 0 (hexes* (stream-cdr h1) h2))]
        [(zero? (stream-car h2))
         (stream-cons 0 (hexes* h1 (stream-cdr h2)))]
        [else
    (let ([h2 (stream-cons 0 h2)])
      (let loop ([q 0] [l (list)] [h1 (stream-cons 0 h1)])
        (let* ([h1 (stream-cdr h1)] [l (cons (stream-car h1) l)] [a
            (let loop ([h2 h2] [l l] [a q])
              (if (null? l) a
                (let* ([h2 (stream-cdr h2)] [x (stream-car h2)])
                  (loop h2 (cdr l) (+ a (* x (car l)))))))])
          (let-values ([(q r) (div-and-mod a 16)])
            (stream-cons r (loop q l h1))))))]))
(define (hexes-square h)
  (let loop ([s #f] [q (expt (stream-car h) 2)] [h h] [l (list)])
    (let-values ([(q r) (div-and-mod
        (let loop ([h h] [l l] [a q])
          (if (null? l) a
            (let* ([h (stream-cdr h)] [x (stream-car h)])
              (loop h (cdr l) (+ a (* 2 x (car l))))))) 16)]
                 [(d h l) (if s
        (let ([h (stream-cdr h)])
          (values (expt (stream-car h) 2) h l))
        (values 0 h (cons (stream-car h) l)))])
      (stream-cons r (loop (not s) (+ q d) h l)))))
(define (hexes-expt h n)
  (cond [(negative? n) (hexes-expt (hexes/ h) (- n))]
        [(zero? n) one-hexes] [(= n 1) h]
        [else
         (let ([hh (hexes-square (hexes-expt h (quotient n 2)))])
           (if (odd? n) (hexes* hh h) hh))]))
(define (hexes-invertible-expt h n)
  (if (zero? n) one-hexes
    (let loop ([n n] [h-inv #f])
      (let-values ([(n h1 s)
          (cond [(even? n) (values (quotient n 2) #f #f)]
                [(= (mod n 4) 1) (values (quotient (1- n) 2) h #f)]
                [else (values (quotient (1+ n) 2)
                              (if h-inv h-inv (hexes/ h)) (not h-inv))])])
        (if (zero? n) h1
          (let ([h2 (hexes-square (loop n (if s h1 h-inv)))])
            (if h1 (hexes* h1 h2) h2)))))))
(define (hexes/n h n)
  (cond [(negative? n) (hexes/n (hexes- h) (- n))] [(= n 1) h] [else
    (let ([b ($hexes-inv (remainder n 16))])
      (if (not b) (error 'hexes/n "Number is non-invertible."))
      (let loop ([q 0] [h h])
        (let ([x (mod (* b (- (stream-car h) q)) 16)])
          (stream-cons x
            (loop (quotient (+ q (* n x)) 16) (stream-cdr h))))))]))
(define hexes/
  (case-lambda
    [(h)
     (let* ([c (stream-car h)] [b ($hexes-inv c)])
       (if (not b) (error 'hexes/ "Hexes is non-invertible."))
       (letrec ([inv (stream-cons b
           (let loop ([q (quotient (* b c) 16)] [l (list b)])
             (let ([a
                 (let loop ([a q] [h h] [l l])
                   (if (null? l) a
                     (let* ([h (stream-cdr h)] [x (stream-car h)])
                       (loop (+ a (* x (car l))) h (cdr l)))))])
               (let ([x (mod (* (remainder a 16) (- b)) 16)])
                 (stream-cons x
                   (loop (quotient (+ a (* c x)) 16) (cons x l)))))))])
         inv))]
    [(h1 h2) (hexes* h1 (hexes/ h2))]))
(define hexes-sqrt
  (case-lambda
    [(h) (hexes-sqrt h 0)]
    [(h index)
     (let ([c (stream-car h)] [h (stream-cdr h)])
       (if (not (or (= c 1) (= c 9)))
         (error 'hexes-sqrt "Only hexes of 1 or 9 is acceptable."))
       (let* ([c (bitwise-xor (if (even? index) 0 14) (if (even? (stream-car h)) 0 8)
                              (if (= c 1) 1 13))] [b ($hexes-inv c)])
         (letrec ([rt (stream-cons c
             (let loop ([s #t] [q (quotient (* c c) 16)] [rp rt] [l (list)] [h h])
               (let* ([r (if (and (null? l) s) rp (stream-cdr rp))] [a
                   (let loop ([r r] [l l] [a q])
                     (if (null? l) a
                       (let* ([r (stream-cdr r)] [x (stream-car r)])
                         (loop r (cdr l) (+ a (* 2 x (car l)))))))])
                 (let* ([y (stream-car h)] [h (stream-cdr h)]
                        [x (mod (quotient (* b (- y a)) 2) 8)]
                        [z (lambda () (if (null? l) x (stream-car (stream-cdr r))))]
                        [q (quotient (+ a (* 2 c x)) 16)])
                   (unless (boolean=? (or (not s) (even? (z)))
                                      (boolean=? (even? (stream-car h)) (even? q)))
                     (set! x (+ x 8)) (set! q (+ q c)))
                   (let-values ([(q r l) (if s
                       (values (+ q (expt (z) 2)) r l) (values q rp (cons (stream-car r) l)))])
                     (stream-cons x (loop (not s) q r l h)))))))])
           rt)))]))
(define (hexes-root h n)
  (cond [(= n 1) h] [(= (mod n 4) 3) (hexes-root (hexes/ h) (- n))]
        [(even? n) (error 'hexes-root "Only odd root index is acceptable.")]
        [else
    (let* ([c (stream-car h)] [b ($hexes-inv c)])
      (if (not b) (error 'hexes-root "Only invertible hexes is acceptable."))
      (let ([iter0 (n-hexes (if (= (remainder n 4) 1) c b))]
            [next (lambda (x)
          (hexes/n (hexes+ (hexes* h (hexes-invertible-expt x (- 1 n)))
                           (hexes*n x (1- n))) n))])
        (let oloop ([skip 0] [h1 iter0] [hh1 iter0] [h2 (next iter0)] [s #f])
          (let loop ([skip skip] [hh1 hh1] [hh2
              (let loop ([j skip] [h2 h2])
                (if (<= j 0) h2 (loop (1- j) (stream-cdr h2))))])
            (let ([x (stream-car hh2)])
              (if (= x (stream-car hh1))
                (begin (set! s #t)
                  (stream-cons x
                    (loop (1+ skip) (stream-cdr hh1) (stream-cdr hh2))))
                (begin (if s ($stream-truncate! hh1))
                  (oloop skip h2 hh2 (next h2) #f))))))))]))
;;}}}
;;{{{ Basic operations of binaries
;;{{{ Basic construction of binaries
(define zero-binaries (cons +inf.0 zero-hexes))
(define one-binaries (cons 0 one-hexes))
(define minus-one-binaries (cons 0 minus-one-hexes))
(define (r-binaries r)
  (if (zero? r) zero-binaries
    (let ([n (numerator r)] [d (denominator r)])
      (let-values ([(c n d) (if (odd? d)
          (let ([c (bitwise-first-bit-set n)])
            (values c (bitwise-arithmetic-shift-right n c) d))
          (let ([c (bitwise-first-bit-set d)])
            (values (- c) n (bitwise-arithmetic-shift-right d c))))])
        (cons c (hexes/n (n-hexes n) d))))))
(define (h-binaries h)
  (let loop ([j 0] [hh h])
    (if (and (< j eval-threshold:) (zero? (stream-car hh)))
      (loop (1+ j) (stream-cdr hh))
      (if (= j eval-threshold:) (list* 0 (box j) h)
        (let ([c (bitwise-first-bit-set (stream-car hh))])
          (cons (+ (* 4 j) c) (($shift-fun c) hh)))))))
;;}}}
;;{{{ Display functions
;;;{{{ Hook keyboard interrupt
(define-syntax with-term
  (lambda (x)
    (syntax-case x ()
      [(_ body ...)
       #'(letrec
             ([iclean (lambda () (keyboard-interrupt-handler kbdi))]
              [isetup (lambda () (keyboard-interrupt-handler kbdi@))]
              [kbdi (keyboard-interrupt-handler)]
              [kbdi@ (lambda () (iclean) (printf "\x1b;[m") (kbdi) (isetup))])
           (isetup) ((lambda (x) (iclean) x) (begin body ...)))])))
;;;}}}
(define display-threshold: 50)
(define display-step: 10)
(define ($display x)
  (display ($hex-to-uppercase x)))
(define $display-inf
  (let ([display-cached (lambda (count)
      (let-values ([(count c) (if (negative? count)
          (values (- count) ($hex-to-uppercase 15))
          (values count ($hex-to-uppercase 0)))])
        (let-values ([(q r) (div-and-mod count display-threshold:)])
          (let loop ([q q] [r r] [str c])
            (cond [(positive? r) (display str) (loop q (1- r) str)]
                  [(positive? q) (if (zero? r)
                      (loop q (1- r) (make-string display-threshold: c))
                      (begin (display str) (loop (1- q) r str)))])))))])
    (case-lambda
      [() ($display-inf 0)]
      [(x) (let ([count x] [showed #f])
       (lambda (x)
         (when (or (and (negative? count) (< x 15))
                   (and (positive? count) (> x 0)))
           (when showed (printf "\x1b;[u") (set! showed #f))
           (display-cached count) (set! count 0))
         (let ([c ($hex-to-uppercase x)])
           (if (< 0 x 15) (display c)
             (begin (set! count (+ count (case x [0 1] [15 -1])))
               (when (and (>= (abs count) display-threshold:)
                          (zero? (mod count display-step:)))
                 (when (not showed) (printf "\x1b;[s") (set! showed #t))
                 (printf "\x1b;[u\x1b;[1m~c(~d)\x1b;[m" c (abs count))))))
         (flush-output-port)))])))
(define ($display-shift f r)
  (let* ([r (remainder r 4)] [fun
      (cond [(positive? r)
             (let ([r (expt 2 r)])
               (let ([q 0])
                 (lambda (c)
                   (let-values ([(d r)
                       (div-and-mod (+ q (* c r)) 16)])
                     (set! q d) (f r)))))]
            [(negative? r)
             (let ([r (expt 2 (- r))])
               (let ([q #f])
                 (lambda (c)
                   (if q
                     (let-values ([(d r)
                         (div-and-mod (+ q (* c r)) 16)])
                       (set! q d) (f r))
                     (set! q (div (* c r) 16))))))]
            [else (lambda (x) (f x))])])
    (lambda (x) (if (procedure? x) (set! f x) (fun x)))))
(define display-binaries
  (case-lambda
    [(b) (display-binaries b #f)]
    [(b n) (with-term (if (= (car b) +inf.0) (printf "0~%") (begin
     (cond [(not n) (set! n -inf.0)]
           [(boolean? n) (set! n +inf.0)])
     (if (positive? n)
       (begin
         (if (box? (cadr b)) ($binaries-eval b #t))
         (printf "[~d]" (car b))
         (if (infinite? n)
           (stream-for-each ($display-shift ($display-inf) 0) (cdr b))
           (let ([f ($display-shift $display 0)])
             (let loop ([n n] [h (cdr b)])
               (if (positive? n) (begin
                   (f (stream-car h))
                   (loop (1- n) (stream-cdr h)))
                 (printf "\x1b;[1m...\x1b;[m~%"))))))
       (begin (set! n (- n))
         (if (box? (cadr b))
           ($binaries-eval b (ceiling (/ (- (car b)) 4))))
         (if (box? (cadr b))
           (let-values ([(q r) (div-and-mod (- (car b)) 4)])
             (display #\.)
             (let ([h (hexes-shift (cddr b) (- q))])
               (if (infinite? n)
                 (stream-for-each
                   ($display-shift ($display-inf) (- r 4)) h)
                 (let ([f ($display-shift $display (- r 4))])
                   (let loop ([n n] [h (if (positive? r)
                       (begin (f (stream-car h)) (stream-cdr h)) h)])
                     (if (positive? n) (begin
                         (f (stream-car h))
                         (loop (1- n) (stream-cdr h)))
                       (printf "\x1b;[1m...\x1b;[m~%")))))))
           (let-values ([(q r) (div-and-mod (car b) 4)])
             (let ([f ($display-shift $display r)])
               (if (negative? q)
                 (let loop ([q q] [h (cdr b)])
                   (if (negative? q)
                     (begin (f (stream-car h))
                       (loop (1+ q) (stream-cdr h)))
                     (begin (display #\.)
                       (if (infinite? n)
                         (begin (f ($display-inf))
                           (stream-for-each f h))
                         (let loop ([n n] [h h])
                           (if (positive? n) (begin
                               (f (stream-car h))
                               (loop (1- n) (stream-cdr h)))
                             (printf "\x1b;[1m...\x1b;[m~%")))))))
                 (if (infinite? n)
                   (begin (display #\.) (f ($display-inf q))
                     (stream-for-each f (cdr b)))
                   (let loop ([q q] [n n] [c #\.])
                     (if c (display c)
                       (let loop ([n q] [h (cdr b)])
                         (when (positive? n)
                           (f (stream-car h))
                           (loop (1- n) (stream-cdr h)))))
                     (if (positive? n)
                       (if (positive? q)
                         (loop (1- q) (1- n) #\0) (loop n q #f))
                       (printf "\x1b;[1m...\x1b;[m~%")))))))))))))]))
;;}}}
;;{{{ Auxiliary functions
(define eval-threshold: 50)
(define eval-step: 10)
(define $binaries-operate
  (case-lambda
    [(h f) ($binaries-operate h f #f)]
    [(h f g)
     (let-values ([(f1 f2) (if (pair? f)
         (values (car f) (cdr f)) (values f f))])
       (if (box? (car h))
         (cons (if g (box (g (unbox (car h)))) (car h))
               (f2 (cdr h))) (f1 h)))]))
(define ($shift-fun c)
  (case c [0 values] [1 hexes/2] [2 hexes/4] [3 hexes/8]))
(define ($eval-hexes-with-box bx h target)
  (let ([c (unbox bx)])
    (cond [(negative? target)
           (let ([t (- target)])
             (let loop ([j 0] [h h])
               (if (and (< j t) (zero? (stream-car h)))
                 (loop (1+ j) (stream-cdr h))
                 (begin (set-box! bx (+ c j))
                   (values (< j t) h)))))]
          [(<= 0 target c)
           (let loop ([j target] [h h])
             (if (<= j 0) (values #f h) (loop (1- j) (stream-cdr h))))]
          [else
           (let-values ([(_ h) ($eval-hexes-with-box bx h c)])
             ($eval-hexes-with-box bx h (- c target)))])))
(define ($make-binaries-with-box m bx h)
  (let ([c (bitwise-first-bit-set (stream-car h))])
    (cons (+ (* 4 (unbox bx)) c m) (($shift-fun c) h))))
(define ($binaries-eval b n)
  (if (not n)
    (if (box? (cadr b))
      (set-box! (cadr b) 0)
      (if (< (car b) +inf.0)
        (set-cdr! b (cons (box 0) (cdr b)))))
    (if (not (box? (cadr b))) #t
      (let* ([bx (cadr b)] [e (unbox bx)] [h (cddr b)])
        (cond [(call/cc (lambda (k)
            (cond [(boolean? n)
                   (let ([n (* eval-step: (1+ (quotient e eval-step:)))])
                     (let-values ([(s h) ($eval-hexes-with-box bx h n)])
                       (if s (k h) (set! s (- eval-step:)))
                       (printf "\x1b;[s")
                       (let loop ([n n] [h h])
                         (printf "\x1b;[u\x1b;[1m~d\x1b;[m" n)
                         (flush-output-port)
                         (let-values ([(s h) ($eval-hexes-with-box bx h s)])
                           (if s (begin (printf "\x1b;[u\x1b;[K\r") (k h))
                             (loop (+ n eval-step:) h))))))]
                  [(> n e)
                   (let-values ([(s h) ($eval-hexes-with-box bx h n)])
                     (if s (k h) #f))]
                  [else #f]))) => (lambda (h)
          (let ([r ($make-binaries-with-box (car b) bx h)])
            (set-car! b (car r))
            (set-cdr! b (cdr r))) #t)] [else #f])))))
;;}}}
(define (binaries-zero? b)
  (cond [(= (car b) +inf.0) #t]
        [(box? (cadr b))
         (+ (car b) (* 4 (unbox (cadr b))))]
        [else #f]))
(define binaries-eval
  (case-lambda
    [(b) (with-term ($binaries-eval b #t))]
    [(b n)
     ($binaries-eval b
       (ceiling (/ (- n (car b)) 4)))]))
(define (binaries-shift b n)
  (if (zero? n) b (cons (+ (car b) n) (cdr b))))
(define (binaries+ . args)
  (let ([sort-fun (lambda (x y) (and
      (or (pair? (car y)) (not (or (pair? (car x)) (> (car x) (car y)))))
      (not (and (pair? (car x)) (pair? (car y)) (> (caar x) (caar y))))))])
    (let loop ([args (sort! sort-fun
        (filter values (map (lambda (b)
              (and (not (= (car b) +inf.0))
                   (if (box? (cadr b))
                     (cons (cons (car b) (cadr b)) (cddr b)) b)))
            args)))])
      (cond [(null? args) zero-binaries]
            [(pair? (caar args))
             (let* ([r (car args)] [m (caar r)])
               (let loop ([args (cdr args)] [l (list (cdr r))]
                          [b (list (unbox (cdar r)))])
                 (if (null? args)
                   (list* m (box (apply min b)) (apply hexes+ l))
                   (let-values ([(q c)
                       (div-and-mod (- (caaar args) m) 4)])
                     (loop (cdr args)
                       (cons (hexes*n (hexes-shift (cdar args) q)
                               (expt 2 c)) l)
                       (cons (+ (unbox (cdaar args)) q) b))))))]
            [else
             (let ([m (caar args)])
               (let-values ([(r rest)
                   (let loop ([args (cdr args)] [l (list (cdar args))])
                     (if (or (null? args) (pair? (caar args)) (> (caar args) m))
                       (values (if (> (length l) 1)
                                 (h-binaries (apply hexes+ l)) #f) args)
                       (loop (cdr args) (cons (cdar args) l))))])
                 (if r (set-car! r (begin (set! m (+ (car r) m)) m))
                   (set! r (car args)))
                 (cond [(null? rest) r]
                       [(box? (cadr r))
                        (let ([r (cons (cons m (cadr r)) (cddr r))])
                          (if (and (pair? (caar rest)) (>= (caaar rest) m))
                            (loop (cons r rest))
                            (begin
                              (let loop ([args rest])
                                (if (or (null? (cdr args))
                                        (and (pair? (caadr args))
                                             (>= (caaadr args) m)))
                                  (set-cdr! args (cons r (cdr args)))
                                  (loop (cdr args))))
                              (loop rest))))]
                       [(and (not (pair? (caar rest))) (<= (caar rest) m))
                        (let loop ([args rest])
                          (if (or (null? (cdr args)) (pair? (caadr args))
                                  (>= (caadr args) m))
                            (set-cdr! args (cons r (cdr args)))
                            (loop (cdr args))))
                        (loop rest)] [else
                   (let ([new #f])
                     (let loop ([args rest])
                       (unless (null? args)
                         (if (pair? (caar args))
                           (let ([e (1+ (div (- m (caaar args)) 4))]
                                 [bx (cdaar args)])
                             (if (< (unbox bx) e)
                               (let-values ([(s h)
                                   ($eval-hexes-with-box bx (cdar args) e)])
                                 (when s (set! new #t)
                                   (set-car! args ($make-binaries-with-box
                                     (caaar args) bx h)))))))
                         (loop (cdr args))))
                     (if new (loop (sort! sort-fun (cons r rest)))
                       (let loop ([args rest] [l (list (cdr r))])
                         (if (null? args) (cons m (apply hexes+ l))
                           (loop (cdr args)
                             (if (pair? (caar args))
                               (let-values ([(q c)
                                   (div-and-mod (- m (caaar args)) 4)])
                                 (cons (($shift-fun c)
                                         (hexes-shift (cdar args) (- q))) l))
                               (let-values ([(q c)
                                   (div-and-mod (- (caar args) m) 4)])
                                 (cons (hexes*n (hexes-shift (cdar args) q)
                                         (expt 2 c)) l))))))))])))]))))
(define binaries-
  (case-lambda
    [(b) (cons (car b) ($binaries-operate (cdr b) hexes-))]
    [(b1 b2) (binaries+ b1 (binaries- b2))]))
(define (binaries*r b r)
  (if (zero? r) zero-binaries
    (let ([n (numerator r)] [d (denominator r)])
      (let-values ([(c n d) (if (odd? d)
          (let ([c (bitwise-first-bit-set n)])
            (values c (bitwise-arithmetic-shift-right n c) d))
          (let ([c (bitwise-first-bit-set d)])
            (values (- c) n (bitwise-arithmetic-shift-right d c))))])
        (cons (+ (car b) c) ($binaries-operate (cdr b)
            (lambda (s) (hexes/n (hexes*n s n) d))))))))
(define (binaries* b1 b2)
  (if (or (= (car b1) +inf.0) (= (car b2) +inf.0)) zero-binaries
    (let-values ([(b1 b2) (if (box? (cadr b2))
        (values b2 b1) (values b1 b2))])
      (cons (+ (car b1) (car b2))
        (let-values ([(h2 g) (if (box? (cadr b2))
            (values (cddr b2) (lambda (x) (+ x (unbox (cadr b2)))))
            (values (cdr b2) #f))])
          ($binaries-operate (cdr b1)
            (lambda (h1) (hexes* h1 h2)) g))))))
(define (binaries-square b)
  (if (= (car b) +inf.0) zero-binaries
    (cons (* 2 (car b))
      ($binaries-operate (cdr b) hexes-square
        (lambda (x) (* 2 x))))))
(define (binaries-expt b n)
  (cond [(and (<= n 0) (= (car b) +inf.0))
         (error 'binaries-expt
           "Zero to the non-positive exponent.")]
        [(and (<= n 0) (box? (cadr b)))
         (error 'binaries-expt
           "Potential zero to the non-positive exponent.")]
        [(zero? n) one-binaries] [(= n 1) b]
        [(= (car b) +inf.0) zero-binaries] [else
    (cons (* n (car b))
      ($binaries-operate (cdr b)
        (cons (lambda (h) (hexes-invertible-expt h n))
              (lambda (h) (hexes-expt h n)))
        (lambda (x) (* n x))))]))
(define binaries/
  (case-lambda
    [(b) (cond
      [(= (car b) +inf.0) (error 'binaries/ "Division by zero.")]
      [(box? (cadr b)) (error 'binaries/ "Division by potential zero.")]
      [else (cons (- (car b)) (hexes/ (cdr b)))])]
    [(b1 b2) (binaries* b1 (binaries/ b2))]))
(define binaries-sqrt?
  (case-lambda
    [(b) (binaries-sqrt? b #t)]
    [(b s) (cond
      [(= (car b) +inf.0) #t] [(box? (cadr b)) (if s
          (warning 'binaries-sqrt? "Potential zero.")) #f]
      [(odd? (car b)) #f] [else
          (case (stream-car (cdr b)) [(1 9) #t] [else #f])])]))
(define binaries-sqrt
  (case-lambda
    [(b) (binaries-sqrt b 0)]
    [(b index) (cond
      [(not (binaries-sqrt? b #f))
       (error 'binaries-sqrt "Failed.")]
      [(= (car b) +inf.0) zero-binaries]
      [else (cons (quotient (car b) 2)
              (hexes-sqrt (cdr b) index))])]))
(define binaries-root?
  (case-lambda
    [(b n) (binaries-root? b n #t)]
    [(b n s) (cond
      [(zero? n) #f] [(= (car b) +inf.0) (positive? n)]
      [(box? (cadr b)) (if s
          (warning 'binaries-root? "Potential zero.")) #f]
      [(not (zero? (remainder (car b) n))) #f] [(odd? n) #t]
      [else
       (let loop ([k (bitwise-first-bit-set n)]
                  [j 4] [r 1] [h (cdr b)])
         (if (<= k 0)
           (zero? (remainder (- (stream-car h) r) j))
           (if (>= j 8)
             (if (not (= (stream-car h) r)) #f
               (loop (1- k) 1 0 (stream-cdr h)))
             (loop (1- k) (* 2 j) r h))))])]))
(define binaries-root
  (case-lambda
    [(b n) (binaries-root b n 0)]
    [(b n index) (cond
      [(not (binaries-root? b n #f))
       (error 'binaries-root "Failed.")]
      [(= (car b) +inf.0) zero-binaries] [(= n 1) b]
      [else (cons (quotient (car b) n)
       (let* ([k (bitwise-first-bit-set n)]
              [n (bitwise-arithmetic-shift-right n k)])
         (hexes-root
           (let loop ([k k] [i index])
             (if (<= k 0) (cdr b)
               (hexes-sqrt (loop (1- k) 0) i))) n)))])]))
;;}}}
;;{{{ Elementary analytic functions of binaries
;;{{{ Auxiliary functions
(define ($hexes-power-list h)
  (let loop ([hn h])
    (stream-cons hn (loop (hexes* hn h)))))
(define ($hexes-invertible-power-list h)
  (let ([h-inv (hexes/ h)])
    (letrec ([pl (stream-cons one-hexes (stream-cons h
        (let loop ([pl pl])
          (let ([pl (stream-cdr pl)])
            (stream-cons (hexes-square (stream-car pl))
              (let* ([pl (stream-cdr pl)]
                     [p2 (hexes-square (stream-car pl))])
                (stream-cons (hexes* p2 h-inv)
                  (stream-cons p2
                    (stream-cons (hexes* p2 h)
                      (loop pl))))))))))])
      (stream-cdr pl))))
(define ($binaries-get-first-digits b n)
  (cond [(box? (cadr b))
         (let ([e (+ n (ceiling (/ (- (car b)) 4)))] [bx (cadr b)])
           (let-values ([(s h) (if (> e (unbox bx))
                                 ($eval-hexes-with-box bx (cddr b) e)
                                 (values #f (cddr b)))])
             (if s ($binaries-get-first-digits
                 ($make-binaries-with-box (car b) bx h) n)
               (values zero-hexes 0
                 (list* (+ (car b) (* 4 (- (unbox bx) n))) (box 0)
                   (hexes-shift (cddr b) (- (unbox bx))))))))]
        [(negative? (car b)) (values #f #f #f)]
        [else
         (let-values ([(q r) (div-and-mod (car b) 4)])
           (let* ([h (hexes-shift (cdr b) q)] [2^r (expt 2 r)]
                  [rest (hexes-shift h (- 1 n))]
                  [x (bitwise-ior 1 (stream-car rest))])
             (values (hexes*n h 2^r)
               (remainder (bitwise-arithmetic-shift x r) 16)
               (cons (- r 4) (stream-cons x (stream-cdr rest))))))]))
;;}}}
;;{{{ Functions defined by power series
;;; Power series of type exp:
;;;   1 + x / 3! + x^2 / 5! + x^3 / 7! + ...
;;;   1 + x / 2! + x^2 / 4! + x^3 / 6! + ...
;;;; Note: Setting `m` to a constant leads to a more uniform output.
(define ($binaries-primitive-exp-germ b)
  (let-values ([(pl pl-binaries) (if (box? (cadr b))
      (values ($hexes-power-list (cddr b)) (lambda (h) (list* 0 (box 0) h)))
      (values ($hexes-invertible-power-list (cdr b)) (lambda (h) (cons 0 h))))])
    (let* ([d (car b)] [log<d-2> (log (- d 2))] [log2 (log 2)] [nl
        (let loop ([c -15] [nl (list)])
          (if (positive? c) nl
            (loop (1+ c) (cons (binaries-shift (r-binaries c) -4) nl))))] [cl
        (let loop ([coeff one-binaries] [j 1])
          (let* ([j (1+ j)] [coeff (binaries*r coeff (/ j))])
            (stream-cons coeff (loop coeff j))))])
      (let-syntax ([main (syntax-rules () [(_ arg)
          (let oloop ([count 0] [last 0] [cl arg] [pl pl] [x 0] [sum one-binaries])
            (let* ([k (expt 2 (max 0 (exact (ceiling
                (/ (- (log (+ count 3)) log<d-2>) log2)))))]
                   [l (ceiling (/ (1+ (* k (- d 2))) 4))]
                   [m (- l (quotient count 4))] [ct (+ count (* 4 m))])
              (let loop ([item (1+ last)] [cl cl] [pl pl]
                         [suml (list sum (list-ref nl x))])
                (if (<= item k)
                  (loop (1+ item) (stream-cdr (stream-cdr cl)) (stream-cdr pl)
                    (cons (binaries-shift
                            (binaries* (pl-binaries (stream-car pl)) (stream-car cl))
                            (- (* d item) count)) suml))
                  (let ([sum (apply binaries+ suml)])
                    (let-values ([(digits x rest)
                        ($binaries-get-first-digits sum m)])
                      (let loop ([j m] [digits digits])
                        (if (positive? j)
                          (stream-cons (stream-car digits)
                            (loop (1- j) (stream-cdr digits)))
                          (oloop ct k cl pl x rest)))))))))])])
        (values (cons 0 (main (stream-cdr cl))) (cons 0 (main cl)))))))
(define ($binaries-primitive-sin-cos b)
  (let ([-b2 (binaries- (binaries-square b))])
    (let-values ([(sing cosg) ($binaries-primitive-exp-germ -b2)])
      (values (binaries* b sing) cosg))))
(define ($binaries-primitive-sin b)
  (let-values ([(psin _) ($binaries-primitive-sin-cos b)])
    psin))
(define ($binaries-primitive-cos b)
  (let-values ([(_ pcos) ($binaries-primitive-sin-cos b)])
    pcos))
(define ($binaries-primitive-sinh-cosh b)
  (let ([b2 (binaries-square b)])
    (let-values ([(sing cosg) ($binaries-primitive-exp-germ b2)])
      (values (binaries* b sing) cosg))))
(define ($binaries-primitive-sinh b)
  (let-values ([(psinh _) ($binaries-primitive-sinh-cosh b)])
    psinh))
(define ($binaries-primitive-cosh b)
  (let-values ([(_ pcosh) ($binaries-primitive-sinh-cosh b)])
    pcosh))
(define ($binaries-primitive-exp b)
  (let-values ([(psinh pcosh) ($binaries-primitive-sinh-cosh b)])
    (binaries+ psinh pcosh)))
;;; Power series of type log:
;;;   1 + x / 3 + x^2 / 5 + x^3 / 7 + ...
;;;; Note: Setting `m` to a constant leads to a more uniform output.
(define ($binaries-primitive-log-germ b)
  (let-values ([(pl pl-binaries) (if (box? (cadr b))
      (values ($hexes-power-list (cddr b)) (lambda (h) (list* 0 (box 0) h)))
      (values ($hexes-invertible-power-list (cdr b)) (lambda (h) (cons 0 h))))])
    (let* ([d (car b)] [nl
        (let loop ([c -15] [nl (list)])
          (if (positive? c) nl
            (loop (1+ c) (cons (binaries-shift (r-binaries c) -4) nl))))])
      (cons 0
        (let oloop ([count 0] [m 1] [last 0] [pl pl] [x 0] [sum one-binaries])
          (let* ([ct (+ count (* 4 m))] [k (exact (ceiling (/ ct d)))])
            (let loop ([item (1+ last)] [coeff (+ 3 (* 2 last))] [pl pl]
                       [suml (list sum (list-ref nl x))])
              (if (<= item k)
                (loop (1+ item) (+ coeff 2) (stream-cdr pl)
                  (cons (binaries-shift
                          (binaries*r (pl-binaries (stream-car pl)) (/ coeff))
                          (- (* d item) count)) suml))
                (let ([sum (apply binaries+ suml)])
                  (let-values ([(digits x rest)
                      ($binaries-get-first-digits sum m)])
                    (let loop ([j m] [digits digits])
                      (if (positive? j)
                        (stream-cons (stream-car digits)
                          (loop (1- j) (stream-cdr digits)))
                        (oloop ct (1+ m) k pl x rest)))))))))))))
(define ($binaries-primitive-atan b)
  (let* ([-b2 (binaries- (binaries-square b))]
         [atang ($binaries-primitive-log-germ -b2)])
    (binaries* b atang)))
(define ($binaries-primitive-atanh b)
  (let* ([b2 (binaries-square b)]
         [atang ($binaries-primitive-log-germ b2)])
    (binaries* b atang)))
;;}}}
(define power-series-threshold: 50)
(define exp-eval-nest: 32)
(define log-eval-nest: 32)
(define cos-eval-nest: 32)
(define atan-eval-nest: 16)
;;; exp x = sqrt exp 2x
(define (binaries-exp b)
  (cond [(= (car b) +inf.0) one-binaries]
        [(box? (cadr b))
         (let ([e (max (ceiling (/ (+ power-series-threshold:
                      2 (- (car b))) 4)) 0)] [bx (cadr b)])
           (let-values ([(s h)
               ($eval-hexes-with-box bx (cddr b) e)])
             (if s (binaries-exp
                     ($make-binaries-with-box (car b) bx h))
               ($binaries-primitive-exp
                 (list* (+ (car b) (* 4 (unbox bx))) (box 0)
                   (hexes-shift (cddr b) (- (unbox bx))))))))]
        [(< (car b) 2) (error 'binaries-exp "Failed.")]
        [else
         (binaries-root
           ($binaries-primitive-exp
             (binaries-shift b exp-eval-nest:))
           (expt 2 exp-eval-nest:))]))
;;; log x = atanh ((x^2 - 1) / (x^2 + 1)) = 1/2 * log x^2
(define (binaries-log b)
  (cond [(box? (cadr b))
         (let ([e (max (1+ (ceiling (/ (- (car b)) 4))) 0)]
               [bx (cadr b)])
           (let-values ([(s h)
               ($eval-hexes-with-box bx (cddr b) e)])
             (if s (binaries-log
                     ($make-binaries-with-box (car b) bx h))
               (error 'binaries-log "Failed."))))]
        [(zero? (car b))
         (binaries-shift
           ($binaries-atanh
             (binaries- one-binaries
               (binaries-shift
                 (binaries/
                   (binaries+ one-binaries
                     (binaries-expt b
                       (expt 2 (1+ log-eval-nest:)))))
                 1)))
           (- log-eval-nest:))]
        [else (error 'binaries-log "Failed.")]))
;;; pow x y = exp (y * log x)
(define (binaries-pow b1 b2)
  (cond [(box? (cadr b1))
         (let ([e (max (1+ (ceiling (/ (- (car b1)) 4))) 0)]
               [bx (cadr b1)])
           (let-values ([(s h)
               ($eval-hexes-with-box bx (cddr b1) e)])
             (if s (binaries-pow
                     ($make-binaries-with-box (car b1) bx h) b2)
               (error 'binaries-pow "Failed."))))]
        [(zero? (car b1))
         (binaries-exp
           (let loop ([b (binaries* b2 (binaries-log b1))])
             (cond [(box? (cadr b))
                    (let ([e (max (ceiling (/ (+ power-series-threshold:
                                 2 (- (car b))) 4)) 0)] [bx (cadr b)])
                      (let-values ([(s h)
                          ($eval-hexes-with-box bx (cddr b) e)])
                        (if s
                          (loop ($make-binaries-with-box (car b) bx h))
                          (list* (+ (car b) (* 4 (unbox bx))) (box 0)
                            (hexes-shift (cddr b) (- (unbox bx)))))))]
                   [(< (car b) 2) (error 'binaries-pow "Failed.")]
                   [else b])))]
        [else (error 'binaries-pow "Failed.")]))
;;; sin x = sqrt ((1 - cos 2x) / 2)
(define (binaries-sin b)
  (cond [(= (car b) +inf.0) zero-binaries]
        [(box? (cadr b))
         (let ([e (max (ceiling (/ (+ power-series-threshold:
                      2 (- (car b))) 4)) 0)] [bx (cadr b)])
           (let-values ([(s h)
               ($eval-hexes-with-box bx (cddr b) e)])
             (if s (binaries-sin
                     ($make-binaries-with-box (car b) bx h))
               ($binaries-primitive-sin
                 (list* (+ (car b) (* 4 (unbox bx))) (box 0)
                   (hexes-shift (cddr b) (- (unbox bx))))))))]
        [(< (car b) 2) (error 'binaries-sin "Failed.")]
        [else
         (binaries-sqrt
           (binaries-shift
             (binaries- one-binaries
               (binaries-cos (binaries-shift b 1))) -1)
           (bitwise-arithmetic-shift (stream-car (cdr b)) -1))]))
;;; cos x = sqrt ((1 + cos 2x) / 2)
(define (binaries-cos b)
  (cond [(= (car b) +inf.0) one-binaries]
        [(box? (cadr b))
         (let ([e (max (ceiling (/ (+ power-series-threshold:
                      2 (- (car b))) 4)) 0)] [bx (cadr b)])
           (let-values ([(s h)
               ($eval-hexes-with-box bx (cddr b) e)])
             (if s (binaries-cos
                     ($make-binaries-with-box (car b) bx h))
               ($binaries-primitive-cos
                 (list* (+ (car b) (* 4 (unbox bx))) (box 0)
                   (hexes-shift (cddr b) (- (unbox bx))))))))]
        [(< (car b) 2) (error 'binaries-cos "Failed.")]
        [else
         (let loop ([j cos-eval-nest:]
                    [x ($binaries-primitive-cos
                         (binaries-shift b cos-eval-nest:))])
           (if (<= j 0) x
             (loop (1- j) (binaries-sqrt
                            (binaries-shift
                              (binaries+ one-binaries x) -1)))))]))
;;; tan x = sqrt ((1 - cos 2x) / (1 + cos 2x)) = sin 2x / (1 + cos 2x)
(define (binaries-tan b)
  (cond [(= (car b) +inf.0) zero-binaries]
        [(box? (cadr b))
         (let ([e (max (ceiling (/ (+ power-series-threshold:
                      1 (- (car b))) 4)) 0)] [bx (cadr b)])
           (let-values ([(s h)
               ($eval-hexes-with-box bx (cddr b) e)])
             (if s (binaries-tan
                     ($make-binaries-with-box (car b) bx h))
               (let-values ([(sin2x cos2x)
                   ($binaries-primitive-sin-cos
                     (list* (+ 1 (car b) (* 4 (unbox bx))) (box 0)
                       (hexes-shift (cddr b) (- (unbox bx)))))])
                 (binaries/ sin2x
                   (binaries+ one-binaries cos2x))))))]
        [(< (car b) 1) (error 'binaries-tan "Failed.")]
        [else
         (binaries-sqrt
           (binaries+ minus-one-binaries
             (binaries-shift
               (binaries/
                 (binaries+ one-binaries
                   (binaries-cos (binaries-shift b 1)))) 1))
           (bitwise-arithmetic-shift (stream-car (cdr b)) -1))]))
;;; asin x = atan (x / sqrt (1 - x^2))
(define (binaries-asin b)
  (let ([formula (lambda (x)
      (binaries-atan
        (binaries/ x
          (binaries-sqrt
            (binaries- one-binaries
              (binaries-square x))))))])
    (cond [(= (car b) +inf.0) zero-binaries]
          [(box? (cadr b))
           (let ([e (max (ceiling (/ (+ power-series-threshold:
                        2 (- (car b))) 4)) 0)] [bx (cadr b)])
             (let-values ([(s h)
                 ($eval-hexes-with-box bx (cddr b) e)])
               (if s (binaries-asin
                       ($make-binaries-with-box (car b) bx h))
                 (formula
                   (list* (+ (car b) (* 4 (unbox bx))) (box 0)
                     (hexes-shift (cddr b) (- (unbox bx))))))))]
          [(< (car b) 2) (error 'binaries-asin "Failed.")]
          [else (formula b)])))
;;; acos x = 2 atan sqrt ((1 - x) / (1 + x))
(define binaries-acos
  (case-lambda
    [(b) (binaries-acos b 0)]
    [(b index)
     (cond [(box? (cadr b))
            (let ([e (max (1+ (ceiling (/ (- (car b)) 4))) 0)]
                  [bx (cadr b)])
              (let-values ([(s h)
                  ($eval-hexes-with-box bx (cddr b) e)])
                (if s (binaries-acos
                        ($make-binaries-with-box (car b) bx h))
                  (error 'binaries-acos "Failed."))))]
           [(and (zero? (car b)) (binaries-sqrt? b #f))
            (let ([b (binaries+ minus-one-binaries
                       (binaries-shift
                         (binaries/
                           (binaries+ one-binaries
                             b)) 1))])
              (if (not (binaries-sqrt? b #f))
                (error 'binaries-acos "Failed."))
              (binaries-shift
                (binaries-atan
                  (binaries-sqrt b index)) 1))]
           [else (error 'binaries-acos "Failed.")])]))
;;; atan x = 1/2 * atan (2x / (1 - x^2))
(define (binaries-atan b)
  (call/cc (lambda (k)
    (let loop ([j atan-eval-nest:] [x
        (cond [(= (car b) +inf.0) (k zero-binaries)]
              [(box? (cadr b))
               (let ([e (max (ceiling (/ (+ power-series-threshold:
                            1 (- (car b))) 4)) 0)] [bx (cadr b)])
                 (let-values ([(s h)
                     ($eval-hexes-with-box bx (cddr b) e)])
                   (if s (k (binaries-atan
                       ($make-binaries-with-box (car b) bx h)))
                     (list* (+ (car b) (* 4 (unbox bx))) (box 0)
                       (hexes-shift (cddr b) (- (unbox bx)))))))]
              [(< (car b) 1) (error 'binaries-atan "Failed.")]
              [else b])])
      (if (<= j 0)
        (binaries-shift
          ($binaries-primitive-atan x)
          (- atan-eval-nest:))
        (loop (1- j)
          (binaries/
            (binaries-shift x 1)
            (binaries- one-binaries
              (binaries-square x)))))))))
;;; sinh x = (exp x - 1 / exp x) / 2
(define (binaries-sinh b)
  (cond [(= (car b) +inf.0) zero-binaries]
        [(box? (cadr b))
         (let ([e (max (ceiling (/ (+ power-series-threshold:
                      2 (- (car b))) 4)) 0)] [bx (cadr b)])
           (let-values ([(s h)
               ($eval-hexes-with-box bx (cddr b) e)])
             (if s (binaries-sinh
                     ($make-binaries-with-box (car b) bx h))
               ($binaries-primitive-sinh
                 (list* (+ (car b) (* 4 (unbox bx))) (box 0)
                   (hexes-shift (cddr b) (- (unbox bx))))))))]
        [(< (car b) 2) (error 'binaries-sinh "Failed.")]
        [else
         (let ([expx (binaries-exp b)])
           (binaries-shift
             (binaries- expx
               (binaries/ expx)) -1))]))
;;; cosh x = (exp x + 1 / exp x) / 2
(define (binaries-cosh b)
  (cond [(= (car b) +inf.0) one-binaries]
        [(box? (cadr b))
         (let ([e (max (ceiling (/ (+ power-series-threshold:
                      2 (- (car b))) 4)) 0)] [bx (cadr b)])
           (let-values ([(s h)
               ($eval-hexes-with-box bx (cddr b) e)])
             (if s (binaries-cosh
                     ($make-binaries-with-box (car b) bx h))
               ($binaries-primitive-cosh
                 (list* (+ (car b) (* 4 (unbox bx))) (box 0)
                   (hexes-shift (cddr b) (- (unbox bx))))))))]
        [(< (car b) 2) (error 'binaries-cosh "Failed.")]
        [else
         (let ([expx (binaries-exp b)])
           (binaries-shift
             (binaries+ expx
               (binaries/ expx)) -1))]))
;;; tanh x = (exp 2x - 1) / (exp 2x + 1)
(define (binaries-tanh b)
  (let ([formula (lambda (x)
      (binaries- one-binaries
        (binaries-shift
          (binaries/
            (binaries+ one-binaries
              (binaries-exp x))) 1)))])
    (cond [(= (car b) +inf.0) zero-binaries]
          [(box? (cadr b))
           (let ([e (max (ceiling (/ (+ power-series-threshold:
                        1 (- (car b))) 4)) 0)] [bx (cadr b)])
             (let-values ([(s h)
                 ($eval-hexes-with-box bx (cddr b) e)])
               (if s (binaries-tanh
                       ($make-binaries-with-box (car b) bx h))
                 (formula
                   (list* (+ 1 (car b) (* 4 (unbox bx))) (box 0)
                       (hexes-shift (cddr b) (- (unbox bx))))))))]
          [(< (car b) 1) (error 'binaries-tanh "Failed.")]
          [else (formula (binaries-shift b 1))])))
;;; asinh x = atanh (x / sqrt (1 + x^2))
(define (binaries-asinh b)
  (let ([formula (lambda (x)
      (binaries-atanh
        (binaries/ x
          (binaries-sqrt
            (binaries+ one-binaries
              (binaries-square x))))))])
    (cond [(= (car b) +inf.0) zero-binaries]
          [(box? (cadr b))
           (let ([e (max (ceiling (/ (+ power-series-threshold:
                        2 (- (car b))) 4)) 0)] [bx (cadr b)])
             (let-values ([(s h)
                 ($eval-hexes-with-box bx (cddr b) e)])
               (if s (binaries-asinh
                       ($make-binaries-with-box (car b) bx h))
                 (formula
                   (list* (+ (car b) (* 4 (unbox bx))) (box 0)
                     (hexes-shift (cddr b) (- (unbox bx))))))))]
          [(< (car b) 2) (error 'binaries-asinh "Failed.")]
          [else (formula b)])))
;;; acosh x = 2 atanh sqrt ((x - 1) / (x + 1))
(define binaries-acosh
  (case-lambda
    [(b) (binaries-acosh b 0)]
    [(b index)
     (cond [(box? (cadr b))
            (let ([e (max (1+ (ceiling (/ (- (car b)) 4))) 0)]
                  [bx (cadr b)])
              (let-values ([(s h)
                  ($eval-hexes-with-box bx (cddr b) e)])
                (if s (binaries-acosh
                        ($make-binaries-with-box (car b) bx h))
                  (error 'binaries-acosh "Failed."))))]
           [(and (zero? (car b)) (binaries-sqrt? b #f))
            (let ([b (binaries- one-binaries
                       (binaries-shift
                         (binaries/
                           (binaries+ one-binaries
                             b)) 1))])
              (if (not (binaries-sqrt? b #f))
                (error 'binaries-acosh "Failed."))
              (binaries-shift
                (binaries-atanh
                  (binaries-sqrt b index)) 1))]
           [else (error 'binaries-acosh "Failed.")])]))
;;; atanh x = 1/2 * log ((1 + x) / (1 - x))
(define ($binaries-atanh b)
  (if (box? (cadr b))
    (let ([e (max (ceiling (/ (+ power-series-threshold:
                 1 (- (car b))) 4)) 0)] [bx (cadr b)])
      (let-values ([(s h)
          ($eval-hexes-with-box bx (cddr b) e)])
        (if s ($binaries-atanh
                ($make-binaries-with-box (car b) bx h))
          ($binaries-primitive-atanh
            (list* (+ (car b) (* 4 (unbox bx))) (box 0)
              (hexes-shift (cddr b) (- (unbox bx))))))))
    ($binaries-primitive-atanh b)))
(define (binaries-atanh b)
  (call/cc (lambda (k)
    (cond [(= (car b) +inf.0) (k zero-binaries)]
          [(box? (cadr b))
           (let ([e (max (ceiling (/ (+ power-series-threshold:
                        1 (- (car b))) 4)) 0)] [bx (cadr b)])
             (let-values ([(s h)
                 ($eval-hexes-with-box bx (cddr b) e)])
               (if s (k (binaries-atanh
                   ($make-binaries-with-box (car b) bx h))))))]
          [(< (car b) 1) (error 'binaries-atanh "Failed.")])
    (binaries-shift
      (binaries-log
        (binaries+ minus-one-binaries
          (binaries-shift
            (binaries/
              (binaries- one-binaries b)) 1))) -1))))
;;}}}

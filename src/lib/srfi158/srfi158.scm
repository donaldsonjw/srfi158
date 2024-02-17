;;;; Bigloo adaption of srfi158
;;;; Copyright (C) Joseph Donaldson (2024). All Rights Reserved
;;;;
;;;; srfi158 Sample Implementation
;;;; Copyright (C) Shiro Kawai, John Cowan, Thomas Gilray (2015). All Rights Reserved.
;;;;
;;;; Permission is hereby granted, free of charge,
;;;; to any person obtaining a copy of this software and associated
;;;; documentation files (the "Software"), to deal in the Software
;;;; without restriction, including without limitation the rights to
;;;; use, copy, modify, merge, publish, distribute, sublicense, and/or
;;;; sell copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions: The above copyright notice and this permission notice
;;;; shall be included in all copies or substantial portions of the
;;;; Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(module srfi158
   (cond-expand
      (bigloo-jvm
       (library pthread
            bigloo-concurrent)))
   
   (extern
      (include "cnumprocs.h")
      (macro $get-number-of-processors::long ()
             "bgl_get_number_of_processors")

      (include "ccoroutine.h")
      (type %coroutine
         (struct)
         "struct bgl_coroutine")
      (macro $make-coroutine::%coroutine* (thunk::procedure) "bgl_make_coroutine")
      (macro $coroutine-yield::obj (cor::%coroutine* val::obj) "bgl_coroutine_yield")
      (macro $coroutine-call::obj (cor::%coroutine*) "bgl_coroutine_call")
      (macro $coroutine-finalize::void (cor::%coroutine* val::obj) "bgl_coroutine_finalize"))

   (java
      (class processor_utils
         (method static $get-number-of-processors::long () "bgl_get_number_of_processors")
         "bigloo.lib.srfi158.processor_utils"))
   
   (export (generator . args)
           (circular-generator . args)
           (make-coroutine-generator proc::procedure)
           (list->generator lst)
           vector->generator
           reverse-vector->generator
           string->generator
           bytevector->generator
           (make-for-each-generator for-each obj)
           (make-unfold-generator stop? mapper successor seed)
           (gcons* . args)
           (gappend . args)
           (gflatten gen)
           ggroup
           gmerge
           gmap
           (gcombine proc seed . gens)
           (gfilter pred gen)
           (gstate-filter proc seed gen)
           (gremove pred gen)
           gtake
           (gdrop gen k)
           (gdrop-while pred gen)
           (gtake-while pred gen)
           gdelete
           gdelete-neighbor-dups
           (gindex value-gen index-gen)
           (gselect value-gen truth-gen)
           generator->list
           generator->reverse-list
           generator->vector
           (generator->vector! vector at gen)
           generator->string
           (generator-fold f seed . gs)
           (generator-for-each f . gs)
           (generator-map->list f . gs)
           (generator-find pred g)
           (generator-count pred g)
           (generator-any pred gen)
           (generator-every pred gen)
           (generator-unfold g unfold . args)
           (make-accumulator kons knil finalize)
           (count-accumulator)
           (list-accumulator)
           (reverse-list-accumulator)
           (vector-accumulator)
           (reverse-vector-accumulator)
           (vector-accumulator! vec at)
           (bytevector-accumulator)
           (bytevector-accumulator! bytevec at)
           (string-accumulator)
           (sum-accumulator)
           (product-accumulator)
           make-iota-generator
           make-range-generator)
   )


;; utilities and macros needed to adapt srfi158 to Bigloo  
(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda)
     (lambda args
       (error "case-lambda" "case-lambda without any clauses." #unspecified)))
    ((case-lambda 
      (?a1 ?e1 ...) 
      ?clause1 ...)
     (lambda args
       (let ((l (length args)))
         (case-lambda "CLAUSE" args l 
           (?a1 ?e1 ...)
           ?clause1 ...))))
    ((case-lambda "CLAUSE" ?args ?l 
      ((?a1 ...) ?e1 ...) 
      ?clause1 ...)
     (if (= ?l (length '(?a1 ...)))
         (apply (lambda (?a1 ...) ?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l 
           ?clause1 ...)))
    ((case-lambda "CLAUSE" ?args ?l
      ((?a1 . ?ar) ?e1 ...) 
      ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l 1 (?a1 . ?ar) (?ar ?e1 ...) 
       ?clause1 ...))
    ((case-lambda "CLAUSE" ?args ?l 
      (?a1 ?e1 ...)
      ?clause1 ...)
     (let ((?a1 ?args))
       ?e1 ...))
    ((case-lambda "CLAUSE" ?args ?l)
     (error "case-lambda" "Wrong number of arguments to CASE-LAMBDA." #unspecified))
    ((case-lambda "IMPROPER" ?args ?l ?k ?al ((?a1 . ?ar) ?e1 ...)
      ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l (+ ?k 1) ?al (?ar ?e1 ...) 
      ?clause1 ...))
    ((case-lambda "IMPROPER" ?args ?l ?k ?al (?ar ?e1 ...) 
      ?clause1 ...)
     (if (>= ?l ?k)
         (apply (lambda ?al ?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l 
           ?clause1 ...)))))          

(define-expander define-values
   (lambda (x e)
      (match-case x
         ((?- (?b1 . ?b2) ?exp)
          (let* ((defines (cons b1 b2))
                (receive-syms (map (lambda (x1) (gensym x1)) defines)))
             (e `(begin
                    ,@(map (lambda (x2) `(define ,x2 #unspecified)) defines)
                    .
                    `(receive ,receive-syms ,exp
                             ,@(map (lambda (b v) `(set! ,b ,v)) defines receive-syms))) e)))
         (else
          (error "define-values" "invalid form" x)))))

(define-syntax let-values
   (syntax-rules ()
      ((let-values (((v1 v2 ...) val-producer))
          body ...)
       (receive (v1 v2 ...) val-producer
                body ...))))


;; bytevectors are u8vectors in Bigloo
(define-inline (make-bytevector size #!optional (default #u8:0))
   (make-u8vector size default))

;(define make-bytevector make-u8vector)

(define bytevector-u8-set! u8vector-set!)

(define bytevector-length u8vector-length)

(define bytevector-u8-ref u8vector-ref)

(define (srfi158-error msg . args)
   (error "srfi158" msg args))

(define (get-number-of-processors)
   (cond-expand
      (bigloo-c
       ($get-number-of-processors)
       )
      (bigloo-jvm
       (processor_utils-$get-number-of-processors))))

;; list->bytevector
(define (list->bytevector list)
  (let ((vec (make-bytevector (length list) #u8:0)))
    (let loop ((i 0) (list list))
      (if (null? list)
        vec
        (begin
          (bytevector-u8-set! vec i (car list))
          (loop (+ i 1) (cdr list)))))))


;; generator
(define (generator . args)
  (lambda () (if (null? args)
               (eof-object)
               (let ((next (car args)))
                (set! args (cdr args))
                next))))

;; circular-generator
(define (circular-generator . args)
  (let ((base-args args))
    (lambda ()
      (when (null? args)
        (set! args base-args))
          (let ((next (car args)))
                (set! args (cdr args))
                next))))


;; make-iota-generator
(define make-iota-generator
  (case-lambda ((count) (make-iota-generator count 0 1))
               ((count start) (make-iota-generator count start 1))
               ((count start step) (make-iota count start step))))

;; make-iota
(define (make-iota count start step)
  (lambda ()
    (cond
      ((<= count 0)
       (eof-object))
      (else
        (let ((result start))
         (set! count (- count 1))
         (set! start (+ start step))
         result)))))


;; make-range-generator
(define make-range-generator
  (case-lambda ((start end) (make-range-generator start end 1))
               ((start) (make-infinite-range-generator start))
               ((start end step)
                (set! start (- (+ start step) step))
                (lambda () (if (< start end)
                             (let ((v start))
                              (set! start (+ start step))
                              v)
                             (eof-object))))))

(define (make-infinite-range-generator start)
  (lambda ()
    (let ((result start))
     (set! start (+ start 1))
     result)))




(cond-expand

   ;; For the native backend, we use actual coroutines, based on
   ;; makecontext, for make-coroutine-generator. In my, admittedly,
   ;; unscientific benchmarking, this implementation is 4-5 times faster
   ;; than the thread-based implementation.
   (bigloo-c
    
    (define (make-coroutine-generator proc::procedure)
       (letrec  ((thunk (lambda () (let ((res (proc (lambda (v)
                                                       ($coroutine-yield cor v)))))
                                      ($coroutine-finalize cor (eof-object)))))
                 (cor::%coroutine* ($make-coroutine thunk)))
          (lambda ()
             ($coroutine-call cor)))))

   ;; For the jvm backend, we use threads and a dedicated threadpool to emulate coroutines. 
   (bigloo-jvm

    (define +generator-mutex+ (make-mutex))
    (define +generator-thread-pool+ #unspecified)
    (define (generator-thread-pool)
       (synchronize +generator-mutex+
          (when (eq? +generator-thread-pool+ #unspecified)
             (set! +generator-thread-pool+ (make-thread-pool (get-number-of-processors))))
          +generator-thread-pool+))
    
    
    (define +corroutine-calling+ '(list corroutine-calling))

    (define (make-coroutine-generator proc::procedure)
       (letrec ((state 'init)
                (result #unspecified)
                (mutex (make-mutex))
                (condv (make-condition-variable))
                (wait-on-condition! (lambda (condv mutex pred)
                                       ;; a loop checking our condition
                                       ;; predicate to make sure we are
                                       ;; not dealing with a spurious
                                       ;; wakeup
                                       (let loop ()
                                          (condition-variable-wait! condv mutex)
                                          (when (not (pred))
                                             (loop)))))
                (start (lambda ()
                          (thread-pool-push-task! (generator-thread-pool)
                             (lambda ()
                                (let ((res
                                         (proc (lambda (v)
                                                  (synchronize mutex
                                                     (set! result v)
                                                     (condition-variable-signal! condv)
                                                     (if (eof-object? result)
                                                         result
                                                         (wait-on-condition! condv mutex
                                                            (lambda () (eq? result +corroutine-calling+)))
                                                         
                                                         ))))))
                                   (synchronize mutex
                                      (set! state 'finished)
                                      (set! result (eof-object))
                                      (condition-variable-signal! condv))))))))
          (lambda ()
             (case state
                ((init)
                 (synchronize mutex
                    (set! state 'running)
                    (set! result +corroutine-calling+)
                    (start)
                    (wait-on-condition! condv mutex
                       (lambda () (not (eq? result +corroutine-calling+))))
                    result))
                ((running)
                 (synchronize mutex
                    (set! result +corroutine-calling+)
                    (condition-variable-signal! condv)
                    (wait-on-condition! condv mutex
                       (lambda () (not (eq? result +corroutine-calling+))))
                    result))
                (else
                 result)))))
    ))


;; list->generator
(define (list->generator lst)
  (lambda () (if (null? lst)
               (eof-object)
               (let ((next (car lst)))
                (set! lst (cdr lst))
                next))))


;; vector->generator
(define vector->generator
  (case-lambda ((vec) (vector->generator vec 0 (vector-length vec)))
               ((vec start) (vector->generator vec start (vector-length vec)))
               ((vec start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (vector-ref vec start)))
                              (set! start (+ start 1))
                              next))))))


;; reverse-vector->generator
(define reverse-vector->generator
  (case-lambda ((vec) (reverse-vector->generator vec 0 (vector-length vec)))
               ((vec start) (reverse-vector->generator vec start (vector-length vec)))
               ((vec start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (vector-ref vec (- end 1))))
                              (set! end (- end 1))
                              next))))))


;; string->generator
(define string->generator
  (case-lambda ((str) (string->generator str 0 (string-length str)))
               ((str start) (string->generator str start (string-length str)))
               ((str start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (string-ref str start)))
                              (set! start (+ start 1))
                              next))))))


;; bytevector->generator
(define bytevector->generator
  (case-lambda ((str) (bytevector->generator str 0 (bytevector-length str)))
               ((str start) (bytevector->generator str start (bytevector-length str)))
               ((str start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (bytevector-u8-ref str start)))
                              (set! start (+ start 1))
                              next))))))


;; make-for-each-generator
(define (make-for-each-generator for-each obj)
  (make-coroutine-generator (lambda (yield) (for-each yield obj))))


;; make-unfold-generator
(define (make-unfold-generator stop? mapper successor seed)
  (make-coroutine-generator (lambda (yield)
                              (let loop ((s seed))
                               (if (stop? s)
                                 (if #f #f)
                                 (begin (yield (mapper s))
                                        (loop (successor s))))))))


;; gcons*
(define (gcons* . args)
  (lambda () (if (null? args)
               (eof-object)
               (if (= (length args) 1)
                 ((car args))
                 (let ((v (car args)))
                  (set! args (cdr args))
                  v)))))


;; gappend
(define (gappend . args)
  (lambda () (if (null? args)
               (eof-object)
               (let loop ((v ((car args))))
                (if (eof-object? v)
                  (begin (set! args (cdr args))
                         (if (null? args)
                           (eof-object)
                           (loop ((car args)))))
                  v)))))

;; gflatten
(define (gflatten gen)
  (let ((state '()))
    (lambda ()
      (if (null? state) (set! state (gen)))
      (if (eof-object? state)
        state
        (let ((obj (car state)))
          (set! state (cdr state))
          obj)))))

;; ggroup
(define ggroup
  (case-lambda
    ((gen k)
     (simple-ggroup gen k))
    ((gen k padding)
     (padded-ggroup (simple-ggroup gen k) k padding))))

(define (simple-ggroup gen k)
  (lambda ()
    (let loop ((item (gen)) (result '()) (count (- k 1)))
      (if (eof-object? item)
        (if (null? result) item (reverse result))
        (if (= count 0)
          (reverse (cons item result))
          (loop (gen) (cons item result) (- count 1)))))))

(define (padded-ggroup gen k padding)
  (lambda ()
    (let ((item (gen)))
      (if (eof-object? item)
        item
        (let ((len (length item)))
          (if (= len k)
              item
              (append item (make-list (- k len) padding))))))))

;; gmerge
(define gmerge
  (case-lambda
    ((<) (srfi158-error "wrong number of arguments for gmerge"))
    ((< gen) gen)
    ((< genleft genright)
     (let ((left (genleft))
           (right (genright)))
       (lambda ()
         (cond
          ((and (eof-object? left) (eof-object? right))
           left)
          ((eof-object? left)
           (let ((obj right)) (set! right (genright)) obj))
          ((eof-object? right)
           (let ((obj left))  (set! left (genleft)) obj))
          ((< right left)
           (let ((obj right)) (set! right (genright)) obj))
          (else
           (let ((obj left)) (set! left (genleft)) obj))))))
    ((< . gens)
     (apply gmerge <
            (let loop ((gens gens) (gs '()))
              (cond ((null? gens) (reverse gs))
                    ((null? (cdr gens)) (reverse (cons (car gens) gs)))
                    (else (loop (cddr gens)
                                (cons (gmerge < (car gens) (cadr gens)) gs)))))))))
    
;; gmap
(define gmap
  (case-lambda
    ((proc) (srfi158-error "wrong number of arguments for gmap"))
    ((proc gen)
     (lambda ()
       (let ((item (gen)))
         (if (eof-object? item) item (proc item)))))
    ((proc . gens)
     (lambda ()
       (let ((items (map (lambda (x) (x)) gens)))
         (if (any eof-object? items) (eof-object) (apply proc items)))))))
    
;; gcombine
(define (gcombine proc seed . gens)
  (lambda ()
    (define items (map (lambda (x) (x)) gens))
    (if (any eof-object? items)
      (eof-object)
      (let ()
       (define-values (value newseed) (apply proc (append items (list seed))))
       (set! seed newseed)
       value))))

;; gfilter
(define (gfilter pred gen)
  (lambda () (let loop ()
              (let ((next (gen)))
               (if (or (eof-object? next)
                       (pred next))
                 next
                 (loop))))))

;; gstate-filter
(define (gstate-filter proc seed gen)
  (let ((state seed))
    (lambda ()
      (let loop ((item (gen)))
        (if (eof-object? item)
          item
          (let-values (((yes newstate) (proc item state)))
            (set! state newstate)
            (if yes
               item
               (loop (gen)))))))))



;; gremove
(define (gremove pred gen)
  (gfilter (lambda (v) (not (pred v))) gen))



;; gtake
(define gtake
  (case-lambda ((gen k) (gtake gen k (eof-object)))
               ((gen k padding)
                (make-coroutine-generator (lambda (yield)
                                            (if (> k 0)
                                              (let loop ((i 0) (v (gen)))
                                               (begin (if (eof-object? v) (yield padding) (yield v))
                                                      (if (< (+ 1 i) k)
                                                        (loop (+ 1 i) (gen))
                                                        (eof-object))))
                                              (eof-object)))))))



;; gdrop
(define (gdrop gen k)
  (lambda () (do () ((<= k 0)) (set! k (- k 1)) (gen))
    (gen)))



;; gdrop-while
(define (gdrop-while pred gen)
  (define found #f)
  (lambda ()
    (let loop ()
     (let ((val (gen)))
      (cond (found val)
            ((and (not (eof-object? val)) (pred val)) (loop))
            (else (set! found #t) val))))))


;; gtake-while
(define (gtake-while pred gen)
  (lambda () (let ((next (gen)))
              (if (eof-object? next)
                next
                (if (pred next)
                    (begin
                       next)
                  (begin (set! gen (generator))
                         (gen)))))))



;; gdelete
(define gdelete
  (case-lambda ((item gen) (gdelete item gen equal?))
               ((item gen ==)
                (lambda () (let loop ((v (gen)))
                            (cond
                              ((eof-object? v) (eof-object))
                              ((== item v) (loop (gen)))
                              (else v)))))))



;; gdelete-neighbor-dups
(define gdelete-neighbor-dups
  (case-lambda ((gen)
                (gdelete-neighbor-dups gen equal?))
               ((gen ==)
                (define firsttime #t)
                (define prev #f)
                (lambda () (if firsttime
                             (begin (set! firsttime #f)
                                    (set! prev (gen))
                                    prev)
                             (let loop ((v (gen)))
                              (cond
                                ((eof-object? v)
                                 v)
                                ((== prev v)
                                 (loop (gen)))
                                (else
                                  (set! prev v)
                                  v))))))))


;; gindex
(define (gindex value-gen index-gen)
  (let ((done? #f) (count 0))
   (lambda ()
     (if done?
       (eof-object)
       (let loop ((value (value-gen)) (index (index-gen)))
        (cond
          ((or (eof-object? value) (eof-object? index))
           (set! done? #t)
           (eof-object))
          ((= index count)
           (set! count (+ count 1))
           value)
          (else
            (set! count (+ count 1))
            (loop (value-gen) index))))))))


;; gselect
(define (gselect value-gen truth-gen)
  (let ((done? #f))
   (lambda ()
     (if done?
       (eof-object)
       (let loop ((value (value-gen)) (truth (truth-gen)))
        (cond
          ((or (eof-object? value) (eof-object? truth))
           (set! done? #t)
           (eof-object))
          (truth value)
          (else (loop (value-gen) (truth-gen)))))))))

;; generator->list
(define generator->list
  (case-lambda ((gen n)
		(generator->list (gtake gen n)))
               ((gen)
		(reverse (generator->reverse-list gen)))))

;; generator->reverse-list
(define generator->reverse-list
  (case-lambda ((gen n)
		(generator->reverse-list (gtake gen n)))
               ((gen)
		(generator-fold cons '() gen))))

;; generator->vector
(define generator->vector
  (case-lambda ((gen) (list->vector (generator->list gen)))
               ((gen n) (list->vector (generator->list gen n)))))


;; generator->vector!
(define (generator->vector! vector at gen)
  (let loop ((value (gen)) (count 0) (at at))
   (cond
     ((eof-object? value) count)
     ((>= at (vector-length vector)) count)
     (else (begin
             (vector-set! vector at value)
             (loop (gen) (+ count 1) (+ at 1)))))))


;; generator->string
(define generator->string
  (case-lambda ((gen) (list->string (generator->list gen)))
               ((gen n) (list->string (generator->list gen n)))))




;; generator-fold
(define (generator-fold f seed . gs)
  (define (inner-fold seed)
    (let ((vs (map (lambda (g) (g)) gs)))
     (if (any eof-object? vs)
       seed
       (inner-fold (apply f (append vs (list seed)))))))
  (inner-fold seed))



;; generator-for-each
(define (generator-for-each f . gs)
  (let loop ()
   (let ((vs (map (lambda (g) (g)) gs)))
    (if (any eof-object? vs)
      (if #f #f)
      (begin (apply f vs)
             (loop))))))


(define (generator-map->list f . gs)
  (let loop ((result '()))
   (let ((vs (map (lambda (g) (g)) gs)))
    (if (any eof-object? vs)
      (reverse result)
      (loop (cons (apply f vs) result))))))


;; generator-find
(define (generator-find pred g)
  (let loop ((v (g)))
    (cond ((eof-object? v) #f)
          ((pred v) v)
          (else (loop (g))))))

;; generator-count
(define (generator-count pred g)
  (generator-fold (lambda (v n) (if (pred v) (+ 1 n) n)) 0 g))


;; generator-any
(define (generator-any pred gen)
  (let loop ((item (gen)))
    (cond ((eof-object? item) #f)
          ((pred item))
          (else (loop (gen))))))


;; generator-every
(define (generator-every pred gen)
  (let loop ((item (gen)) (last #t))
    (if (eof-object? item)
      last
      (let ((r (pred item)))
        (if r
          (loop (gen) r)
          #f)))))


;; generator-unfold
(define (generator-unfold g unfold . args)
  (apply unfold eof-object? (lambda (x) x) (lambda (x) (g)) (g) args))


;; make-accumulator
(define (make-accumulator kons knil finalize)
  (let ((state knil))
    (lambda (obj)
      (if (eof-object? obj)
        (finalize state)
        (set! state (kons obj state))))))


;; count-accumulator
(define (count-accumulator) (make-accumulator
                            (lambda (obj state) (+ 1 state)) 0 (lambda (x) x)))

;; list-accumulator
(define (list-accumulator) (make-accumulator cons '() reverse))

;; reverse-list-accumulator
(define (reverse-list-accumulator) (make-accumulator cons '() (lambda (x) x)))

;; vector-accumulator
(define (vector-accumulator)
  (make-accumulator cons '() (lambda (x) (list->vector (reverse x)))))

;; reverse-vector-accumulator
(define (reverse-vector-accumulator)
  (make-accumulator cons '() list->vector))

;; vector-accumulator!
(define (vector-accumulator! vec at)
  (lambda (obj)
    (if (eof-object? obj)
      vec
      (begin
        (vector-set! vec at obj)
        (set! at (+ at 1))))))

;; bytevector-accumulator
(define (bytevector-accumulator)
  (make-accumulator cons '() (lambda (x) (list->bytevector (reverse x)))))

(define (bytevector-accumulator! bytevec at)
  (lambda (obj)
    (if (eof-object? obj)
      bytevec
      (begin
        (bytevector-u8-set! bytevec at obj)
        (set! at (+ at 1))))))

;; string-accumulator
(define (string-accumulator)
  (make-accumulator cons '()
        (lambda (lst) (list->string (reverse lst)))))

;; sum-accumulator
(define (sum-accumulator) (make-accumulator + 0 (lambda (x) x)))

;; product-accumulator
(define (product-accumulator) (make-accumulator * 1 (lambda (x) x)))
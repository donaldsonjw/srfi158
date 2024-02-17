(module configure
   (main main))


(define *native-build* #unspecified)
(define *jvm-build* #unspecified)
(define *have-makecontext* #unspecified)

(define *remaining-args* #unspecified)
(define *prefix* "/usr/local")

(define (parse-args args)
   (args-parse (cdr args)
      (section "Help")
      ((("--help")
        (help "--help" "This help message"))
       (args-parse-usage #f))
      (section "Backend")
      ((("--disable-native")
        (help "--disable-native" "disable native backend build"))
       (set! *native-build* #f))
      ((("--disable-jvm")
        (help "--disable-jvm" "disable jvm backend build"))
       (set! *jvm-build* #f))
      (("--prefix=?prefix" (help "the installation prefix [default /usr/local]"))
       (set! *prefix* prefix))
      (else
       (print "illegal argument '" else "'. Usage:")
       (args-parse-usage #f)
       (exit -1))))

(define (auto-config)
   (when (eq? *have-makecontext* #unspecified)
      (set! *have-makecontext* (equal? "true" (build-and-exec-c check-makecontext))))
   
   (when (eq? *native-build* #unspecified)
      (set!  *native-build* (and (equal? "true" (build-and-exec hello-module))
                                 *have-makecontext*)))
   
   (when (eq? *jvm-build* #unspecified)
      (set! *jvm-build* (equal? "true" (build-and-exec hello-module "-jvm")))))

(define (main args)
   (parse-args args)
   (auto-config)
   (with-output-to-file "Makefile.config"
      (lambda ()
         (printf "SUPPORT_NATIVE=~a~%" (if *native-build* "true" "false"))
         (printf "SUPPORT_JVM=~a~%" (if *jvm-build* "true" "false"))
         (printf "HAS_MAKECONTEXT=~a~%" (if *have-makecontext* "true" "false"))
         (printf "INSTALL_PREFIX=~a~%" *prefix*)))
   (printf "** Configuration Summary **~%~%")
   (printf "makecontext supported.... ~a ~%" *have-makecontext*)
   (printf "supported backends....... ~(, ) ~%" (append (if *native-build* '(native) '())
                                               (if *jvm-build* '(jvm) '())))
   (printf "install prefix........... ~a ~%" *prefix*)
   (printf "~%"))

(define (ends-with-quote? val::symbol)
   (let ((str (symbol->string val)))
      (char=? #\' (string-ref str
                     (- (string-length str) 1)))))

(define (sh->string obj)
   (match-case obj
      ((quote (and ?a
                   (? symbol?)
                   (? ends-with-quote?)))
       (format "'~a'" (let ((str (symbol->string a)))
                         (substring str 0 (- (string-length str) 1)))))
      ((and ?a (? symbol?))
       (symbol->string a))
      ((and ?a (or (? string?) (? number?)))
       (format "~a" a))
      (else
       (error "sh" "invalid syntax" obj))))

(define (string-index-w/pred str pred?)
   (let ((len (string-length str)))
      (let loop ((i 0))
         (cond ((= i len) #f)
               ((pred? (string-ref str i))
                i)
               (else
                (loop (+ i 1)))))))


(define (string-index-right-w/pred str pred?)
   (let ((len (string-length str)))
      (let loop ((i (- len 1)))
         (cond ((< i 0) #f)
               ((pred? (string-ref str i))
                i)
               (else
                (loop (- i 1)))))))

(define (string-trim str)
   (let ((left (string-index-w/pred str
                  (lambda (c) (not (char-whitespace? c)))))
         (right (string-index-right-w/pred str
                   (lambda (c) (not (char-whitespace? c))))))
      (if left
          (substring str left (+ right 1))
          "")))

(define (sh cmd . args)
   (let* ((proc-args (append (map sh->string (cons cmd args))
                        '(:output :pipe :error :pipe :wait #t)))
          (proc (apply run-process proc-args))
          (exit-status (process-exit-status proc))
          (out (process-output-port proc))
          (err (process-error-port proc)))
      (unwind-protect
         (if (and (number? exit-status) (= exit-status 0))
             (string-trim (read-string out))
             #f)
         (begin
            (close-input-port out)
            (close-input-port err)))))


(define (build-and-exec scm-src . args)
   (let* ((tempdir ".autoconf")
          (tempexe  (symbol->string (gensym "autoconfig")))
          (tempsrc (string-append tempexe ".scm")))
      (make-directory tempdir)
      (chdir tempdir)
      (with-output-to-file tempsrc
         (lambda () (display scm-src)))
      (unwind-protect
         (and (apply sh
                 (append (cons* "bigloo" "-s" args)
                    (list "-o" tempexe tempsrc)))
              (sh (format "./~a" tempexe)))
         (begin
            (chdir "..")
            (sh "rm" "-rf" tempdir)))))

(define (build-and-exec-c c-src . args)
   (let* ((tempdir ".autoconfc")
          (tempexe  (symbol->string (gensym "autoconfig")))
          (tempsrc (string-append tempexe ".c")))
      (make-directory tempdir)
      (chdir tempdir)
      (with-output-to-file tempsrc
         (lambda () (display c-src)))
      (unwind-protect
         (and (apply sh
                 (append (cons* "gcc"  args)
                    (list "-o" tempexe tempsrc)))
              (sh (format "./~a" tempexe)))
         (begin
            (chdir "..")
            ))))

(define hello-module
   "(module hello
      (main main))

    (define (main args)
      (print \"true\"))")



(define check-makecontext
   "#include <ucontext.h>

    int main(int argc, char** argv) {
       ucontext_t context;
       getcontext(&context);
       printf(\"true\");
       return 0;
    }
   ")
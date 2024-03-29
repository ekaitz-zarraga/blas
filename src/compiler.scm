;; BIBLIOGRAPHY:
;; https://github.com/schemedoc/bibliography

(import (scheme write)
        (srfi srfi-1)
        (srfi srfi-60)
        (lib riscv)
        (lib riscv-spike))

(define (parse file)
  (call-with-input-file file
    (lambda (port) (read port))))

;;; CONSTANTS
(define const-pool '((#t . "TRUE")
                     (#f . "FALSE")
                     (() . "NIL")))

(define (found-constant const)
  (let* ((found (assoc const const-pool))
         (label (if found
                    (cdr found)
                    (string-append
                      "const"
                      (number->string (length const-pool))))))
    (when (not found)
      (set! const-pool (alist-cons const label const-pool)))
    (load-word label)))

(define (found-boolean b)
  (if b
    (load-true)
    (load-false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (compile-call call args)
  ;; TODO: ThIS IS WRONG BUT IT'S JUST AN IDEA
  ;; Rewrite the push/pop to avoid using register names
  "Compile inlined, it expects the arguments to be stored on the stack"
  (define argc (length args))
  (let loop ((c argc)
             (a (reverse args)))
    (compile (car a))
    (push "a0")
    (when (not (null? (cdr a)))
      (loop (- c 1) (cdr a))))
  (pop "a0")
  (let loop ((c (- argc 1)))
    (when (< 0 c)
      (pop "a1")
      (intadd)
      (loop (- c 1)))))




(define (compile-exp call args)
  (cond
    ((eq? call 'if) (compile-if args))
    ;((eq? call 'if) (compileif argc))
    ;((eq? call 'if) (compileif argc))
    ;((eq? call 'if) (compileif argc))
    (else (compile-call call args))))


(define (compile ex)
  (cond
    ((boolean?  ex) (found-boolean ex))
    ((integer?  ex) (found-constant ex))
    ((char?     ex) (found-constant ex))
    ((list?     ex) (compile-exp (car ex) (cdr ex)))))

(define (compile-to-file ex)
  (prologue)
  (comment "Program starts here")
  (compile ex)
  (comment "Exit block")
  (finish)
  (epilogue)
  (static-data const-pool)
  (data))

(compile-to-file '(+ 1 1 1 1))

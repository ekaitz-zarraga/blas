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

;;; INLINED

; Integers can be added directly without affecting the value
; Doesn't check!
(define (intadd)
  (emit "add a0, a0, a1"))
(define (intsub)
  (emit "sub a0, a0, a1"))
(define (=2)
  (emit "beq a0, a1, 1f")
  (emit "lw a0, FALSE")
  (emit "j 2f")
  (tag "1")
  (emit "lw a0, TRUE")
  (tag "2"))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (compile-call call args)
  ;; TODO: ThIS IS WRONG BUT IT'S JUST AN IDEA
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


(define (compile-if args)
  (when (let ((l (length args)))
          (or (< 3 l) (> 2 l)))
    (error "Malformed if: (if CONDITION BODY [ELSE-BODY])"))

  (let ((condition (first args))
        (body      (second args))
        (else-body (if (= 3 (length args)) (third args) #f)))

    (compile condition)
    (emit "lw a1, FALSE")
    (emit "beq a0, a1, 1f")
    (compile body)
    (emit "j 2f")
    (tag "1")
    (compile else-body)
    (tag "2")))

(define (compile-exp call args)
  (cond
    ((eq? call 'if) (compile-if args))
    ;((eq? call 'if) (compileif argc))
    ;((eq? call 'if) (compileif argc))
    ;((eq? call 'if) (compileif argc))
    (else (compile-call call args))))


(define (compile ex)
  (cond
    ((integer?  ex) (found-constant ex))
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

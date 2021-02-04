;; BIBLIOGRAPHY:
;; https://github.com/schemedoc/bibliography

(import (scheme write)
        (srfi srfi-1)
        (srfi srfi-60))

(define (parse file)
  (call-with-input-file file
    (lambda (port) (read port))))

;;;; UTILS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (emit . args)
  (display (apply string-append (cons "\t" args)))
  (newline))
(define (tag tagname)
  (display (string-append tagname ":"))
  (newline))
(define (comment text)
  (display (string-append "## " text))
  (newline))


;;;; BASIC PROGRAM BLOCKS FOR SPIKE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (prologue)
  " Prefixes the output code with the needed declarations (_start, .text...).
  !! Spike specific behavior: Initializes Stack Pointer to 0x80001000"
  (emit  ".text")
  (emit  ".globl _start")
  (tag "_start")
  (emit  "li sp, 0x80001000")
  (emit ""))

(define (epilogue)
  "Finish with Spike specific functions
  !! Spike specific behavior: Mostly everything"
  (emit "")
  (tag "exit")
  (emit  "move a1, a0")
  (emit  "li a0, 93")
  (emit  "jal syscall")
  (tag "1")
  (emit  "j 1b") ; Wait indefinitely until syscall returns

  (tag "syscall")
  (emit "la t0, syscall_buffer")
  (emit "sw a0, (t0)")
  (emit "sw a1, 8(t0)")
  (emit "sw a2, 16(t0)")
  (emit "sw a3, 24(t0)")
  (emit "sw a4, 32(t0)")
  (emit "sw a5, 40(t0)")
  (emit "sw a6, 48(t0)")
  (emit "sw a7, 56(t0)")
  (emit "la t1, tohost")
  (emit "sw t0, (t1)")
  (emit "la t1, fromhost")
  (tag "1")
  (emit "lw t2, (t1)")
  (emit "beqz t2, 1b")
  (emit "sw zero, (t1)")
  (emit "lw a0, (t0)")
  (emit "ret"))

(define (data)
  "Define data block
  !! Spike specific behavior: fromhost/tohost and syscall buffer"
  (emit "")
  (comment "Mutable data block")
  (emit ".data")
  (emit ".align 6")
  (emit ".global tohost")
  (tag "tohost")
  (emit ".dword 0")
  (emit ".align 6")
  (emit ".global fromhost")
  (tag "fromhost")
  (emit ".dword 0")
  (tag "syscall_buffer")
  (emit ".skip 64"))

(define (static-data const-pool)
  (emit "")
  (comment "Static data block")
  (emit ".align 4")
  (emit ".rodata")
  (emit-constants const-pool))

(define (_exit)
  (emit "li a0, 0")
  (emit "j exit"))




;;;; STDLIB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; INLINED
(define (push reg)
  (emit "addi sp, sp, -4")
  (emit "sw " reg ",(sp)"))

(define (pop reg)
  (emit "lw " reg ",(sp)")
  (emit "addi sp,sp,4"))

; Integers can be added directly without affecting the value
; Doesn't check!
(define (intadd)
  (emit "add a0, a0, a1"))


;;;; COMPILATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NOTE:
;;;; We may want to make it more similar to this:
;;;; https://www.more-magic.net/posts/internals-data-representation.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; All our values are 32 bit and their types are encoded in the lower bits
;;;; like so:
;;;;                  |   29 bit   |type
;;;;                  -------------+----
;;;;  - Integer:      SV...VVVVVVVVV|000
;;;;  - Real:         SV...VVVVVVVVV|001 (it's not a goal to implement them)
;;;;  - Pair:         AA...AAAAAAAAA|010
;;;;  - Vector:       AA...AAAAAAAAA|011
;;;;  - Procedure:    AA...AAAAAAAAA|100
;;;;  - Boolean:      VV...VVVVVVVVV|101
;;;;  - Char:         00...0VVVVVVVV|110
;;;;                       |<- 8b->|
;;;;
;;;; Being the type encoded in low values keeps the sign correctly set so we
;;;; can compare lower or higher than zero or the size of two numbers of the
;;;; same type with no issues.
;;;; Our 29 bit addresses can point to 2^29 Bytes in memory, that means we can
;;;; address 8 * 2^29 bytes = 2^3 * 2^29 = 2^32 = 4 Gbit = 512 MB, more than
;;;; enough for our targets.
;when (not (memq const (map cons const-pool)))

;;; CONSTANTS
(define const-pool '())

(define (found-constant const)
  (let* ((found (assoc const const-pool))
         (label (if found
                    (cdr found)
                    (string-append "const-" (number->string (length const-pool))))))
    (when (not found)
      (set! const-pool (alist-cons const label const-pool)))
    (emit "lw a0, " label)))

(define (emit-constants const-pool)
  (for-each
    (lambda (const)
      (tag (cdr const))
      (emit ".word " (const->data (car const))))
    const-pool))

(define (const->data value)
  (cond
    ((integer? value) (int->binstring value))
    ((real?    value) (error "Not implemented yet"))))

; Does this make any sense?
; TODO: Make it use SRFI-60
(define (int->binstring i)
  (define MAXINT (expt 2 28)) ; All numbers are signed
  (when (or (< i (- MAXINT)) (>= i MAXINT))
    (error "Integer is too large or too small"))
  (let ((textblock (make-string 34 #\0))
        (numstring (number->string (abs i) 2)))
    (string-set! textblock 0 #\0) ; Set 0b at the beginning
    (string-set! textblock 1 #\b) ; Set 0b at the beginning
    (when (< i 0)
      (string-set! textblock 2 #\1))
    (let loop ((cur 30)
               (rem numstring))
      (when (> (string-length rem) 0)
        (string-set!
          textblock
          cur
          (string-ref rem (- (string-length rem) 1)))
        (loop (- cur 1)
              (substring rem 0 (- (string-length rem) 1)))))
    (string-fill! textblock #\0 31 34)
    textblock))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define (compile-funcall ex)
  (for-each compile (cdr ex)))



(define (compile ex)
  (cond
    ((integer?  ex) (found-constant ex))
    ((list?     ex) (compile-funcall ex))))

(define (compile-to-file ex)
  (prologue)
  (comment "Program starts here")
  (compile ex)
  (_exit)
  (epilogue)
  (static-data const-pool)
  (data))

(compile-to-file '(+ 1 1 1 1))

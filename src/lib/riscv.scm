(define-library (lib riscv)
  (import (scheme base)
          (scheme write)
          (srfi srfi-60))
  (export emit tag comment push pop static-data load-word)
  (begin
    ;; BASIC COMMANDS
    (define (emit . args)
      (display (apply string-append (cons "\t" args)))
      (newline))
    (define (tag tagname)
      (display (string-append tagname ":"))
      (newline))
    (define (comment text)
      (display (string-append "## " text))
      (newline))
    (define (load-word label)
      (emit "lw a0, " label))

    (define (push reg)
      (emit "addi sp, sp, -4")
      (emit "sw " reg ",(sp)"))
    (define (pop reg)
      (emit "lw " reg ",(sp)")
      (emit "addi sp,sp,4"))


    ;; DATA REPRESENTATION
    ;; See ../doc/02_immediate_values.md
    (define (const->data value)
      (cond
        ((integer? value) (int->binstring value))
        ((boolean? value) (bool->binstring value))
        ((real?    value) (error "Not implemented yet"))))

    (define MAX_INTEGER  (- (ash #b1 29) 1))
    (define (int->binstring int)
      (when (> (abs int) MAX_INTEGER)
        (error (string-append "Integer exceeds the max size: "
                              (number->string int))))
      (string-append
        "0b"
        (let ((str (number->string (+ int 1 MAX_INTEGER) 2)))
          (if (< 0 int)
              (substring str 1)
              str))
        "000"))

    (define (bool->binstring bool)
      (if bool
          "0b00000000000000000000000000000101"
          "0b11111111111111111111111111111101"))


    ;; STATIC DATA BLOCK IN THE ASSEMBLY
    (define (emit-constants const-pool)
      (for-each
        (lambda (const)
          (tag (cdr const))
          (emit ".word " (const->data (car const))))
        const-pool))

    (define (static-data const-pool)
      (emit "")
      (comment "Static data block")
      (emit ".align 4")
      (emit ".section .rodata")
      (emit-constants const-pool))))

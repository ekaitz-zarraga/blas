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


    ;; Stdlib
    (define (_null?)
      (emit "lw a1, NIL")
      (emit "beq a0, a1, 1f")
      (emit "lw a0,FALSE")
      (emit "j 2f")
      (tag "1")
      (emit "lw a0,TRUE")
      (tag "2"))
    (define (_boolean?)
      (emit "andi a0, a0, 0xF")
      (emit "li a1, 0x6")
      (emit "beq a0, a1, 1f")
      (emit "lw a0,FALSE")
      (emit "j 2f")
      (tag "1")
      (emit "lw a0,TRUE")
      (tag "2"))
    (define (_integer?)
      (emit "andi a0, a0, 0x1")
      (emit "beqz a0, 1f")
      (emit "lw a0,TRUE")
      (emit "j 2f")
      (tag "1")
      (emit "lw a0,FALSE")
      (tag "2"))
    (define (_pointer?)
      (emit "andi a0, a0, 0x3")
      (emit "beqz a0, 1f")
      (emit "lw a0,FALSE")
      (emit "j 2f")
      (tag "1")
      (emit "lw a0,TRUE")
      (tag "2"))


    ; TODO
    (define (_string?)
      (emit "mv s0, a0")
      (_pointer?)
      (emit "lw a1, TRUE")
      (emit "bne a0, a0, 1f")
      (emit "lb a0,(a0)")
      (emit "andi a0,a0, 0xF")
      ;TODO CHECK IF IT'S STRING
      (tag "1"))


    ;; DATA REPRESENTATION
    ;; See ../doc/02_data_representation.md
    (define (const->data value)
      (cond
        ((integer? value) (int->binstring  value))
        ((boolean? value) (bool->binstring value))
        ((null?    value) (nil->binstring))
        ((real?    value) (error "Not implemented yet"))))

    (define MAX_INTEGER  (- (ash #b1 30) 1))
    (define (int->binstring int)
      (when (> (abs int) MAX_INTEGER)
        (error (string-append "Integer exceeds the max size: "
                              (number->string int))))
      (string-append
        "0b"
        (let ((str (number->string (+ int 1 (* 2 MAX_INTEGER)) 2)))
          (if (< 0 int)
              (substring str 1)
              str))
        "1"))


    (define (bool->binstring bool)
      (if bool
          "0b00000000000000000000000000000110"
          "0b11111111111111111111111111110110"))

    (define (nil->binstring)
      "0b00000000000000000000000000001110")


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

(define-library (lib riscv-spike)
  (import (scheme base)
          (lib riscv))
  (export prologue epilogue data static-data finish)
  (begin
    ;;;; BASIC PROGRAM BLOCKS FOR SPIKE
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (prologue)
      " Prefixes the output code with the needed declarations
      (_start, .text...).
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

    (define (finish)
      (emit "li a0, 0")
      (emit "j exit"))

    ))

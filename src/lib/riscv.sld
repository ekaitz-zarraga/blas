(define-library (lib riscv)
  (import (scheme base)
          (scheme write)
          (srfi srfi-60))
  (export
    emit
    label
    comment
    push
    pop
    static-data
    load-word
    =?
    intadd
    load-true
    load-false
    load-nil)
  (include "riscv.scm"))

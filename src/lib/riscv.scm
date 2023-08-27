;; BASIC COMMANDS
(define (emit . args)
  (display (apply string-append (cons "\t" args)))
  (newline))
(define (label labelname)
  (display (string-append labelname ":"))
  (newline))
(define (comment text)
  (display (string-append "## " text))
  (newline))
(define (load-word label)
  (emit "lw a0, " label))

;; This could be also a `li a0, ...`
(define (load-true)
  (load-word "TRUE"))
(define (load-false)
  (load-word "FALSE"))
(define (load-nil)
  (load-word "NIL"))

;; Inlined
(define (push reg)
  (emit "addi sp,sp,-4")
  (emit "sw " reg ",(sp)"))
(define (pop reg)
  (emit "lw " reg ",(sp)")
  (emit "addi sp,sp,4"))
(define (=?)
  "Checks if a0 and b0 are equal and sets a0 to TRUE or FALSE depending on the
  result"
  (emit "beq a0,a1,1f")
  (emit "lw a0,FALSE")
  (emit "j 2f")
  (label "1")
  (emit "lw a0,TRUE")
  (label "2"))

;; stdlib
(define (predicate fname mask labelname)
  (lambda _
    (label fname)
    (emit "andi a0,a0," mask)
    (emit "li a1," labelname)
    (=?)
    (emit "ret")))
(define _boolean?  (predicate "boolean?" "0xF"  "0b0110"))
(define _integer?  (predicate "integer?" "0x1"  "0b1"))
(define _pointer?  (predicate "pointer?" "0x3"  "0b000"))
(define _char?     (predicate "char?"    "0xFF" "0b00001010"))
(define (_null?)
  (label "null?")
  (emit "lw a1,NIL")
  (=?)
  (emit "ret"))
(define (_string?)
  (label "string?")
  (emit "mv s0,a0")
  (_pointer?)
  (emit "lw a1,TRUE")
  (emit "bne a0,a0,1f")
  (emit "lb a0,(a0)")
  (emit "andi a0,a0,0xF")
  ;TODO CHECK IF IT'S STRING
  (label "1"))

(define (intadd)
  "Uses a0 and a1. Assumes integers in both registers."
  (emit "srai a0,a0,1")   ; Convert to raw integer
  (emit "srai a1,a1,1")   ; Convert to raw integer
  (emit "add  a0,a0,a1")  ; Add raw integers
  (emit "slli a0,a0,1")   ; Tag: make space for the tag
  (emit "addi a0,a0,1"))  ; Tag: add the tag



; SPECIAL FORMS
(define (compile-if args)
  (when (let ((l (length args)))
          (or (< 3 l) (> 2 l)))
    (error "Malformed if: (if CONDITION BODY [ELSE-BODY])"))

  (let ((condition (first args))
        (body      (second args))
        (else-body (if (= 3 (length args)) (third args) #f)))

    (compile condition)
    (emit "lw a1,FALSE")
    (emit "beq a0,a1,1f")
    (compile body)
    (emit "j 2f")
    (tag "1")
    (compile else-body)
    (tag "2")))




;; DATA REPRESENTATION
;; See ../doc/02_data_representation.md
(define (const->data value)
  (cond
    ((integer? value) (int->binstring  value))
    ((boolean? value) (bool->binstring value))
    ((char?    value) (char->binstring value))
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

(define MAX_CHAR (- (ash #b1 16) 1))
(define (char->binstring char)
  (let ((num (char->integer char)))
    (when (> num MAX_CHAR)
      (error "Character out of range"))
    (string-append "0b"
      (substring
        (number->string
          (+ (ash 1 32)   ; Add zeros on the top, needs to be substringed to 1-N
             (ash num 8)
             10)          ; Character tag
          2)
        1))))

(define (nil->binstring)
  "0b00000000000000000000000000001110")


;; STATIC DATA BLOCK IN THE ASSEMBLY
(define (emit-constants const-pool)
  (for-each
    (lambda (const)
      (label (cdr const))
      (emit ".word " (const->data (car const))))
    const-pool))

(define (static-data const-pool)
  (emit "")
  (comment "Static data block")
  (emit ".align 4")
  (emit ".section .rodata")
  (emit-constants const-pool))

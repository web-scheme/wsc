#|* Datum Parser
  *
  * Define the complete WebScheme datum parser,
  * used to implement `read`,
  * capable of scanning both s-expressions and t-expressions.
  *
  * @see https://web-scheme.org/r7rs-small.pdf#subsection.7.1.2
|#
#!sweet

; TODO: Clean up imports.
import (scheme base)
import (chibi)
import (srfi 115)

;; Ultimate flex.
define-syntax λ
  syntax-rules ()
    (λ parameters body ...)
      lambda parameters body ...

include "scanner.scm"
include "bnf-grammar.scm"

;; A global hash table in which to store labels.
;; TODO: Fix this (need scope, recursion).
define label-map make-hash-table(eqv?)

;; The datum grammar:
define datum-parser
  bnf-grammar
    'datum ::= (('identifier))
            \\ (('boolean))
            \\ (('number))
            \\ (('character))
            \\ (('string))
            \\ (('open-list 'data 'close)                     => (λ (open data close) data))
            \\ (('open-vector 'data 'close)                   => (λ (open data close) list->vector(data)))
            \\ (('open-bytevector 'bytes 'close)              => (λ (open data close) apply(bytevector data)))
            \\ (('open-list 'datum 'data 'dot 'datum 'close)  => (λ (open head data dot tail close)
                                                                   cons(head append(data tail))))
            \\ (('label-set 'datum)                           => (λ (label datum)
                                                                   hash-table-set!(label-map label datum)
                                                                   datum))
            \\ (('label)                                      => (λ (label)
                                                                   hash-table-ref(label-map label
                                                                     (λ () error("undefined label" label)))))
            \\ (('quote 'datum)                               => list)
            \\ (('quasiquote 'datum)                          => list)
            \\ (('unquote 'datum)                             => list)
            \\ (('unquote-splicing 'datum)                    => list)
    'data  ::= (())
            \\ (('datum 'data)                                => cons)
    'bytes ::= (())
            \\ (('number 'bytes)                              => (λ (number bytes)
                                                                   (if {exact-integer?(number) and {0 <= number <= 255}}
                                                                     cons(number bytes)
                                                                     error("invalid byte in bytevector literal" number))))
display(scanner("\"hey\""))
newline()
define parse(port)
  let* ((stack '()))
    define consume(token l-value next-scanner)
      display(token)
      display(" => ")
      display(l-value)
      display(" @ ")
      display(next-scanner)
      newline()
      set!(stack cons(token stack))
      call-with-values((λ () next-scanner(port)) consume)
    call-with-values((λ () scanner(port)) consume)
parse("#u8(")

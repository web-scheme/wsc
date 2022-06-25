#|* Token Scanner
  *
  * @see https://web-scheme.org/r7rs-small.pdf#subsection.7.1.1
|#
#!sweet

; TODO: Figure out how to get this in a library, clean up imports.
import (scheme base)
import (chibi)
import (srfi 115)

include "lexical-rules.scm"

;; Booleans:
define true-re   `(: "#t" (? "rue"))
define false-re  `(: "#f" (? "alse"))

;;; Numbers:
define digit-2-re      '(/ "01")
define digit-8-re      '(/ "07")
define digit-10-re     '(/ "09")
define digit-16-re     '(/ "09af")
define radix-2-re      "#b"
define radix-8-re      "#o"
define radix-10-re     '(? "#d")
define radix-16-re     "#x"
define sign-re         '("+-")
define infnan-re       '(or "inf.0" "nan.0")
define exactness-re    '(? (: "#" ("ie")))
define prefix-re(radix)
  `(or (: ,radix ,exactness-re)
       (: ,exactness-re ,radix))
define suffix-re
  `(? (: "e" (? ,sign-re) (+ ,digit-10-re)))
define decimal-10-re
  `(or (: (+ ,digit-10-re) ,suffix-re)
       (: "." (+ ,digit-10-re) ,suffix-re)
       (: (+ ,digit-10-re) "." (? (+ ,digit-10-re)) ,suffix-re))
define ureal-re(digit)
  define integer-or-fraction
    `((+ ,digit)
      (: (+ ,digit) "/" (+ ,digit)))
  cons 'or
    ;; Special-case: base-10 supports decimal notation.
    if eq?(digit digit-10-re)
      cons(decimal-10-re integer-or-fraction)
      integer-or-fraction
define real-re(digit)
  `(or (: (? ,sign-re) ,ureal-re(digit))
       (: ,sign-re ,infnan-re))
define complex-re(digit)
  `(or ,real-re(digit)
       (: ,real-re(digit) "@" ,real-re(digit))
       (: (? ,real-re(digit)) ,sign-re (? (or ,ureal-re(digit) ,infnan-re)) "i"))
define num-re(radix digit)
  `(: ,prefix-re(radix) ,complex-re(digit))
define number-re
  `(or ,num-re(radix-2-re digit-2-re)
       ,num-re(radix-8-re digit-8-re)
       ,num-re(radix-10-re digit-10-re)
       ,num-re(radix-16-re digit-16-re))

define initial-re     `(or alphabetic ("!$%&*/:<=>?@^_~"))
define subsequent-re  `(or ,initial-re ,digit-10-re ("+-.@"))
; TODO: Add support for |verbatim identifiers| and peculiar identifiers.
define identifier-re  `(: ,initial-re (* ,subsequent-re))

define character-re
  `(: "#\\"
      (or graphic
          "alarm"
          "backspace"
          "delete"
          "escape"
          "newline"
          "null"
          "return"
          "space"
          "tab"
          (: "x" (+ ,digit-16-re))))
; TODO: Implement (just placeholders for now).
define parse-character(text) #\h

define string-re
  `(: "\""
      (* (or (~ ("\"\\"))
             (: "\\" any)))
      "\"")
; TODO: Implement (just placeholders for now).
define parse-string(text) text

define label-re     `(: "#" (+ ,digit-10-re) "#")
define label-set-re `(: "#" (+ ,digit-10-re) "=")
define parse-label(text)
  ;; TODO: See if we can avoid the copy.
  string->number(string-copy(text 1 {string-length(text) - 1))

define intraline-whitespace-re '(+ ("\t "))
define line-ending-re          '(or ("\n\r") "\r\n")

;;* Return a scanner function
define nested-comment-scanner(depth outer-scanner)
  lexical-rules
    "#|"    '() @ nested-comment-scanner({depth + 1} outer-scanner)
    "|#"    '() @ (if {depth = 0}
                    outer-scanner
                    nested-comment-scanner({depth - 1} outer-scanner))
    'any    '()

define scanner
  letrec
    \\
      ;; The case-sensitive s-expression scanner:
      s-case-scanner
        lexical-rules
          "#!fold-case"                             @ nocase-scanner
          "#!no-fold-case"        
          "#!sweet"            '!sweet              @ sweet-scanner
          "#!curly-infix"      '!sweet              @ sweet-scanner  ; TODO: Support semi-sweet.
          "#!no-sweet"            
          "#|"                                      @ nested-comment-scanner(0 s-case-scanner)
          "#;"                                      @ datum-comment-scanner
          `(: ";" (* nonl))       
          "("                  'open-list
          "#("                 'open-vector
          "#u8("               'open-bytevector
          ")"                  'close
          "."                  'dot
          "'"                  'quote               'quote
          "`"                  'quasiquote          'quasiquote
          ","                  'unquote             'unquote
          ",@"                 'unquote-splicing    'unquote-splicing
          identifier-re        'identifier          => string->symbol
          true-re              'boolean             #t
          false-re             'boolean             #f
          number-re            'number              => string->number
          character-re         'character           => parse-character
          string-re            'string              => parse-string
          label-set-re         'label-set           => parse-label
          label-re             'label               => parse-label
    s-case-scanner

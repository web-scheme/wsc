#|* Lexical Analysis 
  *
  * Define the WebScheme scanner,
  * capable of scanning both "vanilla" s-expressions and "sweet" t-expressions.
  *
  * @see https://web-scheme.org/r7rs-small.pdf#subsection.7.1.1
  * @see https://srfi.schemers.org/srfi-110/srfi-110.html
|#
#!sweet

import (chibi)       ; TODO: Remove.
import (scheme base)
import (srfi 115)    ; TODO: `(scheme regex)` when full R7RS.
import (srfi 26)     ; cut / cute

;; Ultimate flex.
define-syntax λ
  syntax-rules ()
    (λ parameters body ...)
      lambda parameters body ...

define true-re   `(: "#t" (? "rue"))
define false-re  `(: "#f" (? "alse"))

;;; Basic numerical elements.
define digit-2-re     '(/ "01")
define digit-8-re     '(/ "07")
define digit-10-re    '(/ "09")
define digit-16-re    '(/ "09af")
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

define intraline-whitespace-re '(+ ("\t "))
define line-ending-re          '(or ("\n\r") "\r\n")

define string-re
  `(: "\""
      (* (or (~ ("\"\\"))
             (: "\\" any)))
      "\"")

;define-library (s-lang lexical-rules)
;  import (scheme base)
;  export lexical-rules
;  <* begin

define regexp-read(re port)
  regexp-matches(re port)
define regexp-match-submatch-fields(match)
  define aux(i fields)
    if {i > 100}
      fields
      aux
        {i + 1}
        if regexp-match-submatch(match i)
          cons(i fields)
          fields
  aux(1 '())

;; Re-usable definitions of noop functions used in scanners:
define noop-l-value-fab(match) '()
define noop-scanner-fab() '()

define-syntax *lexical-rules
  syntax-rules (=> @)
    ;; The most basic rule simply returns a static token with no l-value.
    (*lexical-rules (patterns ...) (actions ...) (pattern token) rules ...)
      *lexical-rules (patterns ... pattern)
        (actions ... (token noop-l-value-fab noop-scanner-fab))
        rules \\ ...
    ;; Supply a uniparametric function `l-value-fab`,
    ;; which is passed the text of the match, to return an l-value.
    (*lexical-rules (patterns ...) (actions ...) (pattern token => l-value-fab) rules ...)
      *lexical-rules (patterns ... pattern)
        (actions ... (token l-value-fab noop-scanner-fab))
        rules \\ ...
    ;; Supply a lazily-evaluated expression `scanner-expr`
    ;; which evaluates to a new scanner to be used for subsequent input (state transition).
    (*lexical-rules (patterns ...) (actions ...) (pattern token @ scanner-expr) rules ...)
      *lexical-rules (patterns ... pattern)
        (actions ... (token noop-l-value-fab (λ () scanner-expr)))
        rules \\ ...
    (*lexical-rules sres actions bad-rule rules ...)
      syntax-error("Malformed lexical rule" bad-rule)
    (*lexical-rules (patterns ...) ((*token *l-value-fab *scanner-fab) ...))
      let ((big-regex  regexp(`(or ($ ,patterns) ...)))
           (actions    vector(values(*token *l-value-fab *scanner-fab) ...)))
        λ (port)
          letrec ((match           regexp-read(big-regex port))
                  (submatch-field  car(regexp-match-submatch-fields(match)))
                  (match-text      regexp-match-submatch(match submatch-field)))
            let-values (((token l-value-fab scanner-fab)
                         vector-ref(actions {submatch-field - 1})))
              let ((new-scanner scanner-fab()))
                if null?(new-scanner)
                  `(,token ,l-value-fab(match-text))
                  ;; TODO: Transition state.
                  `(,token ,l-value-fab(match-text))

define-syntax lexical-rules
  syntax-rules ()
    (lexical-rules rules ...)
      *lexical-rules () () rules ...
;*>

define nested-comment-scanner(depth outer-scanner)
  lexical-rules
    "#|"    '() @ nested-comment-scanner({depth + 1} outer-scanner)
    "|#"    '() @ (if {depth = 0}
                    outer-scanner
                    nested-comment-scanner({depth - 1} outer-scanner))
    'any    '()

; TODO: Implement (just placeholders for now).
define parse-character(text) #\h
define parse-string(text) text

letrec
  \\
    scanner
      lexical-rules
        "#!fold-case"        '()           @ nocase-scanner
        "#!no-fold-case"     '()           ; Case is already unfolded.
        "#!sweet"            '!sweet       @ sweet-scanner
        "#!curly-infix"      '!sweet       @ sweet-scanner    ; TODO: Support semi-sweet.
        "#!no-sweet"         '!no-sweet
        "#|"                 '()           @ nested-comment-scanner(0 scanner)
        "#;"                 '()           @ datum-comment-scanner
        `(: ";" (* nonl))    '()           ; Simply discard line comments.
        "("                  'open-list
        "#("                 'open-vector
        "#u8("               'open-bytevector
        ")"                  'close
        "'"                  'quote
        "`"                  'quasiquote
        ","                  'unquote
        ",@"                 'unquote-splicing
        "."                  'dot
        identifier-re        'identifier   => string->symbol
        true-re              'boolean      => (λ (_) #t)
        false-re             'boolean      => (λ (_) #f)
        number-re            'number       => string->number
        character-re         'character    => parse-character
        string-re            'string       => parse-string
  display(scanner("\"hey\""))
  newline()

#|
define compile(rules)
  ;; Construct a monster regex of the form `(or ($ <pattern>) ...)`
  ;; where all submatches are mutually exclusive.
  ;; Pattern sets should be comprehensive i.e. return a match for any input,
  ;; so exactly 1 numbered submatch matches every time.
  define re regexp(`(or . ,map((λ (rule) `($ ,car(rule))) rules)))
  ;; Organize the actions into a vector for random access.
  ;; For each matched submatch, call the corresponding action, passing it:
  ;;   - the current set of rules
  ;;   - the current compiled scan function itself
  define actions (vector . (map cdr rules))
  ;; The compiled scan function reads the next token from a port and returns:
  ;;   - the next token
  ;;   - the next scan function
  define scan(port)
    vector-ref(actions
               car(regexp-match-submatches(regexp-read(re port))))
      rules \\ scan
  scan

;; Tokens that scan in both sweet and vanilla mode:
define ambigustatory-patterns
  list
    cons "#!fold-case"
      λ (rules scan)
        values '() scan
    cons "#!no-fold-case"
      cut State.fold-case.set! <> #f

  ;"#!fold-case" .
  ;  λ ()
  ;"#!no-fold-case" .
  ;  λ ()
  ;'(: (look-behind bol) "#!sweet" (look-ahead eol)) .
  ;  λ ()
  ;'(: (look-behind bol) "#!no-sweet" (look-ahead eol)) .
  ;  λ ()
  ;intraline-whitespace .
  ;  λ ()
  ;line-ending .
  ;  λ ()
  ;line-comment .
  ;  λ ()
|#

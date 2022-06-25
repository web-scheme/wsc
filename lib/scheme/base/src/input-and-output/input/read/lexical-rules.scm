#|* Lexical Analyzer Generator
  *
  * A macro that transforms a series of rules of the form
  *     pattern [ token ] [ => l-value-fab ] [ @ scanner-expr ]
  * into a scanner function.
|#
#!sweet

;; TODO: Try to get this incorporated in R7RS-large.
;; Matching function that reads from a port.
define regexp-read(re port)
  regexp-matches(re port)
;; TODO: Try to get this incorporated in R7RS-large.
;; Return the list of all submatch fields (numeric or named) that actually matched.
;; Returns the field names (or numbers) rather than the matched text.
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

define regexp-submatch-number(match)
  ; TODO: Test assumption that only 1 thing ever matches (peculiar identifiers?).
  car(regexp-match-submatch-fields(match))

;; Re-usable definitions of noop functions used in scanners:
define noop-l-value-fab(match) '()
define noop-scanner-fab() '()

define-syntax *lexical-rules
  syntax-rules (=> @)
    ;; The most basic rule simply returns a static token with no l-value.
    (*lexical-rules (*patterns ...) (*actions ...) (*pattern *token) *tail ...)
      *lexical-rules (*patterns ... *pattern)
        (*actions ... (*token noop-l-value-fab noop-scanner-fab))
        *tail \\ ...
    ;; Static l-value.
    (*lexical-rules (*patterns ...) (*actions ...) (*pattern *token *l-value) *tail ...)
      *lexical-rules (*patterns ... *pattern)
        (*actions ... (*token (λ (text) *l-value) noop-scanner-fab))
        *tail \\ ...
    ;; Supply a uniparametric function `l-value-fab`,
    ;; which is passed the text of the match, to return an l-value.
    (*lexical-rules (*patterns ...) (*actions ...) (*pattern *token => *l-value-fab) *tail ...)
      *lexical-rules (*patterns ... *pattern)
        (*actions ... (*token *l-value-fab noop-scanner-fab))
        *tail \\ ...
    ;; Supply a lazily-evaluated expression `scanner-expr`
    ;; which evaluates to a new scanner to be used for subsequent input (state transition).
    (*lexical-rules (*patterns ...) (*actions ...) (*pattern *token @ *scanner-expr) *tail ...)
      *lexical-rules (*patterns ... *pattern)
        (*actions ... (*token noop-l-value-fab (λ () *scanner-expr)))
        *tail \\ ...
    ;; Syntactic sugar for new scanner transition with null token.
    (*lexical-rules (*patterns ...) (*actions ...) (*pattern @ *scanner-expr) *tail ...)
      *lexical-rules (*patterns ... *pattern)
        (*actions ... ('() noop-l-value-fab (λ () *scanner-expr)))
        *tail \\ ...
    ;; Assume an unstructured rule is just a pattern to be discarded.
    (*lexical-rules (*patterns ...) (*actions ...) *pattern *tail ...)
      *lexical-rules (*patterns ... *pattern)
        (*actions ... ('() noop-l-value-fab noop-scanner-fab))
        *tail \\ ...
    ;; The final transformation.
    (*lexical-rules (*patterns ...) ((*token *l-value-fab *scanner-fab) ...))
      letrec (;; Compile all the individual patterns into a super regex,
              ;; giving each pattern a numbered submatch index
              ;; (1-indexed, since submatch 0 is always whole match).
              (super-regex regexp(`(or ($ ,*patterns) ...)))
              ; TODO: See if this works instead of above:
              ; (super-regex regexp('(or ($ *patterns) ...)))
              ;; Combine all action information into a vector
              ;; such that pattern `i` corresponds to action `i - 1`.
              (actions vector(values(*token *l-value-fab *scanner-fab) ...))
              (scanner
                (λ (port)
                  (let* ((match  regexp-read(super-regex port))
                         (field  regexp-submatch-number(match))
                         (text   regexp-match-submatch(match field)))
                     (let-values (((token l-value-fab scanner-fab)
                                   vector-ref(actions {field - 1})))
                        (let* ((new-scanner scanner-fab())
                               (next-scanner (if null?(new-scanner) scanner new-scanner)))
                           (if null?(token)
                              ;; If the token is null, try again from the same port.
                              next-scanner(port)
                              ;; Otherwise, return to the parser.
                              values(token l-value-fab(text) next-scanner))))))))
        scanner

;;* Define a scanner:
;;* a function which takes a textual input port,
;;* consumes a single token,
;;* and returns 3 values:
;;*   - the token symbol
;;*   - the l-value
;;*   - the scanner for the next token
;;*
;;* The parser should call the returned scanner for the next token,
;;* and so forth, always passing just a port.
;;*
;;* A scanner is defined by a list of rules.
;;* Each rule can take 1 of 3 forms:
;;*   - `(pattern)`
;;*     When `pattern` matches, recurse (discard the match).
;;*   - `(pattern @ new-scaner)`
;;*     Discard the match but recurse with the new scanner.
;;*     `new-scanner` is an expression to be lazily evaluated,
;;*     yielding another scanner defined using `lexical-rules`.
;;*     You may want to use `letrec` for this.
;;*   - `(pattern token)`
;;*     On match, return `token`, a null l-value, and the same scanner.
;;*   - `(pattern token @ new-scaner)`
;;*     On match, return `token`, a null l-value, and the new scanner.
;;*   - `(pattern token l-value)`
;;*     On match, return `token`, the static `l-value`, same scanner.
;;*   - `(pattern token => recipient)`
;;*     On match, return `token`, and pass the matched text to `recipient`,
;;*     returning that result for the l-value. Same scanner.
define-syntax lexical-rules
  syntax-rules ()
    (lexical-rules rules ...)
      *lexical-rules () () rules ...

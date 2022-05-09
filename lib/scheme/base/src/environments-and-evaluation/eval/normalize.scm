#!sweet

;;;; NOT PORTABLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-modules (language tree-il))
;; `match`
(use-modules (ice-9 match))
;; Basic hash tables
(use-modules (srfi srfi-69))
;; `cut` and `cute`
(use-modules (srfi srfi-26))

;; Defined in R7Rs-small:
define all(objs)
  if null?(objs) #t and(car(objs) all(cdr(objs)))
define (symbol=? . objs)
  and
    all(map(symbol? objs))
    apply(eqv? objs)

;; SRFI 125
define hash-table-for-each(proc hash-table)
  hash-table-walk(hash-table proc)
define hash-table-map!(proc hash-table)
  define aux(keys)
    unless null?(keys)
      let ((key car(keys)))
        hash-table-update!(hash-table key (cut proc key <>))
        aux(cdr(keys))
  aux(hash-table-keys(hash-table))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

include "derived-expression-types.scm"

;; Merge 2 bindings 
define append-bindings(prior next)
  ;; When `prior` is null, treat it like an empty binding set.
  ;; When `next` is false, fail fast.
  if {null?(prior) or not(next)}
    next
    begin
      hash-table-for-each
        lambda (key value)
          hash-table-update!/default(prior key (cut append <> value) '())
        next
      prior

define encapsulate-bindings(bindings)
  hash-table-map!
    lambda (key value) list(value)
    bindings
  bindings

;;; @param ellipsis   Ellipsis symbol, usually `...`.
;;; @param literals   List of literal symbols in the syntax rule patterns.
;;; @param pattern    Pattern to match.
;;; @param expression Expression to match against.
;;; @return A nested association list TODO,
;;;         or `#f` if the pattern fails to match.
define syntax-rule-match(ellipsis literals pattern expression)
  ;; If `ellipsis` or `underscore` appear in the literals, they are matched as literals.
  define ellipsis-enabled not(member(ellipsis literals))
  define underscore-enabled not(member('_ literals))
  define aux(pattern expression ellipsis-bindings)
    if member(pattern literals)
      if symbol=?(pattern expression)
        (make-hash-table)  ; Literals do not create bindings; return empty.
        #f                 ; If it does not match, the whole pattern does not match.
      if symbol?(pattern)
        alist->hash-table(list(cons(pattern list(expression))))  ; A single pattern variable always matches.
        if pair?(pattern)
          if pair?(expression)
            let ((bindings aux(car(pattern) car(expression) '())))
              if bindings
                let ((pattern-tail cdr(pattern))
                     (expression-tail cdr(expression)))
                  if pair?(pattern-tail)
                    if {ellipsis-enabled and symbol=?(car(pattern-tail) ellipsis)}
                      if {length(pattern-tail) > length(expression-tail)}
                        ;; Exhausted all possible variable (ellipsis) bindings.
                        append-bindings
                          encapsulate-bindings
                            append-bindings(ellipsis-bindings bindings)
                          aux(cdr(pattern-tail) expression-tail '())
                        aux
                          pattern
                          expression-tail
                          append-bindings(ellipsis-bindings bindings)
                      append-bindings(bindings aux(pattern-tail expression-tail '()))
                    append-bindings(bindings aux(pattern-tail expression-tail '()))
                #f
            if and(ellipsis-enabled
                   null?(expression)
                   pair?(cdr(pattern))
                   symbol=?(cadr(pattern) ellipsis))
              aux(cddr(pattern) expression '())
              #f
          if {vector?(pattern) and vector?(expression)}
            aux(vector->list(pattern) vector->list(expression) '())
            if equal?(pattern expression)  ; TODO: Check that `pattern` is "constant" (as in report).
              (make-hash-table)
              #f
  aux(pattern expression '())

define syntax-rule-expand(ellipsis template bindings)
  ;; HF for easier recursion:
  define aux(template bindings)
    if null?(bindings)
      values(template '())
      if {pair?(car(bindings)) and symbol=?(template caar(bindings))}
        values(cdar(bindings) cdr(bindings))
        if pair?(template)
          let ((template-tail cdr(template)))
            if {pair?(template-tail) and symbol=?(car(template-tail) ellipsis)}
              if null?(car(bindings))
                aux(cdr(template-tail) cdr(bindings))
                if list?(car(bindings))
                  let*-values (((car-expanded car-bindings)
                                aux(car(template) car(bindings)))
                               ((cdr-expanded cdr-bindings)
                                aux(template cons(car-bindings cdr(bindings)))))
                    values(cons(car-expanded cdr-expanded) cdr-bindings)
                  raise("Ellipses mismatch!")
              let*-values (((car-expanded car-bindings) aux(car(template) bindings))
                           ((cdr-expanded cdr-bindings) aux(template-tail car-bindings)))
                values(cons(car-expanded cdr-expanded) cdr-bindings)
          if vector?(template)
            let*-values (((sub-expanded sub-bindings) aux(vector->list(template) bindings)))
              values(list->vector(sub-expanded) sub-bindings)
            values(template bindings)
  define-values (expanded _) aux(template bindings)
  expanded

define bindings
  syntax-rule-match
    '...
    '()
    '(x f (y ...) ...)
    '(1 10 (2 3 4) (5 6 7) (2 4) (3 8))
display(bindings)
newline()
;display
;  syntax-rule-expand
;    '...
;    '(x f (z y ...) ...)
;    bindings

;; define expand-macros(datum rules)
;;   match datum
;;     (head tail ...)
;;       if {symbol?(head) and hash-table-contains?(macros head)}
;;         TODO
;;         TODO
;;     literal
;;       literal

;; display
;;   tree-il->scheme
;;     macroexpand
;;       ; '(let ((x 5) (y 6) (z 7)) (* x y z))
;;       '((lambda (x y z) (* x y z)) 5 6 7)
;; newline()
;; display
;;   tree-il->scheme
;;     macroexpand
;;       '(letrec* ((x 5) (y 6) (z 7)) (* x y z))
;; newline()

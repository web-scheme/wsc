;;; syntax.sscm
;;; Methods for manipulating
;;; [syntax objects](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-13.html)
;;; as well as matching and expanding macros (`syntax-rules`).

#!sweet

;;;; NOT PORTABLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
use-modules (ice-9 match)   ; `match`
use-modules (srfi srfi-69)  ; Basic hash tables
use-modules (srfi srfi-26)  ; `cut` and `cute`

;; Defined in R7Rs-small:
define all(objs)
  if null?(objs)
    #t
    if car(objs)
      all(cdr(objs))
      #f
define (symbol=? . objs)
  and
    all(map(symbol? objs))
    apply(eqv? objs)

;; SRFI 125
define hash-table-empty?(hash-table)
  {hash-table-size(hash-table) = 0}
define hash-table-contains? hash-table-exists?
define hash-table-for-each(proc hash-table)
  hash-table-walk(hash-table proc)
define hash-table-map!(proc hash-table)
  define aux(keys)
    unless null?(keys)
      let ((key car(keys)))
        hash-table-update!(hash-table key (cut proc key <>))
        aux(cdr(keys))
  aux(hash-table-keys(hash-table))
define hash-table-copy(hash-table . mutable?)
  alist->hash-table(hash-table->alist(hash-table))
define hash-table-prune!(proc hash-table)
  define aux(keys)
    unless null?(keys)
      let ((key car(keys)))
        if proc(key hash-table-ref(hash-table key))
          hash-table-delete!(hash-table key)
        aux(cdr(keys))
  aux(hash-table-keys(hash-table))
define hash-table=?(value-comparator left right)
  and
    hash-table?(left)
    hash-table?(right)
    eq?(hash-table->alist(left) hash-table->alist(right))

;; Random helper functions

define write-json(obj)
  define display-array(array)
    let ((tail cdr(array)))
      write-json(car(array))
      unless null?(tail)
        display(", ")
        display-array(tail)
  define display-alist(alist)
    let ((head car(alist))
         (tail cdr(alist)))
      write-json(car(head))
      display(": ")
      write-json(cdr(head))
      unless null?(tail)
        display(", ")
        display-alist(tail)
  if hash-table?(obj)
    let ((alist hash-table->alist(obj)))
      display("{")
      unless null?(alist)
        display-alist(alist)
      display("}")
    if list?(obj)
      begin
        display("[")
        unless null?(obj)
          display-array(obj)
        display("]")
      if null?(obj)
        display("null")
        if eq?(obj #f)
          display("false")
          if eq?(obj #t)
            display("true")
            write(obj)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

include "_derived-expression-types.scm"

define-record-type syntax-object
  make-syntax-object(scope filename line column)
  syntax-object?
  scope syntax-object.get-scope
  filename syntax-object.get-filename
  line syntax-object.get-line
  column syntax-object.get-column

define encapsulate-bindings!(bindings)
  if not(bindings)
    #f  ; Fail fast.
    begin
      hash-table-map!
        lambda (key value) list(value)
        bindings
      bindings

define append-bindings!(prior next)
  if {not(prior) or not(next)}
    #f  ; Fail fast.
    if null?(prior)
      next   ; When `prior` is null, treat it like an empty binding set.
      begin
        hash-table-for-each
          lambda (key value)
            hash-table-update!/default(prior key (cut append <> value) '())
          next
        prior

;; TODO: Replace with `hash-table-union!` (SRFI 125).
define merge-bindings!(prior next)
  if {not(prior) or not(next)}
    #f  ; Fail fast.
    begin
      hash-table-for-each
        lambda (key value) hash-table-set!(prior key value)
        next
      prior

;;; @param ellipsis   Ellipsis symbol, usually `...`.
;;; @param literals   List of literal symbols in the syntax rule patterns.
;;; @param pattern    Pattern to match.
;;; @param expression Expression to match against.
;;; @return A nested association list (TODO: describe better),
;;;         or `#f` if the pattern fails to match.
define syntax-rule-match(ellipsis literals pattern expression)
  ;; TODO: Handle ellipsis or underscore in literals.
  define recurse(pattern expression ellipsis-bindings)
    if member(pattern literals)
      if symbol=?(pattern expression)
        make-hash-table()  ; Literals do not create bindings; return empty.
        #f                 ; If a literal does not match, the pattern does not match.
      if symbol?(pattern)
        if symbol=?(pattern '_)
          make-hash-table()                                  ; The underscore does not create bindings; return empty.
          alist->hash-table(list(cons(pattern expression)))  ; A single pattern variable always matches.
        if pair?(pattern)
          if pair?(expression)
            let ((bindings recurse(car(pattern) car(expression) '())))
              if bindings
                let ((pattern-tail cdr(pattern))
                     (expression-tail cdr(expression)))
                  if pair?(pattern-tail)
                    if symbol=?(car(pattern-tail) ellipsis)
                      if {length(pattern-tail) > length(expression-tail)}
                        ;; Exhausted all possible variable (ellipsis) bindings.
                        merge-bindings!
                          append-bindings!
                            ellipsis-bindings
                            encapsulate-bindings!(bindings)
                          recurse(cdr(pattern-tail) expression-tail '())
                        ;; Still processing bindings for this ellipsis.
                        recurse
                          pattern
                          expression-tail
                          append-bindings!
                            ellipsis-bindings
                            encapsulate-bindings!(bindings)
                      ;; Pattern is not followed by an ellipsis.
                      merge-bindings!(bindings recurse(pattern-tail expression-tail '()))
                    ;; Pattern is an improper list or null.
                    merge-bindings!(bindings recurse(pattern-tail expression-tail '()))
                ;; Short circuit when the recursion fails.
                #f
            if and(null?(expression)
                   pair?(cdr(pattern))
                   symbol=?(cadr(pattern) ellipsis))
              recurse(cddr(pattern) expression '())
              #f
          if {vector?(pattern) and vector?(expression)}
            recurse(vector->list(pattern) vector->list(expression) '())
            if equal?(pattern expression)  ; TODO: Check that `pattern` is "constant" (as in report).
              make-hash-table()
              #f
  recurse(pattern expression '())

define extract-variables(template bindings)
  define aux(template variables)
    if pair?(template)
      aux(cdr(template) aux(car(template) variables))
      if vector?(template)
        aux(vector->list(template) variables)
        if hash-table-contains?(bindings template)
          cons(template variables)
          variables
  aux(template '())

define strip-repeat-bindings!(bindings keys)
  define aux(keys next-strip)
    if null?(keys)
      next-strip
      let* ((key car(keys))
            (value hash-table-ref(bindings key)))
        if list?(value)
          if null?(value)
            #f  ; Ellipsis exhausted.
            begin
              hash-table-set!(next-strip key car(value))
              hash-table-set!(bindings key cdr(value))
              aux(cdr(keys) next-strip)
          raise "Expected key to be repeated but wasn't"
  aux(keys make-hash-table())

define unstrip-leftovers!(bindings strip)
  ;; Put back the unused repeaters. They may be used in a proceeding binding.
  hash-table-for-each
    lambda (key value)
      hash-table-update!/default(bindings key (cut cons value <>) '())
    strip
  bindings

define syntax-rule-expand(ellipsis template bindings)
  ;; HF for easier recursion:
  define aux(template bindings)
    if hash-table-contains?(bindings template)
      ;; The template is just a bound variable.
      let ((value hash-table-ref(bindings template)))
        hash-table-delete!(bindings template)
        value
      if pair?(template)
        let ((template-head car(template))
             (template-tail cdr(template)))
          if {pair?(template-tail) and symbol=?(car(template-tail) ellipsis)}
            let ((sub-bindings
                  strip-repeat-bindings!(bindings extract-variables(template-head bindings))))
              if sub-bindings
                ;; Yes more repeat bindings.
                cons(aux(template-head sub-bindings)
                     aux(template unstrip-leftovers!(bindings sub-bindings)))
                ;; No more repeat bindings to apply; the ellipsis is exhausted.
                begin
                  hash-table-prune!
                    lambda (key value) null?(value)
                    bindings
                  aux(cdr(template-tail) bindings)
            ;; Non-repeated pattern.
            cons(aux(car(template) bindings) aux(cdr(template) bindings))
        if vector?(template)
          list->vector(aux(vector->list(template) bindings))
          template
  aux(template bindings)

define bindings
  syntax-rule-match
    '...
    '()
    '(x f (y z ...) ...)
    '(1 10 (2 3 4) (5 6 7) (2 4) (3 8))
write-json(bindings)
newline()
;write-json(strip-repeat-bindings!(bindings))
;newline()
(display (syntax-rule-expand '... '(f x (z ... y) ...) bindings))
;newline()

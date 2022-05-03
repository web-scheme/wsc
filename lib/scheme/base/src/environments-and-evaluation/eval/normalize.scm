
(use-modules (language tree-il))
(use-modules (ice-9 match))

;; Guile glue
define all(objs)
  if null?(objs) #t and(car(objs) all(cdr(objs)))
define (symbol=? . objs)
  and
    all(map(symbol? objs))
    apply(eqv? objs)

define-syntax cond
  syntax-rules (else =>)
    (cond (else result1 result2 ...))
      begin result1 result2 ...
    (cond (test => result))
      let ((temp test))
        if temp
          result(temp)
    (cond (test => result) clause1 clause2 ...)
      let ((temp test))
        if temp
          result(temp)
          cond(clause1 clause2 ...)
    (cond (test))
      test
    (cond (test) clause1 clause2 ...)
      let ((temp test))
        if temp
          temp
          cond(clause1 clause2 ...)
    (cond (test result1 result2 ...))
      if test
        begin result1 result2 ...
    (cond (test result1 result2 ...) clause1 clause2 ...)
      if test
        begin result1 result2 ...
        cond clause1 clause2 ...

define-syntax case
  syntax-rules (else =>)
    (case (key ...) clauses ...)
      let ((atom-key (key ...)))
        case atom-key clauses ...
    (case key (else => result))
      result(key)
    (case key (else result1 result2 ...))
      begin result1 result2 ...
    (case key ((atoms ...) result1 result2 ...))
      if memv(key '(atoms ...))
        begin result1 result2 ...
    (case key ((atoms ...) => result))
      if memv(key '(atoms ...))
        result(key)
    (case key ((atoms ...) => result) clause clauses ...)
      if memv(key '(atoms ...))
        result(key)
        case key clause clauses ...
    (case key ((atoms ...) result1 result2 ...) clause clauses ...)
      if memv(key '(atoms ...))
        begin result1 result2 ...
        case key clause clauses ...

define-syntax and
  syntax-rules ()
    (and)
      #t
    (and test)
      test
    (and test1 test2 ...)
      if test1 and(test2 ...) #f

define-syntax or
  syntax-rules ()
    (or)
      #f
    (or test)
      test
    (or test1 test2 ...)
      let ((x test1))
        if x x or(test2 ...)

define-syntax when
  syntax-rules ()
    (when test result1 result2 ...)
      if test
        begin result1 result2 ...

define-syntax unless
  syntax-rules ()
    (unless test result1 result2 ...)
      if not(test)
        begin result1 result2 ...

define-syntax let
  syntax-rules ()
    (let ((name val) ...) body1 body2 ...)
      (lambda (name ...) body1 body2 ...) val ...
    (let tag ((name val) ...) body1 body2 ...)
      (letrec ((tag (lambda (name ...) body1 body2 ...))) tag) val ...

define-syntax let*
  syntax-rules ()
    (let* () body1 body2 ...)
      let () body1 body2 ...
    (let* ((name1 val1) (name2 val2) ...) body1 body2 ...)
      let ((name1 val1))
        let* ((name2 val2) ...)
          body1 body2 ...

define-syntax letrec
  syntax-rules ()
    (letrec ((var1 init1) ...) body ...)
      letrec "generate_temp_names" (var1 ...) () ((var1 init1) ...) body ...
    (letrec "generate_temp_names" () (temp1 ...) ((var1 init1) ...) body ...)
      let ((var1 <undefined>) ...)
        let ((temp1 init1) ...)
          set!(var1 temp1) ... body ...
    (letrec "generate_temp_names" (x y ...) (temp ...) ((var1 init1) ...) body ...)
      letrec "generate_temp_names" (y ...) (newtemp temp ...) ((var1 init1) ...) body ...

define-syntax letrec*
  syntax-rules ()
    (letrec* ((var1 init1) ...) body1 body2 ...)
      let ((var1 <undefined>) ...) set!(var1 init1) ... (let () body1 body2 ...)

define-syntax let-values
  syntax-rules ()
    (let-values (binding ...) body0 body1 ...)
      let-values "bind" (binding ...) () (begin body0 body1 ...)
    (let-values "bind" () tmps body)
      let tmps body
    (let-values "bind" ((b0 e0) binding ...) tmps body)
      let-values "mktmp" b0 e0 () (binding ...) tmps body
    (let-values "mktmp" () e0 args bindings tmps body)
      call-with-values
        lambda () e0
        lambda args
          let-values "bind" bindings tmps body
    (let-values "mktmp" (a . b) e0 (arg ...) bindings (tmp ...) body)
      let-values "mktmp" b e0 (arg ... x) bindings (tmp ... (a x)) body
    (let-values "mktmp" a e0 (arg ...) bindings (tmp ...) body)
      call-with-values
        lambda () e0
        lambda (arg ... . x)
          let-values "bind" bindings (tmp ... (a x)) body

define-syntax let*-values
  syntax-rules ()
    (let*-values () body0 body1 ...)
      let () body0 body1 ...
    (let*-values (binding0 binding1 ...) body0 body1 ...)
      let-values (binding0)
        let*-values (binding1 ...) body0 body1 ...

define-syntax define-values
  syntax-rules ()
    (define-values () expr)
      define dummy
        call-with-values
          lambda () expr
          lambda args #f
    (define-values (var) expr)
      define var expr
    (define-values (var0 var1 ... varn) expr)
      (begin
        (define var0 call-with-values((lambda () expr) list))
        (define var1 (let ((v cadr(var0))) set-cdr!(var0 cddr(var0)) v)) ...
        (define varn (let ((v cadr(var0))) set!(var0 car(var0)) v)))
    (define-values (var0 var1 ... . varn) expr)
      (begin
        (define var0 call-with-values((lambda () expr) list))
        (define var1 (let ((v cadr(var0))) set-cdr!(var0 cddr(var0)) v)) ...
        (define varn (let ((v cdr(var0))) set!(var0 car(var0)) v)))
    (define-values var expr)
      define var
        call-with-values
          lambda () expr
          list

;;; @param ellipsis   Ellipsis symbol, usually `...`.
;;; @param literals   List of literal symbols in the syntax rule patterns.
;;; @param pattern    Pattern to match.
;;; @param expression Expression to match against.
;;; @return A nested association list TODO,
;;;         or `#f` if the pattern fails to match.
define syntax-rule-match(ellipsis literals pattern expression)
  define ellipsis-enabled not(member(ellipsis literals))
  define combine(bindings next-bindings)
    if next-bindings
      append(bindings next-bindings)
      #f
  define aux(pattern expression ellipsis-bindings)
    if member(pattern literals)
      if symbol=?(pattern expression)
        '()
        #f
      if symbol?(pattern)
        list(cons(pattern expression))
        if pair?(pattern)
          if pair?(expression)
            let ((bindings aux(car(pattern) car(expression) '())))
              if bindings
                let ((pattern-tail cdr(pattern))
                     (expression-tail cdr(expression)))
                  if pair?(pattern-tail)
                    if {ellipsis-enabled and symbol=?(car(pattern-tail) ellipsis)}
                      if {length(pattern-tail) > length(expression-tail)}
                        cons(combine(ellipsis-bindings bindings)
                             aux(cdr(pattern-tail) expression-tail '()))
                        aux(pattern expression-tail combine(ellipsis-bindings bindings))
                      combine(bindings aux(pattern-tail expression-tail '()))
                    combine(bindings aux(pattern-tail expression-tail '()))
                #f
            if and(ellipsis-enabled
                   null?(expression)
                   pair?(cdr(pattern))
                   symbol=?(cadr(pattern) ellipsis))
              ;; If this is the case, `ellipsis-bindings` is null.
              aux(cddr(pattern) expression '())
              #f
          if {vector?(pattern) and vector?(expression)}
            aux(vector->list(pattern) vector->list(expression) '())
            if equal?(pattern expression)  ; TODO: Check that `pattern` is "constant".
              '()
              #f
  aux(pattern expression '())

define syntax-rule-expand(ellipsis template bindings)
  define aux(template bindings)
    if symbol=?(template caar(bindings))
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
          if null?(template)
            if null?(bindings)
              values('() '())
              raise("Too many bindings!")
            values(template bindings)
  define-values (expanded _) aux(template bindings)
  expanded

define bindings
  syntax-rule-match
    '...
    '()
    '(x f (z y ...) ...)
    '(1 10 (2 3 4) (5 6 7) (2 4) (3 8))
display(bindings)
newline()
display
  syntax-rule-expand
    '...
    '(x f (y ... z) ...)
    bindings

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

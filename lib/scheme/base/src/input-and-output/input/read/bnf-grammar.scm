#|* LALR(1) Parser Generator
  *
  * Define the WebScheme scanner,
  * capable of scanning both "vanilla" s-expressions and "sweet" t-expressions.
  *
  * @see https://web-scheme.org/r7rs-small.pdf#subsection.7.1.1
  * @see https://srfi.schemers.org/srfi-110/srfi-110.html
|#
#!sweet

define empty-action() '()
define singleton-action(non-terminal) non-terminal

define-syntax *bnf-grammar
  syntax-rules (::= =>)
    ;; The special case for empty productions.
    (*bnf-grammar (*rules ...) ::= (()) tail ...)
      *bnf-grammar (*rules ... (() empty-action)) ::= tail ...
    ;; The special case when there's only a single non-terminal in the production.
    (*bnf-grammar (*rules ...) ::= ((*non-terminal)) tail ...)
      *bnf-grammar (*rules ... ((*non-terminal) singleton-action)) ::= tail ...
    ;; The general case.
    (*bnf-grammar (*rules ...) ::= ((*non-terminals ...) => *action) tail ...)
      *bnf-grammar (*rules ... ((*non-terminals ...) *action)) ::= tail ...
    ;; The final transformation.
    (*bnf-grammar ((*expression *action) ...) ::=)
      ;; Each expression is a list of non-terminal symbols.
      ;; Each action is a function taking the same number of arguments
      ;; as non-terminals in the corresponding expression,
      ;; to convert l-values of the expression to the l-value for the production.
      ;; TODO: Implement.
      '()

define-syntax bnf-grammar
  syntax-rules ()
    (bnf-grammar rules ...)
      *bnf-grammar () rules ...

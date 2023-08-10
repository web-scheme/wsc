;; All Wasm definitions that are too fundamental for `(scheme base)` go here.
(component $sub-base

  ;; Variadic parameter list.
  ;;
  ;; Because Scheme is dynamically typed,
  ;; parameters of compiled functions always have type `anyref`,
  ;; except for the final parameter of a variadic function,
  ;; like `zs` in `(f x y . zs)`.
  ;;
  ;; In a static invocation, like `(f a b c d)`,
  ;; the caller creates and passes in a list,
  ;; In a dynamic invocation using `apply`,
  ;; the list may be improper, or not a list at all.
  (type $vararg (option anyref))
)

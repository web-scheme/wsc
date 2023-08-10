;; Exports of `(scheme base)` that are too primitive to be expressed in Scheme.
(component $c-scheme-base

  ;; Pair of two values.
  (type $pair (record (field "car" anyref) (field "cdr" anyref)))

  (core module $impl
    (func (export "pair?") (param $obj anyref) (result bool)
      (runtime-type-check-TODO $obj $pair)
    )
  )
)

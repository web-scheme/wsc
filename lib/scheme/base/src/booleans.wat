(module
  (import "\00pairs-and-lists" "car" (func $car (param i64) (result i64)))
  (import "\00pairs-and-lists" "cdr" (func $cdr (param i64) (result i64)))

  ;; The constants `#f` and `#t`.
  ;; Any boolean that's not `#f` is `#t`, but this is canonical:
  ;;   #f binary: 0000010000000000000000000000000000000000000000000000000000000000
  ;;   #t binary: 0000010011111111111111111111111111111111111111111111111111111111
  (global $false (export "#f") i64 (i64.const 0x0400000000000000))
  (global $true (export "#t") i64 (i64.const 0x04ffffffffffffff))
  (export "#false" (global $false))  ;; Alias for `#f`.
  (export "#true" (global $true))    ;; Alias for `#t`.

  ;;*     (not obj)
  ;;* The `not` procedure returns `#t` if `obj` is false, and returns `#f` otherwise.
  (func $not (export "not")
             (param $obj i64)
             (result i64)
    (i64.or (i32.mul (i64.extend_i32_u (i64.eq (local.get $obj)
                                               (global.get $false)))
                     (i64.const 0x00ffffffffffffff))
            (i64.const 0x0400000000000000)))

  ;;*     (boolean? obj)
  ;;* The `boolean?` predicate returns `#t` if `obj` is either `#t` or `#f` and returns `#f` otherwise.
  (func $boolean__pred (export "boolean?")
                       (param $obj i64)
                       (result i64)
    (return_call $not
      (i64.and (local.get $obj)
               (i64.const 0xff00000000000000))))

  ;;*     (boolean=? . objs)
  ;;* Returns `#t` if all the arguments are booleans and all are `#t` or all are `#f`.
  (func $boolean__equ_pred (export "boolean=?")
                           (param $objs i64)
                           (result i64)
    (if (i64.eqz (local.get $objs))
      (then (return (global.get $true)))
      (else (if (i64.eq (call $boolean__pred
                          (call $car
                            (local.get $objs)))
                        (global.get $false))
              (then (return (global.get $false)))
              (else (return_call $boolean__equ_pred
                      (call $cdr
                        (local.get $objs)))))))))

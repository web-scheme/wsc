
;; A dummy source file for experimentation.

(define-syntax derspler
  (syntax-rules ()
    ((derspler obj port ...)
     (display obj port ...))))

(define x "Does anybody care?")

(derspler x)


(define y #f)


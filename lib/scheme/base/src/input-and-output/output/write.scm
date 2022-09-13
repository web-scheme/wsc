#| TODO: Document entrypoint. |#
#!sweet

;; SRE for figuring out whether string characters written by [write] require escaping or not.
;; Always matches.
define write-string-escape
  rx (or (-> backslash (/ "\"\\"))     ; A single backslash-escaped character, or
         (* (complement (/ "\"\\"))))  ; zero or more non-escaped characters.

define write-string-contents(string start port)
  define match regexp-search(write-string-escape string start)
  define end regexp-match-submatch-end(match 0)
  unless {start = end}
    if regexp-match-submatch(match 'backslash)
      begin
        write-char(#\\ port)
        write-char(string-ref(string start) port)
      write-string(string port start end)
    write-string-contents(string end port)

define write(obj . port)
  if length(port) > 0
    if length(port) > 1
      raise 'wrong-argument-count
      set! port car(port)
    set! port current-output-port()
  match obj
    (? string?)
      begin
        display "\"" port
        write-string-contents(obj 0 port)
        display "\"" port
    (? symbol?)
      begin
        TODO
    TODO

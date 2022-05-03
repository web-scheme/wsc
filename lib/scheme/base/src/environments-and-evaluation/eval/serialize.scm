#!sweet

;; Wasm magic.
define MAGIC #u8(#x00 #x61 #x73 #x6d)
;; Wasm version.
define VERSION #u8(#x01 #x00 #x00 #x00)

;; Wasm section IDs.
define CUSTOM-SECTION-ID 0
define TYPE-SECTION-ID 1
define IMPORT-SECTION-ID 2
define FUNCTION-SECTION-ID 3
define TABLE-SECTION-ID 4
define MEMORY-SECTION-ID 5
define GLOBAL-SECTION-ID 6
define EXPORT-SECTION-ID 7
define START-SECTION-ID 8
define ELEMENT-SECTION-ID 9
define CODE-SECTION-ID 10
define DATA-SECTION-ID 11
define DATA-COUNT-SECTION-ID 12

;; Wasm name section subsection IDs.
;; https://webassembly.github.io/spec/core/appendix/custom.html#name-section
define NAME-SECTION-MODULE-SUBSECTION-ID 0

;;;
;;; @param library-name The library name (list of symbols and exact non-negative integers).
;;;                     For executables, it is `()`.
;;;
define write-module(library-name
                    functions globals
                    imports exports
                    tables memories
                    start
                    port)
  write-bytevector(MAGIC port)
  write-bytevector(VERSION port)
  unless null?(functions)
    write-section(TYPE-SECTION-ID serialize-types(functions #u8()) port)
  unless null?(imports)
    write-section(IMPORT-SECTION-ID serialize-imports(imports #u8()) port)
  unless null?(functions)
    write-section(FUNCTION-SECTION-ID serialize-functions(functions #u8()) port)
  unless null?(tables)
    write-section(TABLE-SECTION-ID serialize-tables(tables #u8()) port)
  unless null?(memories)
    write-section(MEMORY-SECTION-ID serialize-memories(memories #u8()) port)
  unless null?(globals)
    write-section(GLOBAL-SECTION-ID serialize-globals(globals #u8()) port)
  unless null?(exports)
    write-section(EXPORT-SECTION-ID serialize-exports(exports #u8()) port)
  unless null?(start)
    write-section(START-SECTION-ID serialize-start(start) port)
  unless null?(tables)
    write-section(ELEMENT-SECTION-ID serialize-elements(tables #u8()) port)
  unless null?(functions)
    write-section(CODE-SECTION-ID serialize-elements(functions #u8()) port)
  unless null?(memories)
    write-section(DATA-SECTION-ID serialize-data(memories #u8()) port)
    write-section(DATA-COUNT-SECTION-ID serialize-data-count(memories) port)
  unless null?(library-name)
    let ((name-section-buffer #u8(#x00 )))
      write-section(CUSTOM-SECTION-ID serialize-module-name(library-name #u8()) port)

define serialize-section(section-id section-contents)
  define contents-length bytevector-length(section-contents)
  define section-buffer make-bytevector({contents-length + 5})
  bytevector-u8-set!(section-buffer 0 section-id)                    ; Each section starts with a 1-byte section ID.
  bytevector-copy!(section-buffer 1 serialize-i32(contents-length))  ; Followed by an unsigned 32-bit length.
  bytevector-copy!(section-buffer 5 section-contents)                ; Followed by the section contents.
  section-buffer

;; Vectors are encoded with their `u32` length followed by the encoding of their element sequence.
;; https://webassembly.github.io/spec/core/binary/conventions.html#vectors
define serialize-byte-vector(buffer)
  bytevector-append(serialize-i32(bytevector-length(buffer)) buffer)

define serialize-i32(value)
  ;; TODO: Make this better (struct-packing library in r7rs-large?).
  define-values (value byte1) truncate/(value #x100)
  define-values (value byte2) truncate/(value #x100)
  define-values (value byte3) truncate/(value #x100)
  define byte4 truncate-remainder(value #x100)
  bytevector(byte1 byte2 byte3 byte4)

;; Names are encoded as a vector of bytes containing the UTF-8 encoding of the name’s character sequence.
;; https://webassembly.github.io/spec/core/binary/values.html#names
define serialize-name(sym)
  serialize-byte-vector(encode-symbol(sym))

define serialize-module-name(library-name)
  define serialize-library-name-part(part)
    if symbol?(part)
      bytevector-append(bytevector(char->integer(#\soh)) encode-symbol(part))    ; UTF-8 parts prefixed with ASCII 'Start of Heading'.
      bytevector-append(bytevector(char->integer(#\dle)) encode-i32-utf8(part))  ; Integer parts prefixed with ASCII 'Data Link Escape'.
  if null?(library-name)
    serialize-section(NAME-SECTION-MODULE-SUBSECTION-ID buffer)
    let ((rest cdr(library-name)))
      bytevector-append(serialize-library-name-part(car(library-name))
                        if null?(rest) #u8() #u8(#\us)    ; Parts separated by ASCII 'Unit Separator'.
                        serialize-module-name(rest))

;; Return the UTF-8 encoding of the symbol.
define encode-symbol(sym) string->utf8(symbol->string(sym))

;; Return the UTF-8 encoding of the single character.
define encode-char(char) string->utf8(string(char))

;; Return the UTF-8 encoding of the single code point, adjusted for the gap between `#xd7ff` and `#xe000`.
;; `number` can be at most 1112063.
define encode-i32-utf8(number)
  encode-char
    integer->char
      if {number < #xd800} number {number + #x800}


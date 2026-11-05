(lang dune 3.0)

(name starling)

(generate_opam_files true)

(cram enable)

(license MIT)

(authors "Tushman Khalse")

(maintainers "Tushman Khalse")

(source
 (github Starscourge/starling))

(package
 (name starling)
 (synopsis "A statically typed functional language compiled to bytecode")
 (description
  "Starling is a small functional language with Lisp-style syntax, Hindley-Milner type inference, algebraic data types, exhaustive pattern matching and arbitrary-precision integers. It compiles to bytecode and runs on its own stack VM with proper tail calls.")
 (depends
  (ocaml
   (>= 4.14))
  dune
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)
  (zarith :with-test)))

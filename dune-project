(lang dune 2.7)

(cram enable)
(name typocaml)
(allow_approximate_merlin)
(version 0.1.0)
(formatting (enabled_for ocaml reason))
(using menhir 2.0)

(package
 (name typocaml)
 (depends
   (ocaml (>= 4.13.0))
   (fmt ( >= 0.9.0 ))
   (logs ( >= 0.7.0 ))
  )
)

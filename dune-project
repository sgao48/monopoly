(lang dune 3.4)

(generate_opam_files true)
(authors "Shining Gao")
(maintainers "sgao48@jhu.edu")
(package
 (name monopoly) ; put in your own project name here
 (synopsis "Monopoly Game")
 (description "Have fun with it")
 (depends
  (ANSITerminal (>= 0.8.5))
  (spectrum (>= 0.6.0))
  (bisect_ppx (>= 2.8.1))
  (ounit2 (>= 2.2.6)) ; for each opam dependency list the version
  (core (>= v0.15.1)) ; `opam list core` will display which version of core you have
))

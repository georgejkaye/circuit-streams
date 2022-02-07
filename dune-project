(lang dune 2.9)

(name circuit_streams)

(license GPL-3.0-or-later)

(maintainers "George Kaye <g.j.kaye@cs.bham.ac.uk>")

(authors "George Kaye <g.j.kaye@cs.bham.ac.uk")

(source
 (github georgejkaye/circuit-streams))

(bug_reports "https://github.com/georgejkaye/circuit-streams/issues")

(homepage "https://github.com/georgejkaye/circuit-streams")

(documentation "https://github.com/georgejkaye/circuit-streams")

(generate_opam_files true)

(package
 (name circuit_streams)
 (synopsis "Generating mealy machines from stream functions")
 (description "Generating mealy machines from stream functions")
 (depends
  (ocaml
   (>= 4.11.0))))

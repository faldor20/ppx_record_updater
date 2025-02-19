(lang dune 3.18)

(name ppx_record_updater)

(generate_opam_files true)

(source
 (github faldor20/ppx_record_updater))

(authors "Eli Dowling <eli.jambu@gmail.com>")

(maintainers "Eli Dowling <eli.jambu@gmail.com>")

(license MIT)

(documentation https://github.com/faldor20/ppx_record_updater)

(package
 (name ppx_record_updater)
 (synopsis "A PPX deriver that generates an update function and type for dynamically performing partial updates on a record.")
 (description "A PPX deriver that generates an update function and type for dynamically performing partial updates on a record.")
 (depends ocaml ppxlib ppx_deriving)
 (tags (ppx)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

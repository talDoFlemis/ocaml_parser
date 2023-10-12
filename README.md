# OCaml Parser

## Installing OCAML

```bash
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
```

```bash
opam init
```

## Updating OPAM if using OCaml less than 5.1.0

```bassh
opam update
```

```bassh
opam switch create 5.1.0
```

## Installing Dependencies

```bassh
opam install . --deps-only --working-dir
```

## Running

```bash
dune exec ocaml_parser
```

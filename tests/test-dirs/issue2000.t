  $ cat >main.ml <<EOF
  > let a : type b. 'a as b list = []
  > EOF

FIXME: This syntactically incorrect OCaml snippet produces two unrelated cryptic error message.
See https://github.com/ocaml/merlin/issues/2000.
  $ $MERLIN single errors -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 21
        },
        "end": {
          "line": 1,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The type variable name '_ is not allowed in programs"
      },
      {
        "start": {
          "line": 1,
          "col": 22
        },
        "end": {
          "line": 1,
          "col": 23
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Expected a type variable after `as`"
      },
      {
        "start": {
          "line": 1,
          "col": 22
        },
        "end": {
          "line": 1,
          "col": 23
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value b"
      }
    ],
    "notifications": []
  }

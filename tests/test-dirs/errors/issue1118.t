Ensure that the issue highlighted in issue #1118 is no longer reproducible.
The following value definition no longer causes a crash but still produces unrelated errors.
See https://github.com/ocaml/merlin/issues/1118.

  $ cat >main.ml <<EOF
  > let a : type b. 'a as b list = []
  > EOF

  $ $MERLIN single errors -filename main.ml < main.ml
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
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value b"
      }
    ],
    "notifications": []
  }

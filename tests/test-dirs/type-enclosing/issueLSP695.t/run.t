The file is not empty: it has a comment and an operator
  $ $MERLIN single document -position 2:0 -filename main.ml <main.ml 
  {
    "class": "return",
    "value": "Integer multiplication.
      Left-associative operator, see {!Ocaml_operators} for more information.",
    "notifications": []
  }

  $ $MERLIN single errors -filename main.ml <main.ml | jq '.value'
  [
    {
      "start": {
        "line": 2,
        "col": 0
      },
      "end": {
        "line": 2,
        "col": 1
      },
      "type": "parser",
      "sub": [],
      "valid": true,
      "message": "Expected a structure item or end of file"
    }
  ]

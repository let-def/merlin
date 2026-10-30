open Parser_raw

  let string_of_INT = function
    | (s, None) -> Printf.sprintf "INT(%s)" s
    | (s, Some c) -> Printf.sprintf "INT(%s%c)" s c

  let string_of_FLOAT = function
    | (s, None) -> Printf.sprintf "FLOAT(%s)" s
    | (s, Some c) -> Printf.sprintf "FLOAT(%s%c)" s c

  let string_of_STRING = function
    | s, _, Some s' -> Printf.sprintf "STRING(%S,%S)" s s'
    | s, _, None -> Printf.sprintf "STRING(%S)" s

  let string_of_quoted_STRING = function
    | _, _, s, _, Some s' -> Printf.sprintf "QUOTED_STRING(%S,%S)" s s'
    | _, _, s, _, None -> Printf.sprintf "QUOTED_STRING(%S)" s


let print_symbol = function
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_error) -> "error"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_WITH) -> "with"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_WHILE) -> "while"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_WHEN) -> "when"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL) -> "virtual"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_VAL) -> "val"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE) -> "_"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_UIDENT) -> "UIDENT"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TYPE) -> "type"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TRY) -> "try"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TRUE) -> "true"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TO) -> "to"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TILDE) -> "~"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_THEN) -> "then"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_STRUCT) -> "struct"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_STRING) -> "STRING"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_STAR) -> "*"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_SIG) -> "sig"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI) -> ";;"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_SEMI) -> ";"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_RPAREN) -> ")"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_REC) -> "rec"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_RBRACKET) -> "]"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_RBRACE) -> "}"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_QUOTED_STRING_ITEM) -> "QUOTED_STRING_ITEM"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_QUOTED_STRING_EXPR) -> "QUOTED_STRING_EXPR"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_QUOTE) -> "'"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_QUESTION) -> "?"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PRIVATE) -> "private"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP) -> "!+" (* chosen with care; see above *)
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ) -> "+="
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT) -> "+."
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PLUS) -> "+"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PERCENT) -> "%"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OR) -> "or"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OPTLABEL) -> "?<label>"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OPEN) -> "open"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OF) -> "of"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OBJECT) -> "object"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_NONREC) -> "nonrec"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_NEW) -> "new"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MUTABLE) -> "mutable"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MODULE) -> "module"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MINUSGREATER) -> "->"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MINUSDOT) -> "-."
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MINUS) -> "-"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_METHOD) -> "method"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_ESCAPE) -> ".~"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_BRACKET_OPEN) -> ".<"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_BRACKET_CLOSE) -> ">."
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MATCH) -> "match"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LPAREN) -> "("
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LIDENT) -> "LIDENT"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LETOP) -> "LETOP"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LET) -> "let"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LESSMINUS) -> "<-"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LESS) -> "<"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENTPERCENT) -> "[%%"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENT) -> "[%"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETLESS) -> "[<"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETGREATER) -> "[>"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETBAR) -> "[|"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATATAT) -> "[@@@"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATAT) -> "[@@"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETAT) -> "[@"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKET) -> "["
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACELESS) -> "{<"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACE) -> "{"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LAZY) -> "lazy"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LABEL) -> "label"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INT) -> "INT"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INITIALIZER) -> "initializer"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INHERIT) -> "inherit"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP4) -> "INFIXOP4"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP3) -> "INFIXOP3"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP2) -> "INFIXOP2"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP1) -> "INFIXOP1"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP0) -> "INFIXOP0"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INCLUDE) -> "include"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_IN) -> "in"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_IF) -> "if"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_HASHOP) -> "#<op>"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_HASH) -> "#"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACKET) -> ">]"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACE) -> ">}"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_GREATER) -> ">"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR) -> "functor"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FUNCTION) -> "function"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FUN) -> "fun"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FOR) -> "for"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FLOAT) -> "FLOAT"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FALSE) -> "false"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL) -> "external"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION) -> "exception"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EQUAL) -> "="
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EOL) -> "EOL"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EOF) -> "EOF"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_END) -> "end"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_ELSE) -> "else"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EFFECT) -> "effect"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOWNTO) -> "downto"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOTOP) -> "DOTOP"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOTDOT) -> ".."
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOT) -> "."
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DONE) -> "done"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING) -> "DOCSTRING"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DO) -> "do"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_CONSTRAINT) -> "constraint"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COMMENT) -> "COMMENT"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COMMA) -> ","
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COLONGREATER) -> ":>"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COLONEQUAL) -> ":="
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COLONCOLON) -> "::"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COLON) -> ":"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_CLASS) -> "class"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_CHAR) -> "CHAR"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BEGIN) -> "begin"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BARRBRACKET) -> "|]"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BARBAR) -> "||"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BAR) -> "|"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BANG) -> "!"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BACKQUOTE) -> "`"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_ASSERT) -> "assert"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_AS) -> "as"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_ANDOP) -> "ANDOP"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_AND) -> "and"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND) -> "&"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER) -> "&&"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_with_type_binder) -> "with_type_binder"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_with_constraint) -> "with_constraint"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_virtual_with_private_flag) -> "virtual_with_private_flag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_virtual_with_mutable_flag) -> "virtual_with_mutable_flag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_virtual_flag) -> "virtual_flag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_value_description) -> "value_description"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_value) -> "value"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_val_longident) -> "val_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_val_ident) -> "val_ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_val_extra_ident) -> "val_extra_ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_use_file) -> "use_file"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_variance) -> "type_variance"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_variable) -> "type_variable"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_parameters) -> "type_parameters"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_parameter) -> "type_parameter"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_longident) -> "type_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_kind) -> "type_kind"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_constraint) -> "type_constraint"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_tuple_type) -> "tuple_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_toplevel_phrase) -> "toplevel_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_toplevel_directive) -> "toplevel_directive"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_tag_field) -> "tag_field"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_subtractive) -> "subtractive"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_structure_item) -> "structure_item"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_structure) -> "structure"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_strict_binding) -> "strict_binding"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration) -> "str_exception_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_single_attr_id) -> "single_attr_id"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident) -> "simple_pattern_not_ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_pattern) -> "simple_pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_param_pattern) -> "simple_param_pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_expr) -> "simple_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern) -> "simple_delimited_pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_signed_constant) -> "signed_constant"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_signature_item) -> "signature_item"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_signature) -> "signature"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration) -> "sig_exception_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_seq_expr) -> "seq_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_record_expr_field_) -> "separated_or_terminated_nonempty_list_SEMI_record_expr_field_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_pattern_) -> "separated_or_terminated_nonempty_list_SEMI_pattern_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_object_expr_field_) -> "separated_or_terminated_nonempty_list_SEMI_object_expr_field_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_expr_) -> "separated_or_terminated_nonempty_list_SEMI_expr_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_row_field) -> "row_field"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_core_type_) -> "reversed_separated_nontrivial_llist_COMMA_core_type_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_) -> "reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_atomic_type_) -> "reversed_separated_nonempty_llist_STAR_atomic_type_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_type_parameter_) -> "reversed_separated_nonempty_llist_COMMA_type_parameter_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_core_type_) -> "reversed_separated_nonempty_llist_COMMA_core_type_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_BAR_row_field_) -> "reversed_separated_nonempty_llist_BAR_row_field_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AND_with_constraint_) -> "reversed_separated_nonempty_llist_AND_with_constraint_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_) -> "reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_preceded_or_separated_nonempty_llist_BAR_match_case_) -> "reversed_preceded_or_separated_nonempty_llist_BAR_match_case_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_typevar_) -> "reversed_nonempty_llist_typevar_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_name_tag_) -> "reversed_nonempty_llist_name_tag_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_labeled_simple_expr_) -> "reversed_nonempty_llist_labeled_simple_expr_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_functor_arg_) -> "reversed_nonempty_llist_functor_arg_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_concat_fun_param_as_list_) -> "reversed_nonempty_concat_fun_param_as_list_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_preceded_CONSTRAINT_constrain__) -> "reversed_llist_preceded_CONSTRAINT_constrain__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_pattern_pattern_no_exn_) -> "reversed_labeled_tuple_pattern_pattern_no_exn_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_pattern_pattern_) -> "reversed_labeled_tuple_pattern_pattern_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_body) -> "reversed_labeled_tuple_body"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_declaration_) -> "reversed_bar_llist_extension_constructor_declaration_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_) -> "reversed_bar_llist_extension_constructor_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_constructor_declaration_) -> "reversed_bar_llist_constructor_declaration_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_record_expr_content) -> "record_expr_content"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rec_flag) -> "rec_flag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags) -> "private_virtual_flags"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_private_flag) -> "private_flag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration) -> "primitive_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute) -> "post_item_attribute"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_no_attr_) -> "possibly_poly_core_type_no_attr_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_) -> "possibly_poly_core_type_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_payload) -> "payload"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern_var) -> "pattern_var"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn) -> "pattern_no_exn"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern_gen) -> "pattern_gen"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern) -> "pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_val_longident) -> "parse_val_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_pattern) -> "parse_pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_mty_longident) -> "parse_mty_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_module_type) -> "parse_module_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_module_expr) -> "parse_module_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_mod_longident) -> "parse_mod_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_mod_ext_longident) -> "parse_mod_ext_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_expression) -> "parse_expression"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_core_type) -> "parse_core_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_constr_longident) -> "parse_constr_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_any_longident) -> "parse_any_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr) -> "paren_module_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_optlabel) -> "optlabel"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_type_constraint_) -> "option_type_constraint_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_seq_expr__) -> "option_preceded_EQUAL_seq_expr__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_pattern__) -> "option_preceded_EQUAL_pattern__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_module_type__) -> "option_preceded_EQUAL_module_type__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_expr__) -> "option_preceded_EQUAL_expr__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_core_type__) -> "option_preceded_COLON_core_type__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_atomic_type__) -> "option_preceded_COLON_atomic_type__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_preceded_AS_mkrhs_LIDENT___) -> "option_preceded_AS_mkrhs_LIDENT___"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_SEMI_) -> "option_SEMI_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_BAR_) -> "option_BAR_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand) -> "opt_ampersand"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_operator) -> "operator"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_open_description) -> "open_description"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_open_declaration) -> "open_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_object_type) -> "object_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nonempty_type_kind) -> "nonempty_type_kind"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_raw_string_) -> "nonempty_list_raw_string_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_mkrhs_LIDENT__) -> "nonempty_list_mkrhs_LIDENT__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_name_tag) -> "name_tag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mutable_virtual_flags) -> "mutable_virtual_flags"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mutable_flag) -> "mutable_flag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mty_longident) -> "mty_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_type_subst) -> "module_type_subst"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration) -> "module_type_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_type) -> "module_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_subst) -> "module_subst"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_name) -> "module_name"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_expr) -> "module_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body) -> "module_declaration_body"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_binding_body) -> "module_binding_body"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mod_longident) -> "mod_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident) -> "mod_ext_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_val_ident_) -> "mk_longident_mod_longident_val_ident_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_UIDENT_) -> "mk_longident_mod_longident_UIDENT_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_LIDENT_) -> "mk_longident_mod_longident_LIDENT_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_ident_) -> "mk_longident_mod_ext_longident_ident_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident___anonymous_43_) -> "mk_longident_mod_ext_longident___anonymous_43_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_UIDENT_) -> "mk_longident_mod_ext_longident_UIDENT_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_LIDENT_) -> "mk_longident_mod_ext_longident_LIDENT_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_method_) -> "method_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_meth_list) -> "meth_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_match_case) -> "match_case"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_local_structure_item) -> "local_structure_item"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_listx_SEMI_record_pat_field_UNDERSCORE_) -> "listx_SEMI_record_pat_field_UNDERSCORE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_use_file_element_) -> "list_use_file_element_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_text_str_structure_item__) -> "list_text_str_structure_item__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_text_cstr_class_field__) -> "list_text_cstr_class_field__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_text_csig_class_sig_field__) -> "list_text_csig_class_sig_field__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_structure_element_) -> "list_structure_element_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_signature_element_) -> "list_signature_element_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_post_item_attribute_) -> "list_post_item_attribute_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_subst_kind__) -> "list_generic_and_type_declaration_type_subst_kind__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_kind__) -> "list_generic_and_type_declaration_type_kind__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_attribute_) -> "list_attribute_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_and_module_declaration_) -> "list_and_module_declaration_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_and_module_binding_) -> "list_and_module_binding_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_and_class_type_declaration_) -> "list_and_class_type_declaration_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_and_class_description_) -> "list_and_class_description_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_and_class_declaration_) -> "list_and_class_declaration_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_letop_bindings) -> "letop_bindings"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_letop_binding_body) -> "letop_binding_body"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_pattern) -> "let_pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_bindings_no_ext_) -> "let_bindings_no_ext_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_bindings_ext_) -> "let_bindings_ext_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_binding_body_no_punning) -> "let_binding_body_no_punning"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_binding_body) -> "let_binding_body"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pattern_pattern_no_exn_) -> "labeled_tuple_pattern_pattern_no_exn_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pattern_pattern_) -> "labeled_tuple_pattern_pattern_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pat_element_list_pattern_no_exn_) -> "labeled_tuple_pat_element_list_pattern_no_exn_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pat_element_list_pattern_) -> "labeled_tuple_pat_element_list_pattern_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr) -> "labeled_simple_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_longident) -> "label_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern) -> "label_let_pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_declarations) -> "label_declarations"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi) -> "label_declaration_semi"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_declaration) -> "label_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_item_extension) -> "item_extension"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_interface) -> "interface"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_index_mod) -> "index_mod"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_implementation) -> "implementation"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident) -> "ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_nonrec_flag_type_kind_) -> "generic_type_declaration_nonrec_flag_type_kind_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_no_nonrec_flag_type_subst_kind_) -> "generic_type_declaration_no_nonrec_flag_type_subst_kind_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_epsilon_) -> "generic_constructor_declaration_epsilon_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_BAR_) -> "generic_constructor_declaration_BAR_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments) -> "generalized_constructor_arguments"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_functor_args) -> "functor_args"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_functor_arg) -> "functor_arg"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_function_type) -> "function_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_fun_seq_expr) -> "fun_seq_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_fun_params) -> "fun_params"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_fun_param_as_list) -> "fun_param_as_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_fun_expr) -> "fun_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_fun_body) -> "fun_body"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_formal_class_parameters) -> "formal_class_parameters"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_floating_attribute) -> "floating_attribute"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_extension_type) -> "extension_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_epsilon_) -> "extension_constructor_rebind_epsilon_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_BAR_) -> "extension_constructor_rebind_BAR_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_extension) -> "extension"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ext) -> "ext"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expr_colon_package_type) -> "expr_colon_package_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_direction_flag) -> "direction_flag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_delimited_type_supporting_local_open) -> "delimited_type_supporting_local_open"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_delimited_type) -> "delimited_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_core_type) -> "core_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations) -> "constructor_declarations"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments) -> "constructor_arguments"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constrain_field) -> "constrain_field"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constr_longident) -> "constr_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constr_ident) -> "constr_ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constr_extra_nonprefix_ident) -> "constr_extra_nonprefix_ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constant) -> "constant"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_clty_longident) -> "clty_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations) -> "class_type_declarations"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_type) -> "class_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr) -> "class_simple_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_signature) -> "class_signature"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_sig_field) -> "class_sig_field"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_self_type) -> "class_self_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern) -> "class_self_pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_longident) -> "class_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_fun_def) -> "class_fun_def"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding) -> "class_fun_binding"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_field) -> "class_field"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_expr) -> "class_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_attribute) -> "attribute"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_attr_payload) -> "attr_payload"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_attr_id) -> "attr_id"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_atomic_type) -> "atomic_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_any_longident) -> "any_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_and_let_binding) -> "and_let_binding"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_alias_type) -> "alias_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_additive) -> "additive"

let print_value (type a) : a MenhirInterpreter.symbol -> a -> string = function
  | MenhirInterpreter.T MenhirInterpreter.T_error -> (fun _ -> "error")
  | MenhirInterpreter.T MenhirInterpreter.T_WITH -> (fun _ -> "with")
  | MenhirInterpreter.T MenhirInterpreter.T_WHILE -> (fun _ -> "while")
  | MenhirInterpreter.T MenhirInterpreter.T_WHEN -> (fun _ -> "when")
  | MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL -> (fun _ -> "virtual")
  | MenhirInterpreter.T MenhirInterpreter.T_VAL -> (fun _ -> "val")
  | MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE -> (fun _ -> "_")
  | MenhirInterpreter.T MenhirInterpreter.T_UIDENT -> (Printf.sprintf "UIDENT(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_TYPE -> (fun _ -> "type")
  | MenhirInterpreter.T MenhirInterpreter.T_TRY -> (fun _ -> "try")
  | MenhirInterpreter.T MenhirInterpreter.T_TRUE -> (fun _ -> "true")
  | MenhirInterpreter.T MenhirInterpreter.T_TO -> (fun _ -> "to")
  | MenhirInterpreter.T MenhirInterpreter.T_TILDE -> (fun _ -> "~")
  | MenhirInterpreter.T MenhirInterpreter.T_THEN -> (fun _ -> "then")
  | MenhirInterpreter.T MenhirInterpreter.T_STRUCT -> (fun _ -> "struct")
  | MenhirInterpreter.T MenhirInterpreter.T_STRING -> (string_of_STRING)
  | MenhirInterpreter.T MenhirInterpreter.T_STAR -> (fun _ -> "*")
  | MenhirInterpreter.T MenhirInterpreter.T_SIG -> (fun _ -> "sig")
  | MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI -> (fun _ -> ";;")
  | MenhirInterpreter.T MenhirInterpreter.T_SEMI -> (fun _ -> ";")
  | MenhirInterpreter.T MenhirInterpreter.T_RPAREN -> (fun _ -> ")")
  | MenhirInterpreter.T MenhirInterpreter.T_REC -> (fun _ -> "rec")
  | MenhirInterpreter.T MenhirInterpreter.T_RBRACKET -> (fun _ -> "]")
  | MenhirInterpreter.T MenhirInterpreter.T_RBRACE -> (fun _ -> "}")
  | MenhirInterpreter.T MenhirInterpreter.T_QUOTED_STRING_ITEM -> (string_of_quoted_STRING)
  | MenhirInterpreter.T MenhirInterpreter.T_QUOTED_STRING_EXPR -> (string_of_quoted_STRING)
  | MenhirInterpreter.T MenhirInterpreter.T_QUOTE -> (fun _ -> "'")
  | MenhirInterpreter.T MenhirInterpreter.T_QUESTION -> (fun _ -> "?")
  | MenhirInterpreter.T MenhirInterpreter.T_PRIVATE -> (fun _ -> "private")
  | MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP -> (Printf.sprintf "PREFIXOP(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ -> (fun _ -> "+=")
  | MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT -> (fun _ -> "+.")
  | MenhirInterpreter.T MenhirInterpreter.T_PLUS -> (fun _ -> "+")
  | MenhirInterpreter.T MenhirInterpreter.T_PERCENT -> (fun _ -> "%")
  | MenhirInterpreter.T MenhirInterpreter.T_OR -> (fun _ -> "or")
  | MenhirInterpreter.T MenhirInterpreter.T_OPTLABEL -> (Printf.sprintf "OPTLABEL(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_OPEN -> (fun _ -> "open")
  | MenhirInterpreter.T MenhirInterpreter.T_OF -> (fun _ -> "of")
  | MenhirInterpreter.T MenhirInterpreter.T_OBJECT -> (fun _ -> "object")
  | MenhirInterpreter.T MenhirInterpreter.T_NONREC -> (fun _ -> "nonrec")
  | MenhirInterpreter.T MenhirInterpreter.T_NEW -> (fun _ -> "new")
  | MenhirInterpreter.T MenhirInterpreter.T_MUTABLE -> (fun _ -> "mutable")
  | MenhirInterpreter.T MenhirInterpreter.T_MODULE -> (fun _ -> "module")
  | MenhirInterpreter.T MenhirInterpreter.T_MINUSGREATER -> (fun _ -> "->")
  | MenhirInterpreter.T MenhirInterpreter.T_MINUSDOT -> (fun _ -> "-.")
  | MenhirInterpreter.T MenhirInterpreter.T_MINUS -> (fun _ -> "-")
  | MenhirInterpreter.T MenhirInterpreter.T_METHOD -> (fun _ -> "method")
  | MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_ESCAPE -> (fun _ -> ".~")
  | MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_BRACKET_OPEN -> (fun _ -> ".<")
  | MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_BRACKET_CLOSE -> (fun _ -> ">.")
  | MenhirInterpreter.T MenhirInterpreter.T_MATCH -> (fun _ -> "match")
  | MenhirInterpreter.T MenhirInterpreter.T_LPAREN -> (fun _ -> "(")
  | MenhirInterpreter.T MenhirInterpreter.T_LIDENT -> (Printf.sprintf "LIDENT(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_LETOP -> (fun _ -> "LETOP")
  | MenhirInterpreter.T MenhirInterpreter.T_LET -> (fun _ -> "let")
  | MenhirInterpreter.T MenhirInterpreter.T_LESSMINUS -> (fun _ -> "<-")
  | MenhirInterpreter.T MenhirInterpreter.T_LESS -> (fun _ -> "<")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENTPERCENT -> (fun _ -> "[%%")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENT -> (fun _ -> "[%")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETLESS -> (fun _ -> "[<")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETGREATER -> (fun _ -> "[>")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETBAR -> (fun _ -> "[|")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATATAT -> (fun _ -> "[@@@")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATAT -> (fun _ -> "[@@")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETAT -> (fun _ -> "[@")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKET -> (fun _ -> "[")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACELESS -> (fun _ -> "{<")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACE -> (fun _ -> "{")
  | MenhirInterpreter.T MenhirInterpreter.T_LAZY -> (fun _ -> "lazy")
  | MenhirInterpreter.T MenhirInterpreter.T_LABEL -> (Printf.sprintf "LABEL(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_INT -> (string_of_INT)
  | MenhirInterpreter.T MenhirInterpreter.T_INITIALIZER -> (fun _ -> "initializer")
  | MenhirInterpreter.T MenhirInterpreter.T_INHERIT -> (fun _ -> "inherit")
  | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP4 -> (Printf.sprintf "INFIXOP4(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP3 -> (Printf.sprintf "INFIXOP3(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP2 -> (Printf.sprintf "INFIXOP2(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP1 -> (Printf.sprintf "INFIXOP1(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP0 -> (Printf.sprintf "INFIXOP0(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_INCLUDE -> (fun _ -> "include")
  | MenhirInterpreter.T MenhirInterpreter.T_IN -> (fun _ -> "in")
  | MenhirInterpreter.T MenhirInterpreter.T_IF -> (fun _ -> "if")
  | MenhirInterpreter.T MenhirInterpreter.T_HASHOP -> (Printf.sprintf "HASHOP(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_HASH -> (fun _ -> "#")
  | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACKET -> (fun _ -> ">]")
  | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACE -> (fun _ -> ">}")
  | MenhirInterpreter.T MenhirInterpreter.T_GREATER -> (fun _ -> ">")
  | MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR -> (fun _ -> "functor")
  | MenhirInterpreter.T MenhirInterpreter.T_FUNCTION -> (fun _ -> "function")
  | MenhirInterpreter.T MenhirInterpreter.T_FUN -> (fun _ -> "fun")
  | MenhirInterpreter.T MenhirInterpreter.T_FOR -> (fun _ -> "for")
  | MenhirInterpreter.T MenhirInterpreter.T_FLOAT -> (string_of_FLOAT)
  | MenhirInterpreter.T MenhirInterpreter.T_FALSE -> (fun _ -> "false")
  | MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL -> (fun _ -> "external")
  | MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION -> (fun _ -> "exception")
  | MenhirInterpreter.T MenhirInterpreter.T_EQUAL -> (fun _ -> "=")
  | MenhirInterpreter.T MenhirInterpreter.T_EOL -> (fun _ -> "EOL")
  | MenhirInterpreter.T MenhirInterpreter.T_EOF -> (fun _ -> "EOF")
  | MenhirInterpreter.T MenhirInterpreter.T_END -> (fun _ -> "end")
  | MenhirInterpreter.T MenhirInterpreter.T_ELSE -> (fun _ -> "else")
  | MenhirInterpreter.T MenhirInterpreter.T_EFFECT -> (fun _ -> "effect")
  | MenhirInterpreter.T MenhirInterpreter.T_DOWNTO -> (fun _ -> "downto")
  | MenhirInterpreter.T MenhirInterpreter.T_DOTOP -> (fun _ -> "DOTOP")
  | MenhirInterpreter.T MenhirInterpreter.T_DOTDOT -> (fun _ -> "..")
  | MenhirInterpreter.T MenhirInterpreter.T_DOT -> (fun _ -> ".")
  | MenhirInterpreter.T MenhirInterpreter.T_DONE -> (fun _ -> "done")
  | MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING -> (fun _ -> "DOCSTRING")
  | MenhirInterpreter.T MenhirInterpreter.T_DO -> (fun _ -> "do")
  | MenhirInterpreter.T MenhirInterpreter.T_CONSTRAINT -> (fun _ -> "constraint")
  | MenhirInterpreter.T MenhirInterpreter.T_COMMENT -> (fun _ -> "COMMENT")
  | MenhirInterpreter.T MenhirInterpreter.T_COMMA -> (fun _ -> ",")
  | MenhirInterpreter.T MenhirInterpreter.T_COLONGREATER -> (fun _ -> ":>")
  | MenhirInterpreter.T MenhirInterpreter.T_COLONEQUAL -> (fun _ -> ":=")
  | MenhirInterpreter.T MenhirInterpreter.T_COLONCOLON -> (fun _ -> "::")
  | MenhirInterpreter.T MenhirInterpreter.T_COLON -> (fun _ -> ":")
  | MenhirInterpreter.T MenhirInterpreter.T_CLASS -> (fun _ -> "class")
  | MenhirInterpreter.T MenhirInterpreter.T_CHAR -> (fun _ -> "CHAR")
  | MenhirInterpreter.T MenhirInterpreter.T_BEGIN -> (fun _ -> "begin")
  | MenhirInterpreter.T MenhirInterpreter.T_BARRBRACKET -> (fun _ -> "|]")
  | MenhirInterpreter.T MenhirInterpreter.T_BARBAR -> (fun _ -> "||")
  | MenhirInterpreter.T MenhirInterpreter.T_BAR -> (fun _ -> "|")
  | MenhirInterpreter.T MenhirInterpreter.T_BANG -> (fun _ -> "!")
  | MenhirInterpreter.T MenhirInterpreter.T_BACKQUOTE -> (fun _ -> "`")
  | MenhirInterpreter.T MenhirInterpreter.T_ASSERT -> (fun _ -> "assert")
  | MenhirInterpreter.T MenhirInterpreter.T_AS -> (fun _ -> "as")
  | MenhirInterpreter.T MenhirInterpreter.T_ANDOP -> (fun _ -> "ANDOP")
  | MenhirInterpreter.T MenhirInterpreter.T_AND -> (fun _ -> "and")
  | MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND -> (fun _ -> "&")
  | MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER -> (fun _ -> "&&")
  | MenhirInterpreter.N MenhirInterpreter.N_with_type_binder -> (fun _ -> "with_type_binder")
  | MenhirInterpreter.N MenhirInterpreter.N_with_constraint -> (fun _ -> "with_constraint")
  | MenhirInterpreter.N MenhirInterpreter.N_virtual_with_private_flag -> (fun _ -> "virtual_with_private_flag")
  | MenhirInterpreter.N MenhirInterpreter.N_virtual_with_mutable_flag -> (fun _ -> "virtual_with_mutable_flag")
  | MenhirInterpreter.N MenhirInterpreter.N_virtual_flag -> (fun _ -> "virtual_flag")
  | MenhirInterpreter.N MenhirInterpreter.N_value_description -> (fun _ -> "value_description")
  | MenhirInterpreter.N MenhirInterpreter.N_value -> (fun _ -> "value")
  | MenhirInterpreter.N MenhirInterpreter.N_val_longident -> (fun _ -> "val_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_val_ident -> (fun _ -> "val_ident")
  | MenhirInterpreter.N MenhirInterpreter.N_val_extra_ident -> (fun _ -> "val_extra_ident")
  | MenhirInterpreter.N MenhirInterpreter.N_use_file -> (fun _ -> "use_file")
  | MenhirInterpreter.N MenhirInterpreter.N_type_variance -> (fun _ -> "type_variance")
  | MenhirInterpreter.N MenhirInterpreter.N_type_variable -> (fun _ -> "type_variable")
  | MenhirInterpreter.N MenhirInterpreter.N_type_parameters -> (fun _ -> "type_parameters")
  | MenhirInterpreter.N MenhirInterpreter.N_type_parameter -> (fun _ -> "type_parameter")
  | MenhirInterpreter.N MenhirInterpreter.N_type_longident -> (fun _ -> "type_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_type_kind -> (fun _ -> "type_kind")
  | MenhirInterpreter.N MenhirInterpreter.N_type_constraint -> (fun _ -> "type_constraint")
  | MenhirInterpreter.N MenhirInterpreter.N_tuple_type -> (fun _ -> "tuple_type")
  | MenhirInterpreter.N MenhirInterpreter.N_toplevel_phrase -> (fun _ -> "toplevel_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_toplevel_directive -> (fun _ -> "toplevel_directive")
  | MenhirInterpreter.N MenhirInterpreter.N_tag_field -> (fun _ -> "tag_field")
  | MenhirInterpreter.N MenhirInterpreter.N_subtractive -> (fun _ -> "subtractive")
  | MenhirInterpreter.N MenhirInterpreter.N_structure_item -> (fun _ -> "structure_item")
  | MenhirInterpreter.N MenhirInterpreter.N_structure -> (fun _ -> "structure")
  | MenhirInterpreter.N MenhirInterpreter.N_strict_binding -> (fun _ -> "strict_binding")
  | MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration -> (fun _ -> "str_exception_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_single_attr_id -> (fun _ -> "single_attr_id")
  | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident -> (fun _ -> "simple_pattern_not_ident")
  | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern -> (fun _ -> "simple_pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_simple_param_pattern -> (fun _ -> "simple_param_pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_simple_expr -> (fun _ -> "simple_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern -> (fun _ -> "simple_delimited_pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_signed_constant -> (fun _ -> "signed_constant")
  | MenhirInterpreter.N MenhirInterpreter.N_signature_item -> (fun _ -> "signature_item")
  | MenhirInterpreter.N MenhirInterpreter.N_signature -> (fun _ -> "signature")
  | MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration -> (fun _ -> "sig_exception_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_seq_expr -> (fun _ -> "seq_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_record_expr_field_ -> (fun _ -> "separated_or_terminated_nonempty_list_SEMI_record_expr_field_")
  | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_pattern_ -> (fun _ -> "separated_or_terminated_nonempty_list_SEMI_pattern_")
  | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_object_expr_field_ -> (fun _ -> "separated_or_terminated_nonempty_list_SEMI_object_expr_field_")
  | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_expr_ -> (fun _ -> "separated_or_terminated_nonempty_list_SEMI_expr_")
  | MenhirInterpreter.N MenhirInterpreter.N_row_field -> (fun _ -> "row_field")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_core_type_ -> (fun _ -> "reversed_separated_nontrivial_llist_COMMA_core_type_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ -> (fun _ -> "reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_atomic_type_ -> (fun _ -> "reversed_separated_nonempty_llist_STAR_atomic_type_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_type_parameter_ -> (fun _ -> "reversed_separated_nonempty_llist_COMMA_type_parameter_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_core_type_ -> (fun _ -> "reversed_separated_nonempty_llist_COMMA_core_type_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_BAR_row_field_ -> (fun _ -> "reversed_separated_nonempty_llist_BAR_row_field_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AND_with_constraint_ -> (fun _ -> "reversed_separated_nonempty_llist_AND_with_constraint_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ -> (fun _ -> "reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ -> (fun _ -> "reversed_preceded_or_separated_nonempty_llist_BAR_match_case_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_typevar_ -> (fun _ -> "reversed_nonempty_llist_typevar_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_name_tag_ -> (fun _ -> "reversed_nonempty_llist_name_tag_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_labeled_simple_expr_ -> (fun _ -> "reversed_nonempty_llist_labeled_simple_expr_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_functor_arg_ -> (fun _ -> "reversed_nonempty_llist_functor_arg_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_concat_fun_param_as_list_ -> (fun _ -> "reversed_nonempty_concat_fun_param_as_list_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_preceded_CONSTRAINT_constrain__ -> (fun _ -> "reversed_llist_preceded_CONSTRAINT_constrain__")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_pattern_pattern_no_exn_ -> (fun _ -> "reversed_labeled_tuple_pattern_pattern_no_exn_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_pattern_pattern_ -> (fun _ -> "reversed_labeled_tuple_pattern_pattern_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_body -> (fun _ -> "reversed_labeled_tuple_body")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_declaration_ -> (fun _ -> "reversed_bar_llist_extension_constructor_declaration_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_ -> (fun _ -> "reversed_bar_llist_extension_constructor_")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_constructor_declaration_ -> (fun _ -> "reversed_bar_llist_constructor_declaration_")
  | MenhirInterpreter.N MenhirInterpreter.N_record_expr_content -> (fun _ -> "record_expr_content")
  | MenhirInterpreter.N MenhirInterpreter.N_rec_flag -> (fun _ -> "rec_flag")
  | MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags -> (fun _ -> "private_virtual_flags")
  | MenhirInterpreter.N MenhirInterpreter.N_private_flag -> (fun _ -> "private_flag")
  | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration -> (fun _ -> "primitive_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute -> (fun _ -> "post_item_attribute")
  | MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_no_attr_ -> (fun _ -> "possibly_poly_core_type_no_attr_")
  | MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_ -> (fun _ -> "possibly_poly_core_type_")
  | MenhirInterpreter.N MenhirInterpreter.N_payload -> (fun _ -> "payload")
  | MenhirInterpreter.N MenhirInterpreter.N_pattern_var -> (fun _ -> "pattern_var")
  | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn -> (fun _ -> "pattern_no_exn")
  | MenhirInterpreter.N MenhirInterpreter.N_pattern_gen -> (fun _ -> "pattern_gen")
  | MenhirInterpreter.N MenhirInterpreter.N_pattern -> (fun _ -> "pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_parse_val_longident -> (fun _ -> "parse_val_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_parse_pattern -> (fun _ -> "parse_pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_parse_mty_longident -> (fun _ -> "parse_mty_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_parse_module_type -> (fun _ -> "parse_module_type")
  | MenhirInterpreter.N MenhirInterpreter.N_parse_module_expr -> (fun _ -> "parse_module_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_parse_mod_longident -> (fun _ -> "parse_mod_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_parse_mod_ext_longident -> (fun _ -> "parse_mod_ext_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> (fun _ -> "parse_expression")
  | MenhirInterpreter.N MenhirInterpreter.N_parse_core_type -> (fun _ -> "parse_core_type")
  | MenhirInterpreter.N MenhirInterpreter.N_parse_constr_longident -> (fun _ -> "parse_constr_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_parse_any_longident -> (fun _ -> "parse_any_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr -> (fun _ -> "paren_module_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_optlabel -> (fun _ -> "optlabel")
  | MenhirInterpreter.N MenhirInterpreter.N_option_type_constraint_ -> (fun _ -> "option_type_constraint_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_seq_expr__ -> (fun _ -> "option_preceded_EQUAL_seq_expr__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_pattern__ -> (fun _ -> "option_preceded_EQUAL_pattern__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_module_type__ -> (fun _ -> "option_preceded_EQUAL_module_type__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_expr__ -> (fun _ -> "option_preceded_EQUAL_expr__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_core_type__ -> (fun _ -> "option_preceded_COLON_core_type__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_atomic_type__ -> (fun _ -> "option_preceded_COLON_atomic_type__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_AS_mkrhs_LIDENT___ -> (fun _ -> "option_preceded_AS_mkrhs_LIDENT___")
  | MenhirInterpreter.N MenhirInterpreter.N_option_SEMI_ -> (fun _ -> "option_SEMI_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_BAR_ -> (fun _ -> "option_BAR_")
  | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> (fun _ -> "opt_ampersand")
  | MenhirInterpreter.N MenhirInterpreter.N_operator -> (fun _ -> "operator")
  | MenhirInterpreter.N MenhirInterpreter.N_open_description -> (fun _ -> "open_description")
  | MenhirInterpreter.N MenhirInterpreter.N_open_declaration -> (fun _ -> "open_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_object_type -> (fun _ -> "object_type")
  | MenhirInterpreter.N MenhirInterpreter.N_nonempty_type_kind -> (fun _ -> "nonempty_type_kind")
  | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_raw_string_ -> (fun _ -> "nonempty_list_raw_string_")
  | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_mkrhs_LIDENT__ -> (fun _ -> "nonempty_list_mkrhs_LIDENT__")
  | MenhirInterpreter.N MenhirInterpreter.N_name_tag -> (fun _ -> "name_tag")
  | MenhirInterpreter.N MenhirInterpreter.N_mutable_virtual_flags -> (fun _ -> "mutable_virtual_flags")
  | MenhirInterpreter.N MenhirInterpreter.N_mutable_flag -> (fun _ -> "mutable_flag")
  | MenhirInterpreter.N MenhirInterpreter.N_mty_longident -> (fun _ -> "mty_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_module_type_subst -> (fun _ -> "module_type_subst")
  | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration -> (fun _ -> "module_type_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_module_type -> (fun _ -> "module_type")
  | MenhirInterpreter.N MenhirInterpreter.N_module_subst -> (fun _ -> "module_subst")
  | MenhirInterpreter.N MenhirInterpreter.N_module_name -> (fun _ -> "module_name")
  | MenhirInterpreter.N MenhirInterpreter.N_module_expr -> (fun _ -> "module_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body -> (fun _ -> "module_declaration_body")
  | MenhirInterpreter.N MenhirInterpreter.N_module_binding_body -> (fun _ -> "module_binding_body")
  | MenhirInterpreter.N MenhirInterpreter.N_mod_longident -> (fun _ -> "mod_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident -> (fun _ -> "mod_ext_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_val_ident_ -> (fun _ -> "mk_longident_mod_longident_val_ident_")
  | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_UIDENT_ -> (fun _ -> "mk_longident_mod_longident_UIDENT_")
  | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_LIDENT_ -> (fun _ -> "mk_longident_mod_longident_LIDENT_")
  | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_ident_ -> (fun _ -> "mk_longident_mod_ext_longident_ident_")
  | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident___anonymous_43_ -> (fun _ -> "mk_longident_mod_ext_longident___anonymous_43_")
  | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_UIDENT_ -> (fun _ -> "mk_longident_mod_ext_longident_UIDENT_")
  | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_LIDENT_ -> (fun _ -> "mk_longident_mod_ext_longident_LIDENT_")
  | MenhirInterpreter.N MenhirInterpreter.N_method_ -> (fun _ -> "method_")
  | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> (fun _ -> "meth_list")
  | MenhirInterpreter.N MenhirInterpreter.N_match_case -> (fun _ -> "match_case")
  | MenhirInterpreter.N MenhirInterpreter.N_local_structure_item -> (fun _ -> "local_structure_item")
  | MenhirInterpreter.N MenhirInterpreter.N_listx_SEMI_record_pat_field_UNDERSCORE_ -> (fun _ -> "listx_SEMI_record_pat_field_UNDERSCORE_")
  | MenhirInterpreter.N MenhirInterpreter.N_list_use_file_element_ -> (fun _ -> "list_use_file_element_")
  | MenhirInterpreter.N MenhirInterpreter.N_list_text_str_structure_item__ -> (fun _ -> "list_text_str_structure_item__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_text_cstr_class_field__ -> (fun _ -> "list_text_cstr_class_field__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_text_csig_class_sig_field__ -> (fun _ -> "list_text_csig_class_sig_field__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_structure_element_ -> (fun _ -> "list_structure_element_")
  | MenhirInterpreter.N MenhirInterpreter.N_list_signature_element_ -> (fun _ -> "list_signature_element_")
  | MenhirInterpreter.N MenhirInterpreter.N_list_post_item_attribute_ -> (fun _ -> "list_post_item_attribute_")
  | MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_subst_kind__ -> (fun _ -> "list_generic_and_type_declaration_type_subst_kind__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_kind__ -> (fun _ -> "list_generic_and_type_declaration_type_kind__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_attribute_ -> (fun _ -> "list_attribute_")
  | MenhirInterpreter.N MenhirInterpreter.N_list_and_module_declaration_ -> (fun _ -> "list_and_module_declaration_")
  | MenhirInterpreter.N MenhirInterpreter.N_list_and_module_binding_ -> (fun _ -> "list_and_module_binding_")
  | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_type_declaration_ -> (fun _ -> "list_and_class_type_declaration_")
  | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_description_ -> (fun _ -> "list_and_class_description_")
  | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_declaration_ -> (fun _ -> "list_and_class_declaration_")
  | MenhirInterpreter.N MenhirInterpreter.N_letop_bindings -> (fun _ -> "letop_bindings")
  | MenhirInterpreter.N MenhirInterpreter.N_letop_binding_body -> (fun _ -> "letop_binding_body")
  | MenhirInterpreter.N MenhirInterpreter.N_let_pattern -> (fun _ -> "let_pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_no_ext_ -> (fun _ -> "let_bindings_no_ext_")
  | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_ext_ -> (fun _ -> "let_bindings_ext_")
  | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body_no_punning -> (fun _ -> "let_binding_body_no_punning")
  | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body -> (fun _ -> "let_binding_body")
  | MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pattern_pattern_no_exn_ -> (fun _ -> "labeled_tuple_pattern_pattern_no_exn_")
  | MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pattern_pattern_ -> (fun _ -> "labeled_tuple_pattern_pattern_")
  | MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pat_element_list_pattern_no_exn_ -> (fun _ -> "labeled_tuple_pat_element_list_pattern_no_exn_")
  | MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pat_element_list_pattern_ -> (fun _ -> "labeled_tuple_pat_element_list_pattern_")
  | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr -> (fun _ -> "labeled_simple_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_label_longident -> (fun _ -> "label_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern -> (fun _ -> "label_let_pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_label_declarations -> (fun _ -> "label_declarations")
  | MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi -> (fun _ -> "label_declaration_semi")
  | MenhirInterpreter.N MenhirInterpreter.N_label_declaration -> (fun _ -> "label_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_item_extension -> (fun _ -> "item_extension")
  | MenhirInterpreter.N MenhirInterpreter.N_interface -> (fun _ -> "interface")
  | MenhirInterpreter.N MenhirInterpreter.N_index_mod -> (fun _ -> "index_mod")
  | MenhirInterpreter.N MenhirInterpreter.N_implementation -> (fun _ -> "implementation")
  | MenhirInterpreter.N MenhirInterpreter.N_ident -> (fun _ -> "ident")
  | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_nonrec_flag_type_kind_ -> (fun _ -> "generic_type_declaration_nonrec_flag_type_kind_")
  | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_no_nonrec_flag_type_subst_kind_ -> (fun _ -> "generic_type_declaration_no_nonrec_flag_type_subst_kind_")
  | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_epsilon_ -> (fun _ -> "generic_constructor_declaration_epsilon_")
  | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_BAR_ -> (fun _ -> "generic_constructor_declaration_BAR_")
  | MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments -> (fun _ -> "generalized_constructor_arguments")
  | MenhirInterpreter.N MenhirInterpreter.N_functor_args -> (fun _ -> "functor_args")
  | MenhirInterpreter.N MenhirInterpreter.N_functor_arg -> (fun _ -> "functor_arg")
  | MenhirInterpreter.N MenhirInterpreter.N_function_type -> (fun _ -> "function_type")
  | MenhirInterpreter.N MenhirInterpreter.N_fun_seq_expr -> (fun _ -> "fun_seq_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_fun_params -> (fun _ -> "fun_params")
  | MenhirInterpreter.N MenhirInterpreter.N_fun_param_as_list -> (fun _ -> "fun_param_as_list")
  | MenhirInterpreter.N MenhirInterpreter.N_fun_expr -> (fun _ -> "fun_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_fun_body -> (fun _ -> "fun_body")
  | MenhirInterpreter.N MenhirInterpreter.N_formal_class_parameters -> (fun _ -> "formal_class_parameters")
  | MenhirInterpreter.N MenhirInterpreter.N_floating_attribute -> (fun _ -> "floating_attribute")
  | MenhirInterpreter.N MenhirInterpreter.N_extension_type -> (fun _ -> "extension_type")
  | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_epsilon_ -> (fun _ -> "extension_constructor_rebind_epsilon_")
  | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_BAR_ -> (fun _ -> "extension_constructor_rebind_BAR_")
  | MenhirInterpreter.N MenhirInterpreter.N_extension -> (fun _ -> "extension")
  | MenhirInterpreter.N MenhirInterpreter.N_ext -> (fun _ -> "ext")
  | MenhirInterpreter.N MenhirInterpreter.N_expr_colon_package_type -> (fun _ -> "expr_colon_package_type")
  | MenhirInterpreter.N MenhirInterpreter.N_direction_flag -> (fun _ -> "direction_flag")
  | MenhirInterpreter.N MenhirInterpreter.N_delimited_type_supporting_local_open -> (fun _ -> "delimited_type_supporting_local_open")
  | MenhirInterpreter.N MenhirInterpreter.N_delimited_type -> (fun _ -> "delimited_type")
  | MenhirInterpreter.N MenhirInterpreter.N_core_type -> (fun _ -> "core_type")
  | MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations -> (fun _ -> "constructor_declarations")
  | MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments -> (fun _ -> "constructor_arguments")
  | MenhirInterpreter.N MenhirInterpreter.N_constrain_field -> (fun _ -> "constrain_field")
  | MenhirInterpreter.N MenhirInterpreter.N_constr_longident -> (fun _ -> "constr_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_constr_ident -> (fun _ -> "constr_ident")
  | MenhirInterpreter.N MenhirInterpreter.N_constr_extra_nonprefix_ident -> (fun _ -> "constr_extra_nonprefix_ident")
  | MenhirInterpreter.N MenhirInterpreter.N_constant -> (fun _ -> "constant")
  | MenhirInterpreter.N MenhirInterpreter.N_clty_longident -> (fun _ -> "clty_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations -> (fun _ -> "class_type_declarations")
  | MenhirInterpreter.N MenhirInterpreter.N_class_type -> (fun _ -> "class_type")
  | MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr -> (fun _ -> "class_simple_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_class_signature -> (fun _ -> "class_signature")
  | MenhirInterpreter.N MenhirInterpreter.N_class_sig_field -> (fun _ -> "class_sig_field")
  | MenhirInterpreter.N MenhirInterpreter.N_class_self_type -> (fun _ -> "class_self_type")
  | MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern -> (fun _ -> "class_self_pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_class_longident -> (fun _ -> "class_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_class_fun_def -> (fun _ -> "class_fun_def")
  | MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding -> (fun _ -> "class_fun_binding")
  | MenhirInterpreter.N MenhirInterpreter.N_class_field -> (fun _ -> "class_field")
  | MenhirInterpreter.N MenhirInterpreter.N_class_expr -> (fun _ -> "class_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_attribute -> (fun _ -> "attribute")
  | MenhirInterpreter.N MenhirInterpreter.N_attr_payload -> (fun _ -> "attr_payload")
  | MenhirInterpreter.N MenhirInterpreter.N_attr_id -> (fun _ -> "attr_id")
  | MenhirInterpreter.N MenhirInterpreter.N_atomic_type -> (fun _ -> "atomic_type")
  | MenhirInterpreter.N MenhirInterpreter.N_any_longident -> (fun _ -> "any_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_and_let_binding -> (fun _ -> "and_let_binding")
  | MenhirInterpreter.N MenhirInterpreter.N_alias_type -> (fun _ -> "alias_type")
  | MenhirInterpreter.N MenhirInterpreter.N_additive -> (fun _ -> "additive")

let print_token = function
  | WITH -> print_value (MenhirInterpreter.T MenhirInterpreter.T_WITH) ()
  | WHILE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_WHILE) ()
  | WHEN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_WHEN) ()
  | VIRTUAL -> print_value (MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL) ()
  | VAL -> print_value (MenhirInterpreter.T MenhirInterpreter.T_VAL) ()
  | UNDERSCORE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE) ()
  | UIDENT v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_UIDENT) v
  | TYPE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_TYPE) ()
  | TRY -> print_value (MenhirInterpreter.T MenhirInterpreter.T_TRY) ()
  | TRUE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_TRUE) ()
  | TO -> print_value (MenhirInterpreter.T MenhirInterpreter.T_TO) ()
  | TILDE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_TILDE) ()
  | THEN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_THEN) ()
  | STRUCT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_STRUCT) ()
  | STRING v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_STRING) v
  | STAR -> print_value (MenhirInterpreter.T MenhirInterpreter.T_STAR) ()
  | SIG -> print_value (MenhirInterpreter.T MenhirInterpreter.T_SIG) ()
  | SEMISEMI -> print_value (MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI) ()
  | SEMI -> print_value (MenhirInterpreter.T MenhirInterpreter.T_SEMI) ()
  | RPAREN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_RPAREN) ()
  | REC -> print_value (MenhirInterpreter.T MenhirInterpreter.T_REC) ()
  | RBRACKET -> print_value (MenhirInterpreter.T MenhirInterpreter.T_RBRACKET) ()
  | RBRACE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_RBRACE) ()
  | QUOTED_STRING_ITEM v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_QUOTED_STRING_ITEM) v
  | QUOTED_STRING_EXPR v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_QUOTED_STRING_EXPR) v
  | QUOTE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_QUOTE) ()
  | QUESTION -> print_value (MenhirInterpreter.T MenhirInterpreter.T_QUESTION) ()
  | PRIVATE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_PRIVATE) ()
  | PREFIXOP v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP) v
  | PLUSEQ -> print_value (MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ) ()
  | PLUSDOT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT) ()
  | PLUS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_PLUS) ()
  | PERCENT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_PERCENT) ()
  | OR -> print_value (MenhirInterpreter.T MenhirInterpreter.T_OR) ()
  | OPTLABEL v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_OPTLABEL) v
  | OPEN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_OPEN) ()
  | OF -> print_value (MenhirInterpreter.T MenhirInterpreter.T_OF) ()
  | OBJECT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_OBJECT) ()
  | NONREC -> print_value (MenhirInterpreter.T MenhirInterpreter.T_NONREC) ()
  | NEW -> print_value (MenhirInterpreter.T MenhirInterpreter.T_NEW) ()
  | MUTABLE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_MUTABLE) ()
  | MODULE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_MODULE) ()
  | MINUSGREATER -> print_value (MenhirInterpreter.T MenhirInterpreter.T_MINUSGREATER) ()
  | MINUSDOT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_MINUSDOT) ()
  | MINUS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_MINUS) ()
  | METHOD -> print_value (MenhirInterpreter.T MenhirInterpreter.T_METHOD) ()
  | METAOCAML_ESCAPE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_ESCAPE) ()
  | METAOCAML_BRACKET_OPEN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_BRACKET_OPEN) ()
  | METAOCAML_BRACKET_CLOSE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_BRACKET_CLOSE) ()
  | MATCH -> print_value (MenhirInterpreter.T MenhirInterpreter.T_MATCH) ()
  | LPAREN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LPAREN) ()
  | LIDENT v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LIDENT) v
  | LETOP v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LETOP) v
  | LET -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LET) ()
  | LESSMINUS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LESSMINUS) ()
  | LESS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LESS) ()
  | LBRACKETPERCENTPERCENT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENTPERCENT) ()
  | LBRACKETPERCENT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENT) ()
  | LBRACKETLESS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETLESS) ()
  | LBRACKETGREATER -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETGREATER) ()
  | LBRACKETBAR -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETBAR) ()
  | LBRACKETATATAT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATATAT) ()
  | LBRACKETATAT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATAT) ()
  | LBRACKETAT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETAT) ()
  | LBRACKET -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKET) ()
  | LBRACELESS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACELESS) ()
  | LBRACE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACE) ()
  | LAZY -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LAZY) ()
  | LABEL v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LABEL) v
  | INT v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INT) v
  | INITIALIZER -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INITIALIZER) ()
  | INHERIT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INHERIT) ()
  | INFIXOP4 v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP4) v
  | INFIXOP3 v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP3) v
  | INFIXOP2 v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP2) v
  | INFIXOP1 v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP1) v
  | INFIXOP0 v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP0) v
  | INCLUDE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INCLUDE) ()
  | IN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_IN) ()
  | IF -> print_value (MenhirInterpreter.T MenhirInterpreter.T_IF) ()
  | HASHOP v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_HASHOP) v
  | HASH -> print_value (MenhirInterpreter.T MenhirInterpreter.T_HASH) ()
  | GREATERRBRACKET -> print_value (MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACKET) ()
  | GREATERRBRACE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACE) ()
  | GREATER -> print_value (MenhirInterpreter.T MenhirInterpreter.T_GREATER) ()
  | FUNCTOR -> print_value (MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR) ()
  | FUNCTION -> print_value (MenhirInterpreter.T MenhirInterpreter.T_FUNCTION) ()
  | FUN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_FUN) ()
  | FOR -> print_value (MenhirInterpreter.T MenhirInterpreter.T_FOR) ()
  | FLOAT v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_FLOAT) v
  | FALSE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_FALSE) ()
  | EXTERNAL -> print_value (MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL) ()
  | EXCEPTION -> print_value (MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION) ()
  | EQUAL -> print_value (MenhirInterpreter.T MenhirInterpreter.T_EQUAL) ()
  | EOL -> print_value (MenhirInterpreter.T MenhirInterpreter.T_EOL) ()
  | EOF -> print_value (MenhirInterpreter.T MenhirInterpreter.T_EOF) ()
  | END -> print_value (MenhirInterpreter.T MenhirInterpreter.T_END) ()
  | ELSE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_ELSE) ()
  | EFFECT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_EFFECT) ()
  | DOWNTO -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DOWNTO) ()
  | DOTOP v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DOTOP) v
  | DOTDOT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DOTDOT) ()
  | DOT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DOT) ()
  | DONE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DONE) ()
  | DOCSTRING v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING) v
  | DO -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DO) ()
  | CONSTRAINT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_CONSTRAINT) ()
  | COMMENT v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_COMMENT) v
  | COMMA -> print_value (MenhirInterpreter.T MenhirInterpreter.T_COMMA) ()
  | COLONGREATER -> print_value (MenhirInterpreter.T MenhirInterpreter.T_COLONGREATER) ()
  | COLONEQUAL -> print_value (MenhirInterpreter.T MenhirInterpreter.T_COLONEQUAL) ()
  | COLONCOLON -> print_value (MenhirInterpreter.T MenhirInterpreter.T_COLONCOLON) ()
  | COLON -> print_value (MenhirInterpreter.T MenhirInterpreter.T_COLON) ()
  | CLASS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_CLASS) ()
  | CHAR v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_CHAR) v
  | BEGIN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_BEGIN) ()
  | BARRBRACKET -> print_value (MenhirInterpreter.T MenhirInterpreter.T_BARRBRACKET) ()
  | BARBAR -> print_value (MenhirInterpreter.T MenhirInterpreter.T_BARBAR) ()
  | BAR -> print_value (MenhirInterpreter.T MenhirInterpreter.T_BAR) ()
  | BANG -> print_value (MenhirInterpreter.T MenhirInterpreter.T_BANG) ()
  | BACKQUOTE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_BACKQUOTE) ()
  | ASSERT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_ASSERT) ()
  | AS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_AS) ()
  | ANDOP v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_ANDOP) v
  | AND -> print_value (MenhirInterpreter.T MenhirInterpreter.T_AND) ()
  | AMPERSAND -> print_value (MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND) ()
  | AMPERAMPER -> print_value (MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER) ()

let symbol_of_index = function
  | 0 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_error))
  | 1 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_WITH))
  | 2 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_WHILE))
  | 3 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_WHEN))
  | 4 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL))
  | 5 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_VAL))
  | 6 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE))
  | 7 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_UIDENT))
  | 8 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TYPE))
  | 9 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TRY))
  | 10 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TRUE))
  | 11 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TO))
  | 12 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TILDE))
  | 13 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_THEN))
  | 14 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_STRUCT))
  | 15 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_STRING))
  | 16 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_STAR))
  | 17 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_SIG))
  | 18 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI))
  | 19 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_SEMI))
  | 20 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_RPAREN))
  | 21 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_REC))
  | 22 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_RBRACKET))
  | 23 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_RBRACE))
  | 24 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_QUOTED_STRING_ITEM))
  | 25 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_QUOTED_STRING_EXPR))
  | 26 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_QUOTE))
  | 27 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_QUESTION))
  | 28 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PRIVATE))
  | 29 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP))
  | 30 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ))
  | 31 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT))
  | 32 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PLUS))
  | 33 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PERCENT))
  | 34 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OR))
  | 35 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OPTLABEL))
  | 36 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OPEN))
  | 37 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OF))
  | 38 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OBJECT))
  | 39 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_NONREC))
  | 40 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_NEW))
  | 41 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MUTABLE))
  | 42 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MODULE))
  | 43 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MINUSGREATER))
  | 44 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MINUSDOT))
  | 45 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MINUS))
  | 46 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_METHOD))
  | 47 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_ESCAPE))
  | 48 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_BRACKET_OPEN))
  | 49 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_BRACKET_CLOSE))
  | 50 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MATCH))
  | 51 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LPAREN))
  | 52 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LIDENT))
  | 53 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LETOP))
  | 54 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LET))
  | 55 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LESSMINUS))
  | 56 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LESS))
  | 57 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENTPERCENT))
  | 58 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENT))
  | 59 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETLESS))
  | 60 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETGREATER))
  | 61 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETBAR))
  | 62 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATATAT))
  | 63 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATAT))
  | 64 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETAT))
  | 65 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKET))
  | 66 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACELESS))
  | 67 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACE))
  | 68 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LAZY))
  | 69 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LABEL))
  | 70 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INT))
  | 71 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INITIALIZER))
  | 72 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INHERIT))
  | 73 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP4))
  | 74 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP3))
  | 75 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP2))
  | 76 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP1))
  | 77 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP0))
  | 78 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INCLUDE))
  | 79 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_IN))
  | 80 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_IF))
  | 81 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_HASHOP))
  | 82 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_HASH))
  | 83 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACKET))
  | 84 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACE))
  | 85 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_GREATER))
  | 86 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR))
  | 87 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FUNCTION))
  | 88 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FUN))
  | 89 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FOR))
  | 90 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FLOAT))
  | 91 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FALSE))
  | 92 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL))
  | 93 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION))
  | 94 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EQUAL))
  | 95 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EOL))
  | 96 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EOF))
  | 97 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_END))
  | 98 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_ELSE))
  | 99 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EFFECT))
  | 100 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOWNTO))
  | 101 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOTOP))
  | 102 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOTDOT))
  | 103 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOT))
  | 104 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DONE))
  | 105 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING))
  | 106 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DO))
  | 107 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_CONSTRAINT))
  | 108 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COMMENT))
  | 109 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COMMA))
  | 110 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COLONGREATER))
  | 111 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COLONEQUAL))
  | 112 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COLONCOLON))
  | 113 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COLON))
  | 114 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_CLASS))
  | 115 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_CHAR))
  | 116 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BEGIN))
  | 117 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BARRBRACKET))
  | 118 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BARBAR))
  | 119 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BAR))
  | 120 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BANG))
  | 121 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BACKQUOTE))
  | 122 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_ASSERT))
  | 123 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_AS))
  | 124 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_ANDOP))
  | 125 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_AND))
  | 126 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND))
  | 127 -> Some (MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER))
  | 144 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_with_type_binder))
  | 145 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_with_constraint))
  | 146 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_virtual_with_private_flag))
  | 147 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_virtual_with_mutable_flag))
  | 148 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_virtual_flag))
  | 149 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_value_description))
  | 150 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_value))
  | 151 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_val_longident))
  | 152 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_val_ident))
  | 153 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_val_extra_ident))
  | 154 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_use_file))
  | 155 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_variance))
  | 156 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_variable))
  | 157 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_parameters))
  | 158 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_parameter))
  | 159 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_longident))
  | 160 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_kind))
  | 161 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_constraint))
  | 162 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_tuple_type))
  | 163 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_toplevel_phrase))
  | 164 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_toplevel_directive))
  | 165 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_tag_field))
  | 166 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_subtractive))
  | 167 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_structure_item))
  | 168 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_structure))
  | 169 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_strict_binding))
  | 170 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration))
  | 171 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_single_attr_id))
  | 172 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident))
  | 173 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_pattern))
  | 174 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_param_pattern))
  | 175 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_expr))
  | 176 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern))
  | 177 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_signed_constant))
  | 178 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_signature_item))
  | 179 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_signature))
  | 180 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration))
  | 181 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_seq_expr))
  | 182 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_record_expr_field_))
  | 183 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_pattern_))
  | 184 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_object_expr_field_))
  | 185 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_expr_))
  | 186 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_row_field))
  | 187 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_core_type_))
  | 188 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_))
  | 189 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_atomic_type_))
  | 190 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_type_parameter_))
  | 191 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_core_type_))
  | 192 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_BAR_row_field_))
  | 193 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AND_with_constraint_))
  | 194 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_))
  | 195 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_preceded_or_separated_nonempty_llist_BAR_match_case_))
  | 196 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_typevar_))
  | 197 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_name_tag_))
  | 198 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_labeled_simple_expr_))
  | 199 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_functor_arg_))
  | 200 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_concat_fun_param_as_list_))
  | 201 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_preceded_CONSTRAINT_constrain__))
  | 202 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_pattern_pattern_no_exn_))
  | 203 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_pattern_pattern_))
  | 204 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_body))
  | 205 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_declaration_))
  | 206 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_))
  | 207 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_constructor_declaration_))
  | 208 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_record_expr_content))
  | 209 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rec_flag))
  | 210 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags))
  | 211 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_private_flag))
  | 212 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration))
  | 213 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute))
  | 214 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_no_attr_))
  | 215 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_))
  | 216 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_payload))
  | 217 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern_var))
  | 218 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn))
  | 219 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern_gen))
  | 220 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern))
  | 221 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_val_longident))
  | 222 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_pattern))
  | 223 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_mty_longident))
  | 224 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_module_type))
  | 225 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_module_expr))
  | 226 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_mod_longident))
  | 227 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_mod_ext_longident))
  | 228 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_expression))
  | 229 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_core_type))
  | 230 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_constr_longident))
  | 231 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_any_longident))
  | 232 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr))
  | 233 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_optlabel))
  | 234 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_type_constraint_))
  | 235 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_seq_expr__))
  | 236 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_pattern__))
  | 237 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_module_type__))
  | 238 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_expr__))
  | 239 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_core_type__))
  | 240 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_atomic_type__))
  | 241 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_preceded_AS_mkrhs_LIDENT___))
  | 242 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_SEMI_))
  | 243 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_BAR_))
  | 244 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand))
  | 245 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_operator))
  | 246 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_open_description))
  | 247 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_open_declaration))
  | 248 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_object_type))
  | 249 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nonempty_type_kind))
  | 250 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_raw_string_))
  | 251 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_mkrhs_LIDENT__))
  | 252 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_name_tag))
  | 253 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mutable_virtual_flags))
  | 254 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mutable_flag))
  | 255 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mty_longident))
  | 256 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_type_subst))
  | 257 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration))
  | 258 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_type))
  | 259 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_subst))
  | 260 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_name))
  | 261 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_expr))
  | 262 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body))
  | 263 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_binding_body))
  | 264 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mod_longident))
  | 265 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident))
  | 266 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_val_ident_))
  | 267 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_UIDENT_))
  | 268 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_LIDENT_))
  | 269 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_ident_))
  | 270 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident___anonymous_43_))
  | 271 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_UIDENT_))
  | 272 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_LIDENT_))
  | 273 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_method_))
  | 274 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_meth_list))
  | 275 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_match_case))
  | 276 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_local_structure_item))
  | 277 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_listx_SEMI_record_pat_field_UNDERSCORE_))
  | 278 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_use_file_element_))
  | 279 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_text_str_structure_item__))
  | 280 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_text_cstr_class_field__))
  | 281 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_text_csig_class_sig_field__))
  | 282 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_structure_element_))
  | 283 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_signature_element_))
  | 284 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_post_item_attribute_))
  | 285 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_subst_kind__))
  | 286 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_kind__))
  | 287 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_attribute_))
  | 288 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_and_module_declaration_))
  | 289 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_and_module_binding_))
  | 290 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_and_class_type_declaration_))
  | 291 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_and_class_description_))
  | 292 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_and_class_declaration_))
  | 293 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_letop_bindings))
  | 294 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_letop_binding_body))
  | 295 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_pattern))
  | 296 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_bindings_no_ext_))
  | 297 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_bindings_ext_))
  | 298 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_binding_body_no_punning))
  | 299 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_binding_body))
  | 300 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pattern_pattern_no_exn_))
  | 301 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pattern_pattern_))
  | 302 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pat_element_list_pattern_no_exn_))
  | 303 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pat_element_list_pattern_))
  | 304 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr))
  | 305 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_longident))
  | 306 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern))
  | 307 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_declarations))
  | 308 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi))
  | 309 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_declaration))
  | 310 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_item_extension))
  | 311 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_interface))
  | 312 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_index_mod))
  | 313 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_implementation))
  | 314 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident))
  | 315 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_nonrec_flag_type_kind_))
  | 316 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_no_nonrec_flag_type_subst_kind_))
  | 317 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_epsilon_))
  | 318 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_BAR_))
  | 319 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments))
  | 320 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_functor_args))
  | 321 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_functor_arg))
  | 322 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_function_type))
  | 323 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_fun_seq_expr))
  | 324 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_fun_params))
  | 325 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_fun_param_as_list))
  | 326 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_fun_expr))
  | 327 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_fun_body))
  | 328 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_formal_class_parameters))
  | 329 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_floating_attribute))
  | 330 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_extension_type))
  | 331 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_epsilon_))
  | 332 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_BAR_))
  | 333 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_extension))
  | 334 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ext))
  | 335 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expr_colon_package_type))
  | 336 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_direction_flag))
  | 337 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_delimited_type_supporting_local_open))
  | 338 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_delimited_type))
  | 339 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_core_type))
  | 340 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations))
  | 341 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments))
  | 342 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constrain_field))
  | 343 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constr_longident))
  | 344 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constr_ident))
  | 345 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constr_extra_nonprefix_ident))
  | 346 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constant))
  | 347 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_clty_longident))
  | 348 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations))
  | 349 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_type))
  | 350 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr))
  | 351 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_signature))
  | 352 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_sig_field))
  | 353 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_self_type))
  | 354 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern))
  | 355 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_longident))
  | 356 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_fun_def))
  | 357 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding))
  | 358 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_field))
  | 359 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_expr))
  | 360 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_attribute))
  | 361 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_attr_payload))
  | 362 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_attr_id))
  | 363 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_atomic_type))
  | 364 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_any_longident))
  | 365 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_and_let_binding))
  | 366 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_alias_type))
  | 367 -> Some (MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_additive))
  | _ -> None

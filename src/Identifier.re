// JS identifier type and validation

let keywords =
  Belt.Set.String.fromArray([|
    "break",
    "case",
    "catch",
    "continue",
    "const",
    "let",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "finally",
    "for",
    "function",
    "if",
    "in",
    "instanceof",
    "new",
    "return",
    "switch",
    "this",
    "throw",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
  |]);

// Note: this regex is much more restrictive than the true set
// of valid identifiers, which includes unicode characters.
// This could be expanded
let identifierRegex = [%re {|/^[a-zA-Z$_][a-zA-Z0-9$_]*$/|}];

let isValidIdentifier = s =>
  !keywords->Belt.Set.String.has(s) && Js.Re.test_(identifierRegex, s);

exception InvalidIdentifier(string);

module Validation = {
  let checkError =
    Some(
      s =>
        if (isValidIdentifier(s)) {
          None;
        } else {
          Some(InvalidIdentifier(s));
        },
    );
};

module Identifier =
  Opaque.MakeString(
    Validation,
    {},
  );

include Identifier;

module Set = Opaque.MakeStringSet(Identifier);

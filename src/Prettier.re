type rawJS = string;
type prettyJS = string;

type options = {
  parser: string,
  trailingComma: string,
  semi: bool,
  singleQuote: bool,
  arrowParens: string,
  endOfLine: string,
};

let defaultOptions = {
  parser: "babel",
  trailingComma: "es5",
  semi: true,
  singleQuote: true,
  arrowParens: "avoid",
  endOfLine: "auto",
};

let options =
    (
      ~parser=defaultOptions.parser,
      ~trailingComma=defaultOptions.trailingComma,
      ~semi=defaultOptions.semi,
      ~singleQuote=defaultOptions.singleQuote,
      ~arrowParens=defaultOptions.arrowParens,
      ~endOfLine=defaultOptions.endOfLine,
      (),
    ) => {
  parser,
  trailingComma,
  semi,
  singleQuote,
  arrowParens,
  endOfLine,
};

[@bs.module "prettier"]
external prettierWithOptions: (rawJS, options) => prettyJS = "format";

let prettier = (~options=defaultOptions, rawJS) =>
  prettierWithOptions(rawJS, options);

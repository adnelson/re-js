// Minimal Jest bindings

type expect;

[@bs.val] external expect: 'a => expect = "expect";

[@bs.send] external toEqual: (expect, 'a) => unit = "toEqual";

[@bs.send]
external toMatchSnapshot: (expect, unit) => unit = "toMatchSnapshot";

[@bs.val] external describe: (string, (. unit) => unit) => unit = "describe";

// Gross hack but otherwise I get `describe does not expect any arguments`
//let describe: (string, (. unit) => unit) => unit = (name,

[@bs.val] external test: (string, (. unit) => unit) => unit = "test";

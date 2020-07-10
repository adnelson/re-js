// Minimal Jest bindings

module Expect = {
  type t;

  [@bs.val] external expect: 'a => t = "expect";
  [@bs.send] external toEqual: (t, 'a) => unit = "toEqual";
  [@bs.get] external not: t => t = "not";
  [@bs.send] external toMatchSnapshot: (t, unit) => unit = "toMatchSnapshot";
  [@bs.send] external toThrowSomething: t => unit = "toThrow";
};

[@bs.val] external describe: (string, (. unit) => unit) => unit = "describe";

[@bs.val] external test: (string, (. unit) => unit) => unit = "test";

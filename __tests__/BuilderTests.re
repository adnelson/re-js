open Jest;
open Expect;
open Builder;

test("identifier validation", (.) => {
  expect(() =>
    ident("iAmValid")
  )->not->toThrowSomething;
  expect(() =>
    ident("i am not valid")
  )->toThrowSomething;
});

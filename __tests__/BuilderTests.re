open Builder;
open Jest;
open Expect;

test("identifier validation", () => {
  expect(() =>
    ident("iAmValid")
  )->not->toThrowSomething;
  expect(() =>
    ident("iAmValid123")
  )->not->toThrowSomething;
  expect(() =>
    ident("CapitalNumber123")
  )->not->toThrowSomething;
  expect(() =>
    ident("dollar$ign$")
  )->not->toThrowSomething;
  expect(() =>
    ident("i am not valid")
  )->toThrowSomething;
});

open Examples;
open Jest;
open Expect;

// Snapshot-based testing, using the examples defined in `Examples`.
module SnapshotTests = {
  // Given some object and a way to translate it into a string, prettify the
  // string and run a jest test that checks it against a snapshot.
  let toJsonAndCheckSnapshot:
    'a.
    (string, Json.Encode.encoder('a), 'a) => unit
   =
    (exampleName, toJson, example) =>
      test(exampleName, () =>
        expect(example |> Utils.Json.prettyWith(toJson))->toMatchSnapshot()
      );

  Declaration.examples->Belt.Array.forEach(((name, ex)) =>
    toJsonAndCheckSnapshot(name, ToJson.declaration, ex)
  );

  Expr.examples->Belt.Array.forEach(((name, ex)) =>
    toJsonAndCheckSnapshot(name, ToJson.expr, ex)
  );

  Module.examples->Belt.Array.forEach(((name, ex)) =>
    toJsonAndCheckSnapshot(name, ToJson.module_, ex)
  );
};

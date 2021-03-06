open Examples;
open Jest;
open Expect;

// Snapshot-based testing, using the examples defined in `Examples`.
module SnapshotTests = {
  // Given some object and a way to translate it into a string, prettify the
  // string and run a jest test that checks it against a snapshot.
  let renderAndCheckSnapshot: 'a. (string, 'a => string, 'a) => unit =
    (exampleName, render, example) =>
      test(exampleName, () =>
        expect(example->render->Prettier.prettier |> Js.String.trim)
        ->toMatchSnapshot()
      );

  Declaration.examples->Belt.Array.forEach(((name, ex)) =>
    renderAndCheckSnapshot(name, Render.declaration, ex)
  );

  Expr.examples->Belt.Array.forEach(((name, ex)) =>
    renderAndCheckSnapshot(name, Render.expr, ex)
  );

  Statement.examples->Belt.Array.forEach(((name, ex)) =>
    renderAndCheckSnapshot(name, Render.statement, ex)
  );

  Module.examples->Belt.Array.forEach(((name, ex)) =>
    renderAndCheckSnapshot(name, Render.module_, ex)
  );

  Function.examples->Belt.Array.forEach(((name, f)) =>
    renderAndCheckSnapshot(name, Render.expr, f)
  );
};

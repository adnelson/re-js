open Examples;
open Jest;

module DeclareVar = {
  describe("declareVar", (.) => {
    test("example1", (.) => {
      let ex = DeclareVar.example1;
      expect(Render.declaration(ex))->toMatchSnapshot();
    })
  });
};

open Builder;

let pretty = Prettier.prettier;

module DeclareVar = {
  let example1 =
    declareVar("foo", `Binary(("+", string("hello "), string("world!"))));

  let example2 =
    declareVar("bar", `Ternary((var("foo"), string("x"), string("y"))));

  let examples = [|("example1", example1), ("example2", example2)|];
};

module Expr = {
  let classExample =
    class_(
      ~name="MyClass",
      ~extends="CoolClass"->var,
      [|
        functionProperty(
          "yo",
          [|"flarp"|],
          block1(
            `Expr(consoleDotLog([|string("yo there!"), var("flarp")|])),
          ),
        ),
        functionProperty(
          ~static=true,
          "beepBoop",
          [|"x", "y", "z"|],
          block1(`Expr(consoleDotLog([|string("static!")|]))),
        ),
        classVariable("blarp", arrowFunc1([|"boop"|], "xyz"->string)),
      |],
    );

  let reactComponentClassExample =
    reactComponentClass(
      ~name="BlankCopyScreen",
      ~pure=true,
      block1(
        `Return(
          Some(
            `Jsx(
              `Element(
                jsxElement(
                  "ScreenContainer",
                  ~props=[|
                    ("hasSafeArea", true->bool),
                    ("scrollable", false->bool),
                    ("style", dots("styles"->var, [|"screenContainerWD"|])),
                  |],
                  ~children=[|
                    `Element(
                      jsxElement(
                        "Text",
                        ~children=[|`String("Hello World!")|],
                      ),
                    ),
                  |],
                ),
              ),
            ),
          ),
        ),
      ),
    );

  let hello =
    arrowFunc(
      [||],
      [|
        `Declaration(DeclareVar.example1),
        `Declaration(DeclareVar.example2),
        `Expr(classExample),
        `Expr(reactComponentClassExample),
        `Expr(consoleDotLog1(string("hello world!"))),
        `Expr(call0(dots(var("foo"), [|"bar", "baz", "qux"|]))),
        `Expr(
          call2(
            dots(var("foo"), [|"bar", "baz", "qux"|]),
            int(6969),
            string("hello"),
          ),
        ),
        `Expr(
          arrowFunc(
            ~sync=`Async,
            [|"x"|],
            [|
              `Expr(
                consoleDotLog2(string("hello world!"), `Await(var("x"))),
              ),
              `Expr(
                consoleDotLog1(
                  `InterpolatedString([|
                    `S("hello world!"),
                    `E(var("x")),
                  |]),
                ),
              ),
            |],
          ),
        ),
        `Expr(
          `Jsx(
            `Element(
              jsxElement("Foo", ~props=[|("flab", `Number(123.4))|]),
            ),
          ),
        ),
        `Return(Some(array([|int(123), float(0.123), string("hi")|]))),
      |],
    );

  let examples = [|
    ("class", classExample),
    ("reactComponentClass", reactComponentClassExample),
    ("hello", hello),
  |];
};

module Imports = {
  let example1 = importDefault(~from="react", ~name="React");
  let example2 = importNames(~from="@foo/bar", [|"MyContainer"|]);
  let example3 = importNames(~from="react-native", [|"StyleSheet", "Text"|]);

  let examples = [|
    ("default", example1),
    ("single key", example2),
    ("two keys", example3),
  |];
};

module Module = {
  let example1 = {
    Ast.imports: Imports.examples->Utils.Array.snds,

    statements: [|
      `Statement(
        `Declaration(
          declareVar(
            "styles",
            dots("StyleSheet"->var, [|"create"|])
            ->call1(
                object1(
                  "screenContainerWD",
                  object2(
                    ("alignItems", "center"->string),
                    ("justifyContent", "center"->string),
                  ),
                ),
              ),
          ),
        ),
      ),
      exportDeclareVar("hello", Some(Expr.hello)),
    |],

    defaultExport: Some(`ExportExpr(var("hello"))),
  };

  let examples = [|("everything", example1)|];
};

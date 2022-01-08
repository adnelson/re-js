open Ast;

let int: int => expr = i => `Number(i->float_of_int);
let bool: bool => expr = b => `Bool(b);
let true_ = `Bool(true);
let false_ = `Bool(false);
let float: float => expr = f => `Number(f);
let string: string => expr = s => `String(s);
let array: array(expr) => expr = exprs => `Array(exprs);
let null: expr = `Null;
let undefined: expr = `Undefined;
let eq = (e1, e2) => `Binary(("===", e1, e2));
let neq = (e1, e2) => `Binary(("!==", e1, e2));
let refEq = (e1, e2) => `Binary(("==", e1, e2));
let notRefEq = (e1, e2) => `Binary(("!=", e1, e2));
let plus = (e1, e2) => `Binary(("+", e1, e2));
let minus = (e1, e2) => `Binary(("-", e1, e2));
let times = (e1, e2) => `Binary(("*", e1, e2));
let divide = (e1, e2) => `Binary(("/", e1, e2));
let and_ = (e1, e2): expr => `Binary(("&&", e1, e2));
let or_ = (e1, e2): expr => `Binary(("||", e1, e2));
let not = e => `Unary(("!", e));
let nullishCoalesce = (e1, e2) => `Binary(("??", e1, e2));
let unsafeRawExpression = raw => `UNSAFE_RAW_EXPRESSION(raw);

let isNullOrUndefined = (e): expr => refEq(e, null);
let isNotNullOrUndefined = (e): expr => notRefEq(e, null);

let ident = Identifier.fromString;

let idents = is => is->Belt.Array.map(ident);

let var = name => `Variable(name->ident);

let dot = (e, key: string) =>
  try(`Dot((e, key->ident))) {
  | Identifier.InvalidIdentifier(_) => `ArrayGet((e, key->string))
  };

let dots = (e, keyChain): expr => keyChain->Belt.Array.reduce(e, dot);

let questionDot = (e, key: string) =>
  try(`QuestionDot((e, key->ident))) {
  | Identifier.InvalidIdentifier(_) => e->and_(`ArrayGet((e, key->string)))
  };

let questionDots = (e, keyChain): expr =>
  keyChain->Belt.Array.reduce(e, questionDot);

// Like `dots` but an array of identifiers instead of strings
let dotsIdents = (e, keyChain: array(ident)): expr =>
  keyChain->Belt.Array.reduce(e, (e, i) => `Dot((e, i)));

let call = (f, args) => `Call((f, args));
let call0 = f => call(f, [||]);
let call1 = (f, x) => call(f, [|x|]);
let call2 = (f, x, y) => call(f, [|x, y|]);
let call3 = (f, e1, e2, e3) => call(f, [|e1, e2, e3|]);
let call4 = (f, e1, e2, e3, e4) => call(f, [|e1, e2, e3, e4|]);
let call5 = (f, e1, e2, e3, e4, e5) => call(f, [|e1, e2, e3, e4, e5|]);
let call6 = (f, e1, e2, e3, e4, e5, e6) =>
  call(f, [|e1, e2, e3, e4, e5, e6|]);

let consoleDotLog = args => `Call((dots(var("console"), [|"log"|]), args));
let consoleDotLog1 = e => consoleDotLog([|e|]);
let consoleDotLog2 = (e1, e2) => consoleDotLog([|e1, e2|]);

module Pattern = {
  let name = (~inner=?, n) => `Name((n->ident, inner));
  let names = ns => ns->Belt.Array.map(n => name(n));
  let array = patterns => `DestructureArray(patterns);
  let object_ = patterns => `DestructureObject(patterns);
};

let declareVar = (~kind=`Const, i, e): declaration =>
  `DeclareVar(({kind, pattern: i->Pattern.name}, Some(e)));

let declarePattern = (~kind=`Const, pattern, e): declaration =>
  `DeclareVar(({kind, pattern}, Some(e)));

let expr: expr => statement = e => `Expr(e);
let declaration: declaration => statement = e => `Declaration(e);
let block: array(statement) => block = ss => `Block(ss);
let block1: statement => block = s => block([|s|]);
let block1Expr: expr => block = e => expr(e)->block1;
let unsafeRawStatement = raw => `UNSAFE_RAW_STATEMENT(raw);

let appendStatement: (block, statement) => block =
  (`Block(ss), s) => `Block(ss->Utils.Array.append(s));

let functionProperty =
    (~static=false, ~sync=`Sync, name, params, body): classProperty =>
  `Function((
    static ? `Static : `NotStatic,
    {sync, name: ident(name), params, body},
  ));

let classVariable = (name, expr): classProperty =>
  `ClassVariable((name->ident, expr));

let try_ = (~excVar=?, ~catch=block([||]), tryBlock): statement =>
  `TryCatch((tryBlock, excVar, catch));

let func = (~sync=`Sync, ~name=?, params, body): expr =>
  `Function({
    sync,
    name: name->Belt.Option.map(ident),
    params,
    body: `Block(body),
  });
let arrowFunc = (~sync=`Sync, params, body): expr =>
  `ArrowFunction({sync, name: (), params, body: `Block(body)});
let arrowFunc1 = (~sync=`Sync, params, e): expr =>
  `ArrowFunction({sync, name: (), params, body: `Return(e)});

let iife = (~init as `Block(statements)=block([||]), return: expr) =>
  arrowFunc(
    [||],
    statements->Utils.Array.append(`Return(Some(return)): statement),
  )
  ->call0;

let iifeTryCatch =
    (
      ~init: block=block([||]),
      ~return: expr,
      ~excVar=?,
      ~catchInit=block([||]),
      ~catchReturn: expr,
      (),
    ) =>
  arrowFunc(
    [||],
    [|
      try_(
        ~excVar?,
        ~catch=catchInit->appendStatement(`Return(Some(catchReturn))),
        init->appendStatement(`Return(Some(return))),
      ),
    |],
  )
  ->call0;

let class_ = (~name=?, ~extends: option(expr)=?, properties): expr =>
  `Class((name->Belt.Option.map(ident), extends, properties));

let jsxElement = (~props=?, ~children=?, name) => {
  elementName: name->ident,
  elementProps:
    props->Belt.Option.mapWithDefault([||], a =>
      a->Belt.Array.map(((i, e)) => (i->ident, e))
    ),
  elementChildren: children->Belt.Option.getWithDefault([||]),
};

let addProp = (e, key, prop) => {
  ...e,
  elementProps: e.elementProps->Belt.Array.concat([|(key->ident, prop)|]),
};

let object_ = (kvs: array((ident, expr))): expr =>
  `Object(kvs->Belt.Array.map(((k, v)) => (`I(k), v)));

let object0 = object_([||]);

let object1 = (k, v) => object_([|(k->ident, v)|]);
let object2 = ((k1, v1), (k2, v2)) =>
  object_([|(k1->ident, v1), (k2->ident, v2)|]);
let object3 = ((k1, v1), (k2, v2), (k3, v3)) =>
  object_([|(k1->ident, v1), (k2->ident, v2), (k3->ident, v3)|]);
let object4 = ((k1, v1), (k2, v2), (k3, v3), (k4, v4)) =>
  object_([|
    (k1->ident, v1),
    (k2->ident, v2),
    (k3->ident, v3),
    (k4->ident, v4),
  |]);

let importDefault = (~name, ~from) => {
  from,
  what: [|`Destructure(`Name((name->ident, None)))|],
};

let importNames = (~from, names) => {
  from,
  what: [|
    `Destructure(
      `MultipleDestructures(
        names->Belt.Array.map(n => `Name((n->ident, None))),
      ),
    ),
  |],
};

let if_ = (~ifFalse=?, cond, ifTrue): statement =>
  `If((cond, ifTrue, ifFalse));

// Create an AST for a react component class.
let reactComponentClass = (~name, ~pure, renderBody) => {
  let extends = dots("React"->var, [|(pure ? "Pure" : "") ++ "Component"|]);
  class_(~name, ~extends, [|functionProperty("render", [||], renderBody)|]);
};

let rec json: Js.Json.t => expr =
  j =>
    switch (j->Js.Json.classify) {
    | JSONString(s) => s->string
    | JSONNumber(n) => n->float
    | JSONTrue => true->bool
    | JSONFalse => false->bool
    | JSONNull => null
    | JSONArray(arr) => arr->Belt.Array.map(json)->array
    | JSONObject(obj) =>
      `Object(
        obj
        ->Js.Dict.entries
        ->Utils.Array.map(((k, v)) => (`S(k), v->json)),
      )
    };

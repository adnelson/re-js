module type OpaqueString = {
  type t;
  let fromString: string => t;
  let toString: t => string;
  let toJson: Json.Encode.encoder(t);
  let fromJson: Json.Decode.decoder(t);
  let eq: (t, t) => bool;
  let appendString: (t, string) => t;
  let prependString: (string, t) => t;
};

module MakeOpaqueString = (()) : OpaqueString => {
  type t = string;
  external fromString: string => t = "%identity";
  external toString: t => string = "%identity";
  let toJson = Json.Encode.string;
  let fromJson = Json.Decode.string;
  let eq = (==);
  let appendString = (++);
  let prependString = (++);
};

module Humps = {
  type decamelizeOptions = {separator: string};

  [@bs.module "humps"]
  external decamelize: (string, decamelizeOptions) => string = "decamelize";

  let camelToKebab: string => string = s => s->decamelize({separator: "-"});
};

module Crypto = {
  type hash;
  [@bs.send] external hashToString: hash => string = "toString";

  [@bs.module] external sha256: string => hash = "crypto-js/sha256";
};

module Json = {
  include Js.Json;
  let computeHash: t => Crypto.hash = j => j->stringify->Crypto.sha256;

  module Encode = {
    include Json.Encode;

    // Shorthand for encoding raw json as-is
    let json: encoder(Js.Json.t) = j => j;

    // Like `pair` but for 3-tuples
    let tuple3 = (enc1, enc2, enc3, (v1, v2, v3)) =>
      [|v1->enc1, v2->enc2, v3->enc3|] |> array(json);

    let object1 = (fieldName, enc, x) => object_([(fieldName, x |> enc)]);

    let object2 = (fieldName1, enc1, fieldName2, enc2, (v1, v2)) =>
      object_([(fieldName1, v1 |> enc1), (fieldName2, v2 |> enc2)]);

    let object3 =
        (fieldName1, enc1, fieldName2, enc2, fieldName3, enc3, (v1, v2, v3)) =>
      object_([
        (fieldName1, v1 |> enc1),
        (fieldName2, v2 |> enc2),
        (fieldName3, v3 |> enc3),
      ]);

    let objectOpt: list((string, option(Js.Json.t))) => Js.Json.t =
      items =>
        object_(
          items->Belt.List.keepMap(((k, optV)) =>
            optV->Belt.Option.map(v => (k, v))
          ),
        );
  };
};

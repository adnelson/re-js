module Json = {
  include Js.Json;

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

    let object4 =
        (
          fieldName1,
          enc1,
          fieldName2,
          enc2,
          fieldName3,
          enc3,
          fieldName4,
          enc4,
          (v1, v2, v3, v4),
        ) =>
      object_([
        (fieldName1, v1 |> enc1),
        (fieldName2, v2 |> enc2),
        (fieldName3, v3 |> enc3),
        (fieldName4, v4 |> enc4),
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

module Array = {
  include Belt.Array;
  let snds = arr => arr->map(snd);
};

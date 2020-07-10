module type StringType = {
  type t;
  let fromString: string => t;
  let toString: t => string;
  let toJson: Json.Encode.encoder(t);
  let fromJson: Json.Decode.decoder(t);
  let eq: (t, t) => bool;
};

module MakeString = (Validation: { let checkError: option(string => option(exn)) }) : StringType => {
  type t = string;

  let fromString = switch (Validation.checkError) {
    | None => s => s
    | Some(check) => s => switch (s->check) {
        | None => s
        | Some(exn) =>raise(exn)
      }
  };
  external toString: t => string = "%identity";
  let toJson = Json.Encode.string;
  let fromJson = Json.Decode.string;
  let eq = (==);
};

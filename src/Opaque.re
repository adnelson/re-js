module type StringType = {
  type t;
  let fromString: string => t;
  let toString: t => string;
  let toJson: Json.Encode.encoder(t);
  let fromJson: Json.Decode.decoder(t);
  let eq: (t, t) => bool;
};

module MakeString = (()) : StringType => {
  type t = string;
  external fromString: string => t = "%identity";
  external toString: t => string = "%identity";
  let toJson = Json.Encode.string;
  let fromJson = Json.Decode.string;
  let eq = (==);
};

module A = Belt.Array;

module type StringType = {
  type t;
  let fromString: string => t;
  let toString: t => string;
  let toJson: Json.Encode.encoder(t);
  let fromJson: Json.Decode.decoder(t);
  let eq: (t, t) => bool;
};

module MakeString =
       (Validation: {let checkError: option(string => option(exn));})
       : StringType => {
  type t = string;

  let fromString =
    switch (Validation.checkError) {
    | None => (s => s)
    | Some(check) => (
        s =>
          switch (s->check) {
          | None => s
          | Some(exn) => raise(exn)
          }
      )
    };
  external toString: t => string = "%identity";
  let toJson = Json.Encode.string;
  let fromJson = Json.Decode.string;
  let eq = (==);
};

module MakeCompare = (S: StringType) =>
  Belt.Id.MakeComparable({
    type nonrec t = S.t;
    let cmp = Pervasives.compare;
  });

module MakeSet = (Compare: Belt.Id.Comparable) => {
  include Belt.Set;
  type set = t(Compare.t, Compare.identity);
  let fromArray: array(Compare.t) => set =
    Belt.Set.fromArray(~id=(module Compare));
  let fromList: list(Compare.t) => set = l => l->Belt.List.toArray->fromArray;
  let empty: set = fromArray([||]);
  let singleton: Compare.t => set = x => fromArray([|x|]);
};

module MakeStringSet = (S: StringType) => MakeSet((MakeCompare(S)));

open Jest;
open Expect;
open Json;

let _ = {
  describe("parse", () => {
    test("success", () =>
      expect @@ parse("null") |> toEqual(Some(Encode.null))
    );

    test("error", () =>
      expect @@ parse("{") |> toEqual(None)
    );
  });

  describe("parseOrRaise", () => {
    test("success", () =>
      expect @@ parseOrRaise("null") |> toEqual(Encode.null)
    );

    test("error", () =>
      expectFn(parseOrRaise, "{")
      |> toThrowException(ParseError("Unexpected end of JSON input"))
    );
  });

  test("stringify", () =>
    expect @@ stringify(Encode.null) |> toEqual("null")
  );
};

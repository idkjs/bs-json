open Jest;
open Expect;

module Test = {
  type default_case =
    | Float
    | Int
    | String
    | Null
    | Array
    | Object
    | Bool
    | Char;

  let valueFor = {
    open! Json.Encode;
    fun
    | Float => float(1.23)
    | Int => int(23)
    | String => string("test")
    | Null => null
    | Array => jsonArray([||])
    | Object => object_([])
    | Bool => bool(true)
    | Char => char('a');
  };

  let throws = (~name="throws", decoder, kinds) =>
    testAll(name, List.map(valueFor, kinds), value =>
      expectFn(decoder, value)
      |> toThrowException(Json.Decode.DecodeError(""))
    );
};

let () = {
  describe("id", () =>
    Json.(
      Decode.(
        test("id", () =>
          expect @@ int(0 |> Encode.int |> Decode.id) |> toEqual(0)
        )
      )
    )
  );

  describe("bool", () =>
    open Json;
    open Decode;

    test("bool", () =>
      expect @@ bool(Encode.bool(true)) |> toEqual(true)
    );
    test("bool - false", () =>
      expect @@ bool(Encode.bool(false)) |> toEqual(false)
    );

    Test.throws(bool, [Float, Int, String, Null, Array, Object, Char]);
  );

  describe("float", () =>
    open Json;
    open! Decode;

    test("float", () =>
      expect @@ float(Encode.float(1.23)) |> toEqual(1.23)
    );
    test("int", () =>
      expect @@ float(Encode.int(23)) |> toEqual(23.)
    );

    Test.throws(float, [Bool, String, Null, Array, Object, Char]);
  );

  describe("int", () =>
    open Json;
    open! Decode;

    test("int", () =>
      expect @@ int(Encode.int(23)) |> toEqual(23)
    );

    test("int > 32-bit", () =>
      /* Use %raw since integer literals > Int32.max_int overflow without warning */
      let big_int = [%raw "2147483648"];
      expect @@ int(Encode.int(big_int)) |> toEqual(big_int);
    );
    test("infinity", () =>
      let inf = [%raw "Infinity"];
      expectFn(int, Encode.int(inf))
      |> toThrowException(Decode.DecodeError("Expected integer, got null"));
    );

    Test.throws(int, [Bool, Float, String, Null, Array, Object, Char]);
  );

  describe("string", () =>
    open Json;
    open! Decode;

    test("string", () =>
      expect @@ string(Encode.string("test")) |> toEqual("test")
    );

    test("single-character string", () =>
      expect @@ string(Encode.char('a')) |> toEqual("a")
    );

    Test.throws(string, [Bool, Float, Int, Null, Array, Object]);
  );

  describe("date", () =>
    open Json;
    open! Decode;

    test("ISO8601-formatted string", () =>
      expect @@
      date(Encode.string("2012-04-23T18:25:43.511Z"))
      |> toEqual(Js.Date.fromString("2012-04-23T18:25:43.511Z"))
    );

    Test.throws(date, [Bool, Float, Int, Null, Array, Object]);
  );

  describe("char", () =>
    open Json;
    open! Decode;

    test("character", () =>
      expect @@ char(Encode.char('a')) |> toEqual('a')
    );

    test("single-character string", () =>
      expect @@ char(Encode.string("a")) |> toEqual('a')
    );

    test("empty string", () =>
      expectFn(char, Encode.string(""))
      |> toThrowException(
           Decode.DecodeError("Expected single-character string, got \"\""),
         )
    );

    test("multiple-character string", () =>
      expectFn(char, Encode.string("abc"))
      |> toThrowException(
           Decode.DecodeError(
             "Expected single-character string, got \"abc\"",
           ),
         )
    );

    Test.throws(char, [Bool, Float, Int, Null, Array, Object]);
  );

  describe("nullable", () =>
    open Json;
    open! Decode;

    test("int -> int", () =>
      expect @@
      (nullable(int))(Encode.int(23))
      |> toEqual(Js.Null.return(23))
    );
    test("null -> int", () =>
      expect @@ (nullable(int))(Encode.null) |> toEqual(Js.null)
    );

    test("boolean -> boolean ", () =>
      expect @@
      nullable(bool, Encode.bool(true))
      |> toEqual(Js.Null.return(true))
    );
    test("float -> float", () =>
      expect @@
      nullable(float, Encode.float(1.23))
      |> toEqual(Js.Null.return(1.23))
    );
    test("string -> string", () =>
      expect @@
      nullable(string, Encode.string("test"))
      |> toEqual(Js.Null.return("test"))
    );
    test("null -> null", () =>
      expect @@ nullable(nullAs(Js.null), Encode.null) |> toEqual(Js.null)
    );

    Test.throws(nullable(int), [Bool, Float, String, Array, Object, Char]);
    Test.throws(nullable(bool), [Int]);
  );

  describe("nullAs", () =>
    open Json;
    open Decode;

    test("as 0 - null", () =>
      expect @@ (nullAs(0))(Encode.null) |> toEqual(0)
    );

    test("as Js.null", () =>
      expect(nullAs(Js.null, Encode.null)) |> toEqual(Js.null)
    );
    test("as None", () =>
      expect(nullAs(None, Encode.null)) |> toEqual(None)
    );
    test("as Some _", () =>
      expect(nullAs(Some("foo"), Encode.null)) |> toEqual(Some("foo"))
    );

    Test.throws(nullAs(0), [Bool, Float, Int, String, Array, Object, Char]);
  );

  describe("array", () =>
    open Json;
    open! Decode;

    test("array", () =>
      expect @@ (array(int))(Encode.jsonArray([||])) |> toEqual([||])
    );

    test("boolean", () =>
      expect @@
      array(bool, parseOrRaise({| [true, false, true] |}))
      |> toEqual([|true, false, true|])
    );
    test("float", () =>
      expect @@
      array(float, parseOrRaise({| [1, 2, 3] |}))
      |> toEqual([|1., 2., 3.|])
    );
    test("int", () =>
      expect @@
      array(int, parseOrRaise({| [1, 2, 3] |}))
      |> toEqual([|1, 2, 3|])
    );
    test("string", () =>
      expect @@
      array(string, parseOrRaise({| ["a", "b", "c"] |}))
      |> toEqual([|"a", "b", "c"|])
    );
    test("nullAs", () =>
      expect @@
      array(nullAs(Js.null), parseOrRaise({| [null, null, null] |}))
      |> toEqual([|Js.null, Js.null, Js.null|])
    );

    test("array int -> array boolean", () =>
      expectFn(array(bool), parseOrRaise({| [1, 2, 3] |}))
      |> toThrowException(DecodeError("Expected boolean, got 1\n\tin array"))
    );
    test("non-DecodeError exceptions in decoder should pass through", () =>
      expectFn(
        array(_ => failwith("fail")),
        Encode.array(Encode.int, [|1|]),
      )
      |> toThrowException(Failure("fail"))
    );

    Test.throws(array(int), [Bool, Float, Int, String, Null, Object, Char]);
  );

  describe("list", () =>
    open Json;
    open! Decode;

    test("array", () =>
      expect @@ (list(int))(Encode.jsonArray([||])) |> toEqual([])
    );

    test("boolean", () =>
      expect @@
      list(bool, parseOrRaise({| [true, false, true] |}))
      |> toEqual([true, false, true])
    );
    test("float", () =>
      expect @@
      list(float, parseOrRaise({| [1, 2, 3] |}))
      |> toEqual([1., 2., 3.])
    );
    test("int", () =>
      expect @@
      list(int, parseOrRaise({| [1, 2, 3] |}))
      |> toEqual([1, 2, 3])
    );
    test("string", () =>
      expect @@
      list(string, parseOrRaise({| ["a", "b", "c"] |}))
      |> toEqual(["a", "b", "c"])
    );
    test("nullAs", () =>
      expect @@
      list(nullAs(Js.null), parseOrRaise({| [null, null, null] |}))
      |> toEqual([Js.null, Js.null, Js.null])
    );

    test("array int -> list boolean", () =>
      expectFn(list(bool), parseOrRaise({| [1, 2, 3] |}))
      |> toThrowException(DecodeError("Expected boolean, got 1\n\tin array"))
    );
    test("non-DecodeError exceptions in decoder should pass through", () =>
      expectFn(list(_ => failwith("fail")), Encode.list(Encode.int, [1]))
      |> toThrowException(Failure("fail"))
    );

    Test.throws(list(int), [Bool, Float, Int, String, Null, Object, Char]);
  );

  describe("pair", () =>
    open Json;
    open! Decode;

    test("heterogenous", () =>
      expect @@
      pair(string, int, parseOrRaise({| ["a", 3] |}))
      |> toEqual(("a", 3))
    );
    test("int int", () =>
      expect @@
      pair(int, int, parseOrRaise({| [4, 3] |}))
      |> toEqual((4, 3))
    );
    test("too small", () =>
      expectFn(pair(int, int), parseOrRaise({| [4] |}))
      |> toThrowException(
           DecodeError("Expected array of length 2, got array of length 1"),
         )
    );
    test("too large", () =>
      expectFn(pair(int, int), parseOrRaise({| [3, 4, 5] |}))
      |> toThrowException(
           DecodeError("Expected array of length 2, got array of length 3"),
         )
    );
    test("bad type a", () =>
      expectFn(pair(int, int), parseOrRaise({| ["3", 4] |}))
      |> toThrowException(
           DecodeError("Expected number, got \"3\"\n\tin pair/tuple2"),
         )
    );
    test("bad type b", () =>
      expectFn(pair(string, string), parseOrRaise({| ["3", 4] |}))
      |> toThrowException(
           DecodeError("Expected string, got 4\n\tin pair/tuple2"),
         )
    );
    test("not array", () =>
      expectFn(pair(int, int), parseOrRaise({| 4 |}))
      |> toThrowException(DecodeError("Expected array, got 4"))
    );
    test("non-DecodeError exceptions in decoder should pass through", () =>
      expectFn(
        pair(_ => failwith("fail"), int),
        parseOrRaise({| [4, 3] |}),
      )
      |> toThrowException(Failure("fail"))
    );
  );

  describe("tuple2", () =>
    open Json;
    open! Decode;

    test("heterogenous", () =>
      expect @@
      tuple2(string, int, parseOrRaise({| ["a", 3] |}))
      |> toEqual(("a", 3))
    );
    test("too small", () =>
      expectFn(tuple2(int, int), parseOrRaise({| [4] |}))
      |> toThrowException(
           DecodeError("Expected array of length 2, got array of length 1"),
         )
    );
    test("too large", () =>
      expectFn(tuple2(int, int), parseOrRaise({| [3, 4, 5] |}))
      |> toThrowException(
           DecodeError("Expected array of length 2, got array of length 3"),
         )
    );
    test("bad type a", () =>
      expectFn(tuple2(int, int), parseOrRaise({| ["3", 4] |}))
      |> toThrowException(
           DecodeError("Expected number, got \"3\"\n\tin pair/tuple2"),
         )
    );
    test("bad type b", () =>
      expectFn(tuple2(string, string), parseOrRaise({| ["3", 4] |}))
      |> toThrowException(
           DecodeError("Expected string, got 4\n\tin pair/tuple2"),
         )
    );
    test("not array", () =>
      expectFn(tuple2(int, int), parseOrRaise({| 4 |}))
      |> toThrowException(DecodeError("Expected array, got 4"))
    );
    test("non-DecodeError exceptions in decoder should pass through", () =>
      expectFn(
        tuple2(_ => failwith("fail"), int),
        parseOrRaise({| [4, 3] |}),
      )
      |> toThrowException(Failure("fail"))
    );
  );

  describe("tuple3", () =>
    open Json;
    open! Decode;

    test("heterogenous", () =>
      expect @@
      tuple3(string, int, float, parseOrRaise({| ["a", 3, 4.5] |}))
      |> toEqual(("a", 3, 4.5))
    );
    test("too small", () =>
      expectFn(tuple3(int, int, int), parseOrRaise({| [4] |}))
      |> toThrowException(
           DecodeError("Expected array of length 3, got array of length 1"),
         )
    );
    test("too large", () =>
      expectFn(tuple3(int, int, int), parseOrRaise({| [3, 4, 5, 6, 7] |}))
      |> toThrowException(
           DecodeError("Expected array of length 3, got array of length 5"),
         )
    );
    test("bad type a", () =>
      expectFn(tuple3(int, int, int), parseOrRaise({| ["3", 4, 5] |}))
      |> toThrowException(
           DecodeError("Expected number, got \"3\"\n\tin tuple3"),
         )
    );
    test("bad type b", () =>
      expectFn(
        tuple3(string, string, string),
        parseOrRaise({| ["3", 4, "5"] |}),
      )
      |> toThrowException(DecodeError("Expected string, got 4\n\tin tuple3"))
    );
    test("not array", () =>
      expectFn(tuple3(int, int, int), parseOrRaise({| 4 |}))
      |> toThrowException(DecodeError("Expected array, got 4"))
    );
    test("non-DecodeError exceptions in decoder should pass through", () =>
      expectFn(
        tuple3(_ => failwith("fail"), int, int),
        parseOrRaise({| [4, 3, 5] |}),
      )
      |> toThrowException(Failure("fail"))
    );
  );

  describe("tuple4", () =>
    open Json;
    open! Decode;

    test("heterogenous", () =>
      expect @@
      tuple4(
        string,
        int,
        float,
        bool,
        parseOrRaise({| ["a", 3, 4.5, true] |}),
      )
      |> toEqual(("a", 3, 4.5, true))
    );
    test("too small", () =>
      expectFn(tuple4(int, int, int, int), parseOrRaise({| [4] |}))
      |> toThrowException(
           DecodeError("Expected array of length 4, got array of length 1"),
         )
    );
    test("too large", () =>
      expectFn(
        tuple4(int, int, int, int),
        parseOrRaise({| [3, 4, 5, 6, 7, 8] |}),
      )
      |> toThrowException(
           DecodeError("Expected array of length 4, got array of length 6"),
         )
    );
    test("bad type a", () =>
      expectFn(
        tuple4(int, int, int, int),
        parseOrRaise({| ["3", 4, 5, 6] |}),
      )
      |> toThrowException(
           DecodeError("Expected number, got \"3\"\n\tin tuple4"),
         )
    );
    test("bad type b", () =>
      expectFn(
        tuple4(string, string, string, string),
        parseOrRaise({| ["3", 4, "5", "6"] |}),
      )
      |> toThrowException(DecodeError("Expected string, got 4\n\tin tuple4"))
    );
    test("not array", () =>
      expectFn(tuple4(int, int, int, int), parseOrRaise({| 4 |}))
      |> toThrowException(DecodeError("Expected array, got 4"))
    );
    test("non-DecodeError exceptions in decoder should pass through", () =>
      expectFn(
        tuple4(_ => failwith("fail"), int, int, int),
        parseOrRaise({| [4, 3, 5, 6] |}),
      )
      |> toThrowException(Failure("fail"))
    );
  );

  describe("dict", () =>
    open Json;
    open! Decode;

    test("object", () =>
      expect @@ dict(int, Encode.object_([])) |> toEqual(Js.Dict.empty())
    );

    test("boolean", () =>
      expect @@
      dict(bool, parseOrRaise({| { "a": true, "b": false } |}))
      |> toEqual(Obj.magic([%obj {a: true, b: false}]))
    );
    test("float", () =>
      expect @@
      dict(float, parseOrRaise({| { "a": 1.2, "b": 2.3 } |}))
      |> toEqual(Obj.magic([%obj {a: 1.2, b: 2.3}]))
    );
    test("int", () =>
      expect @@
      dict(int, parseOrRaise({| { "a": 1, "b": 2 } |}))
      |> toEqual(Obj.magic([%obj {a: 1, b: 2}]))
    );
    test("string", () =>
      expect @@
      dict(string, parseOrRaise({| { "a": "x", "b": "y" } |}))
      |> toEqual(Obj.magic([%obj {a: "x", b: "y"}]))
    );
    test("nullAs", () =>
      expect @@
      dict(nullAs(Js.null), parseOrRaise({| { "a": null, "b": null } |}))
      |> toEqual(Obj.magic([%obj {a: Js.null, b: Js.null}]))
    );
    test("null -> dict string", () =>
      expectFn(dict(string), parseOrRaise({| { "a": null, "b": null } |}))
      |> toThrowException(
           DecodeError("Expected string, got null\n\tin dict"),
         )
    );
    test("non-DecodeError exceptions in decoder should pass through", () =>
      expectFn(dict(_ => failwith("fail")), parseOrRaise({| { "a": 0 } |}))
      |> toThrowException(Failure("fail"))
    );

    Test.throws(dict(int), [Bool, Float, Int, String, Null, Array, Char]);
  );

  describe("field", () =>
    open Json;
    open! Decode;

    test("boolean", () =>
      expect @@
      field("b", bool, parseOrRaise({| { "a": true, "b": false } |}))
      |> toEqual(false)
    );
    test("float", () =>
      expect @@
      field("b", float, parseOrRaise({| { "a": 1.2, "b": 2.3 } |}))
      |> toEqual(2.3)
    );
    test("int", () =>
      expect @@
      field("b", int, parseOrRaise({| { "a": 1, "b": 2 } |}))
      |> toEqual(2)
    );
    test("string", () =>
      expect @@
      field("b", string, parseOrRaise({| { "a": "x", "b": "y" } |}))
      |> toEqual("y")
    );
    test("nullAs", () =>
      expect @@
      field(
        "b",
        nullAs(Js.null),
        parseOrRaise({| { "a": null, "b": null } |}),
      )
      |> toEqual(Js.null)
    );
    test("missing key", () =>
      expectFn(
        field("c", string),
        parseOrRaise({| { "a": null, "b": null } |}),
      )
      |> toThrowException(DecodeError("Expected field 'c'"))
    );
    test("decoder error", () =>
      expectFn(
        field("b", string),
        parseOrRaise({| { "a": null, "b": null } |}),
      )
      |> toThrowException(
           DecodeError("Expected string, got null\n\tat field 'b'"),
         )
    );
    test("non-DecodeError exceptions in decoder should pass through", () =>
      expectFn(
        field("a", _ => failwith("fail")),
        parseOrRaise({| { "a": 0 } |}),
      )
      |> toThrowException(Failure("fail"))
    );

    Test.throws(
      field("foo", int),
      [Bool, Float, Int, String, Null, Array, Object, Char],
    );
  );

  describe("at", () =>
    open Json;
    open! Decode;

    test("boolean", () =>
      expect @@
      at(
        ["a", "x", "y"],
        bool,
        parseOrRaise(
          {| {
        "a": { "x" : { "y" : false } },
        "b": false
      } |},
        ),
      )
      |> toEqual(false)
    );
    test("nullAs", () =>
      expect @@
      at(
        ["a", "x"],
        nullAs(Js.null),
        parseOrRaise(
          {| {
        "a": { "x" : null },
        "b": null
      } |},
        ),
      )
      |> toEqual(Js.null)
    );

    test("missing key", () =>
      expectFn(
        at(["a", "y"], nullAs(Js.null)),
        parseOrRaise(
          {| {
        "a": { "x" : null },
        "b": null
      } |},
        ),
      )
      |> toThrowException(DecodeError("Expected field 'y'\n\tat field 'a'"))
    );
    test("decoder error", () =>
      expectFn(
        at(["a", "x", "y"], nullAs(Js.null)),
        parseOrRaise(
          {| {
        "a": { "x" : { "y": "foo" } },
        "b": null
      } |},
        ),
      )
      |> toThrowException(
           DecodeError(
             "Expected null, got \"foo\"\n\tat field 'y'\n\tat field 'x'\n\tat field 'a'",
           ),
         )
    );
    test("empty list of keys should raise Invalid_argument", () =>
      expectFn(at([]), int)
      |> toThrowException(
           Invalid_argument(
             "Expected key_path to contain at least one element",
           ),
         )
    );

    Test.throws(
      at(["foo", "bar"], int),
      [Bool, Float, Int, String, Null, Array, Object, Char],
    );
  );

  describe("optional", () =>
    open Json;
    open! Decode;

    test("boolean -> int", () =>
      expect @@ (optional(int))(Encode.bool(true)) |> toEqual(None)
    );
    test("float -> int", () =>
      expect @@ (optional(int))(Encode.float(1.23)) |> toEqual(None)
    );
    test("int -> int", () =>
      expect @@ (optional(int))(Encode.int(23)) |> toEqual(Some(23))
    );
    test("string -> int", () =>
      expect @@ (optional(int))(Encode.string("test")) |> toEqual(None)
    );
    test("null -> int", () =>
      expect @@ (optional(int))(Encode.null) |> toEqual(None)
    );
    test("array -> int", () =>
      expect @@ (optional(int))(Encode.jsonArray([||])) |> toEqual(None)
    );
    test("object -> int", () =>
      expect @@ (optional(int))(Encode.object_([])) |> toEqual(None)
    );

    test("boolean -> boolean ", () =>
      expect @@ optional(bool, Encode.bool(true)) |> toEqual(Some(true))
    );
    test("float -> float", () =>
      expect @@ optional(float, Encode.float(1.23)) |> toEqual(Some(1.23))
    );
    test("string -> string", () =>
      expect @@
      optional(string, Encode.string("test"))
      |> toEqual(Some("test"))
    );
    test("null -> null", () =>
      expect @@
      optional(nullAs(Js.null), Encode.null)
      |> toEqual(Some(Js.null))
    );
    test("int -> boolean", () =>
      expect @@ (optional(bool))(Encode.int(1)) |> toEqual(None)
    );

    test("optional field", () =>
      expect @@
      optional(field("x", int), parseOrRaise({| { "x": 2} |}))
      |> toEqual(Some(2))
    );
    test("optional field - incorrect type", () =>
      expect @@
      optional(field("x", int), parseOrRaise({| { "x": 2.3} |}))
      |> toEqual(None)
    );
    test("optional field - no such field", () =>
      expect @@
      optional(field("y", int), parseOrRaise({| { "x": 2} |}))
      |> toEqual(None)
    );
    test("field optional", () =>
      expect @@
      field("x", optional(int), parseOrRaise({| { "x": 2} |}))
      |> toEqual(Some(2))
    );
    test("field optional - incorrect type", () =>
      expect @@
      field("x", optional(int), parseOrRaise({| { "x": 2.3} |}))
      |> toEqual(None)
    );
    test("field optional - no such field", () =>
      expectFn(field("y", optional(int)), parseOrRaise({| { "x": 2} |}))
      |> toThrowException(DecodeError("Expected field 'y'"))
    );

    test("non-DecodeError exceptions in decoder should pass through", () =>
      expectFn(optional(_ => failwith("fail")), Encode.null)
      |> toThrowException(Failure("fail"))
    );
  );

  describe("oneOf", () =>
    open Json;
    open! Decode;

    test("object with field", () =>
      expect @@
      (oneOf([int, field("x", int)]))(parseOrRaise({| { "x": 2} |}))
      |> toEqual(2)
    );
    test("int", () =>
      expect @@
      (oneOf([int, field("x", int)]))(Encode.int(23))
      |> toEqual(23)
    );

    test("non-DecodeError exceptions in decoder should pass through", () =>
      expectFn(oneOf([_ => failwith("fail")]), Encode.null)
      |> toThrowException(Failure("fail"))
    );

    Test.throws(
      oneOf([int, field("x", int)]),
      [Bool, Float, String, Null, Array, Object, Char],
    );
  );

  describe("either", () =>
    open Json;
    open! Decode;

    test("object with field", () =>
      expect @@
      (either(int, field("x", int)))(parseOrRaise({| { "x": 2} |}))
      |> toEqual(2)
    );
    test("int", () =>
      expect @@
      (either(int, field("x", int)))(Encode.int(23))
      |> toEqual(23)
    );

    Test.throws(
      either(int, field("x", int)),
      [Bool, Float, String, Null, Array, Object, Char],
    );
  );

  describe("withDefault", () =>
    open Json;
    open! Decode;

    test("boolean", () =>
      expect @@ (withDefault(0, int))(Encode.bool(true)) |> toEqual(0)
    );
    test("float", () =>
      expect @@ (withDefault(0, int))(Encode.float(1.23)) |> toEqual(0)
    );
    test("int", () =>
      expect @@ (withDefault(0, int))(Encode.int(23)) |> toEqual(23)
    );
    test("string", () =>
      expect @@ (withDefault(0, int))(Encode.string("test")) |> toEqual(0)
    );
    test("null", () =>
      expect @@ (withDefault(0, int))(Encode.null) |> toEqual(0)
    );
    test("array", () =>
      expect @@ (withDefault(0, int))(Encode.jsonArray([||])) |> toEqual(0)
    );
    test("object", () =>
      expect @@ (withDefault(0, int))(Encode.object_([])) |> toEqual(0)
    );

    test("non-DecodeError exceptions in decoder should pass through", () =>
      expectFn(withDefault(4, _ => failwith("fail")), Encode.int(0))
      |> toThrowException(Failure("fail"))
    );
  );

  describe("map", () =>
    open Json;
    open! Decode;

    test("int", () =>
      expect @@ (int |> map((+)(2)))(Encode.int(23)) |> toEqual(25)
    );

    Test.throws(
      int |> map((+)(2)),
      [Bool, Float, String, Null, Array, Object, Char],
    );
  );

  describe("andThen", () =>
    open Json;
    open! Decode;

    test("int -> int", () =>
      expect @@ (int |> andThen(_ => int))(Encode.int(23)) |> toEqual(23)
    );

    test("int -> int andThen float", () =>
      expect @@ (int |> andThen(_ => float))(Encode.int(23)) |> toEqual(23.)
    );
    test("int -> float andThen int", () =>
      expect @@ (float |> andThen(_ => int))(Encode.int(23)) |> toEqual(23)
    );

    Test.throws(
      ~name="int andThen int ",
      int |> andThen(_ => int),
      [Bool, Float, String, Null, Array, Object, Char],
    );
    Test.throws(
      ~name="float andThen int ",
      float |> andThen(_ => int),
      [Float],
    );
    Test.throws(~name="int to ", int |> andThen(_ => float), [Float]);
  );

  describe("composite expressions", () =>
    open Json;
    open! Decode;

    test("dict array array int", () =>
      expect @@
      dict(
        array(array(int)),
        parseOrRaise({| { "a": [[1, 2], [3]], "b": [[4], [5, 6]] } |}),
      )
      |> toEqual(
           Obj.magic(
             [%obj {a: [|[|1, 2|], [|3|]|], b: [|[|4|], [|5, 6|]|]}],
           ),
         )
    );
    test("dict array array int - heterogenous structure", () =>
      expectFn(
        dict(array(array(int))),
        parseOrRaise({| { "a": [[1, 2], [true]], "b": [[4], [5, 6]] } |}),
      )
      |> toThrowException(
           DecodeError(
             "Expected number, got true\n\tin array at index 0\n\tin array at index 1\n\tin dict",
           ),
         )
    );
    test("dict array array int - heterogenous structure 2", () =>
      expectFn(
        dict(array(array(int))),
        parseOrRaise({| { "a": [[1, 2], "foo"], "b": [[4], [5, 6]] } |}),
      )
      |> toThrowException(
           DecodeError(
             "Expected array, got \"foo\"\n\tin array at index 1\n\tin dict",
           ),
         )
    );
    test("field", () =>
      let json = parseOrRaise({| { "foo": [1, 2, 3], "bar": "baz" } |});
      expect @@
      (field("foo", array(int), json), field("bar", string, json))
      |> toEqual(([|1, 2, 3|], "baz"));
    );
  );
};

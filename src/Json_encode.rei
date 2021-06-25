/** Provides functions for encoding a JSON data structure */;

/** The type of a encoder combinator */

type encoder('a) = 'a => Js.Json.t;

/** [null] is the singleton null JSON value */
[@bs.val]
external null: Js.Json.t = "null";

/** [string s] makes a JSON string of the [string] [s] */
external string: string => Js.Json.t = "%identity";

/** [float n] makes a JSON number of the [float] [n] */
external float: float => Js.Json.t = "%identity";

/** [int n] makes a JSON number of the [int] [n] */
external int: int => Js.Json.t = "%identity";

/** [bool b] makes a JSON boolean of the [bool] [b] */
external bool: bool => Js.Json.t = "%identity";

/** [char c] makes a JSON string of the [char] [c] */

let char: char => Js.Json.t;

/** [date d] makes an ISO 8601 JSON string of the [Js.Date.t] [d] */

let date: Js.Date.t => Js.Json.t;

/** [nullable encoder option] returns either the encoded value or [null] */

let nullable: (encoder('a), option('a)) => Js.Json.t;

/** [withDefault default encoder option] returns the encoded value if present, oterwise [default] */

let withDefault: (Js.Json.t, encoder('a), option('a)) => Js.Json.t;

/** [pair encoder encoder tuple] creates a JSON array from a tuple of size 2 */

let pair: (encoder('a), encoder('b), ('a, 'b)) => Js.Json.t;

/** [tuple2 encoder encoder tuple] creates a JSON array from a tuple of size 2. Alias of [pair] */

let tuple2: (encoder('a), encoder('b), ('a, 'b)) => Js.Json.t;

/** [tuple3 enc enc enc tuple] creates a JSON array from a tuple of size 3 */

let tuple3:
  (encoder('a), encoder('b), encoder('c), ('a, 'b, 'c)) => Js.Json.t;

/** [tuple4 enc enc enc enc tuple] creates a JSON array from a tuple of size 4 */

let tuple4:
  (
    encoder('a),
    encoder('b),
    encoder('c),
    encoder('d),
    ('a, 'b, 'c, 'd)
  ) =>
  Js.Json.t;

/** [jsonDict d] makes a JSON object of the [Js.Dict.t] [d] */
external jsonDict: Js_dict.t(Js.Json.t) => Js.Json.t = "%identity";

/** [dict encoder d] makes a JSON object of the [Js.Dict.t] [d] with the given [encoder] */

let dict: encoder('a) => encoder(Js_dict.t('a));

/** [object_ props] makes a JSON object of the [props] list of properties */

let object_: list((string, Js.Json.t)) => Js.Json.t;

/** [array encoder l] makes a JSON array of the [list] [l] using the given [encoder]
 *  NOTE: This will be renamed `array` once the existing and deprecated `array` function
 *  has been removed.
 */

let array: encoder('a) => encoder(array('a));

/** [list encoder a] makes a JSON array of the [array] [a] using the given [encoder] */

let list: encoder('a) => encoder(list('a));

/** The functions below are specialized for specific array type which
    happened to be already JSON object in the BuckleScript runtime. Therefore
    they are more efficient (constant time rather than linear conversion). */;

/** [jsonArray a] makes a JSON array of the [Js.Json.t array] [a] */
external jsonArray: array(Js.Json.t) => Js.Json.t = "%identity";

/** [stringArray a] makes a JSON array of the [string array] [a] */
external stringArray: array(string) => Js.Json.t = "%identity";

/** [numberArray a] makes a JSON array of the [float array] [a] */
external numberArray: array(float) => Js.Json.t = "%identity";

/** [boolArray] makes a JSON array of the [bool array] [a] */
external boolArray: array(bool) => Js.Json.t = "%identity";

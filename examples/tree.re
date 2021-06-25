/* Decode a JSON tree structure */
type tree('a) =
  | Node('a, list(tree('a)))
  | Leaf('a);

module Decode = {
  open Json.Decode;

  let rec tree = decoder =>
    field("type", string)
    |> andThen(
         fun
         | "node" => node(decoder)
         | "leaf" => leaf(decoder)
         | _ => failwith("unknown node type"),
       )

  and node = (decoder, json) =>
    [@implicit_arity]
    Node(
      json |> field("value", decoder),
      json |> field("children", array(tree(decoder)) |> map(Array.to_list)),
    )

  and leaf = (decoder, json) => Leaf(json |> field("value", decoder));
};

let rec indent =
  fun
  | n when n <= 0 => ()
  | n => {
      print_string("  ");
      indent(n - 1);
    };

let print = {
  let rec aux = level =>
    fun
    | [@implicit_arity] Node(value, children) => {
        indent(level);
        Js.log(value);
        children |> List.iter(child => aux(level + 1, child));
      }
    | Leaf(value) => {
        indent(level);
        Js.log(value);
      };

  aux(0);
};

let json = {| {
  "type": "node",
  "value": 9,
  "children": [{
    "type": "node",
    "value": 5,
    "children": [{
      "type": "leaf",
      "value": 3
    }, {
      "type": "leaf",
      "value": 2
    }]
  }, {
      "type": "leaf",
      "value": 4
  }]
} |};

let myTree =
  json |> Json.parseOrRaise |> Decode.tree(Json.Decode.int) |> print;

type t = { row : int; col : int; color : Color.t; side : int }

let make ~row ~col ~color ~side = { row; col; color; side }

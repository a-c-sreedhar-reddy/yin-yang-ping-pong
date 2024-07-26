type color = Black | White

type ball = {
  x : int;
  y : int;
  dx : int;
  dy : int;
  radius : int;
  color : color;
}

type cell = { row : int; col : int; color : color }
type state = { black_ball : ball; white_ball : ball; cells : cell list }

type canvas_state = {
  ctx : Webapi.Canvas.Canvas2d.t;
  height : int;
  width : int;
}

let cell_side = 40

let move_ball canvas_state color state =
  let ball = if color == Black then state.black_ball else state.white_ball in
  let ball =
    if
      ball.x + ball.dx > canvas_state.width - ball.radius
      || ball.x + ball.dx < ball.radius
    then { ball with dx = -ball.dx }
    else ball
  in
  let ball =
    if
      ball.y + ball.dy > canvas_state.height - ball.radius
      || ball.y + ball.dy < ball.radius
    then { ball with dy = -ball.dy }
    else ball
  in
  let ball = { ball with x = ball.x + ball.dx; y = ball.y + ball.dy } in
  match color with
  | Black -> { state with black_ball = ball }
  | White -> { state with white_ball = ball }

let cell_and_ball_collides (cell : cell) ball =
  ball.x >= (cell.col * cell_side) - ball.radius
  && ball.x <= (cell.col * cell_side) + cell_side + ball.radius
  && ball.y >= (cell.row * cell_side) - ball.radius
  && ball.y <= (cell.row * cell_side) + cell_side + ball.radius
  && ball.color == cell.color

let toggle_color = function White -> Black | Black -> White

let horizontal_collision cell ball =
  ball.x + ball.radius <= cell.col * cell_side
  || ball.x - ball.radius >= (cell.col * cell_side) + cell_side

let cell_and_ball_after_collision cell ball =
  if cell_and_ball_collides cell ball then
    let ball =
      if horizontal_collision cell ball then { ball with dx = -ball.dx }
      else { ball with dy = -ball.dy }
    in

    ({ cell with color = toggle_color cell.color }, ball)
  else (cell, ball)

let ball_collision_detection color state =
  let cells = state.cells |> List.filter (fun cell -> cell.color == color) in
  let ball = if color == Black then state.black_ball else state.white_ball in
  let colliding_cell =
    cells |> List.find_opt (fun cell -> cell_and_ball_collides cell ball)
  in

  match colliding_cell with
  | Some cell ->
      let new_cell, ball = cell_and_ball_after_collision cell ball in
      let cells : cell list =
        state.cells
        |> List.map (fun cell ->
               if cell.row == new_cell.row && cell.col == new_cell.col then
                 new_cell
               else cell)
      in
      let state : state =
        match color with
        | Black -> { state with cells; black_ball = ball }
        | White -> { state with cells; white_ball = ball }
      in
      state
  | None -> state

let next_state canvas_state state : state =
  state
  |> ball_collision_detection Black
  |> ball_collision_detection White
  |> move_ball canvas_state Black
  |> move_ball canvas_state White

let draw_cells canvas_state state =
  state.cells
  |> List.iter (fun cell ->
         let open Webapi.Canvas.Canvas2d in
         beginPath canvas_state.ctx;
         rect
           ~x:(float (cell.col * cell_side))
           ~y:(float (cell.row * cell_side))
           ~w:(float cell_side) ~h:(float cell_side) canvas_state.ctx;
         setFillStyle canvas_state.ctx String
           (match cell.color with White -> "grey" | Black -> "black");
         fill canvas_state.ctx;
         closePath canvas_state.ctx)

let draw_ball canvas_state ball color =
  let ctx = canvas_state.ctx in
  let open Webapi.Canvas.Canvas2d in
  beginPath ctx;
  arc ~x:(float ball.x) ~y:(float ball.y) ~r:(float ball.radius) ~startAngle:0.
    ~endAngle:(Js.Math._PI *. 2.) ~anticw:false ctx;
  setFillStyle ctx String
    (match color with White -> "grey" | Black -> "black");
  fill ctx;
  closePath ctx

let draw canvas_state state =
  let open Webapi.Canvas.Canvas2d in
  clearRect ~x:0. ~y:0. ~w:(float canvas_state.width)
    ~h:(float canvas_state.height)
    canvas_state.ctx;
  draw_cells canvas_state state;
  draw_ball canvas_state state.white_ball White;
  draw_ball canvas_state state.black_ball Black

let rec loop canvas_state state =
  draw canvas_state state;
  Webapi.requestAnimationFrame (fun _ ->
      loop canvas_state (next_state canvas_state state))

let canvas_element =
  Webapi.Dom.Document.getElementById "canvas" Webapi.Dom.document
;;

match canvas_element with
| Some canvas_element ->
    let canvas_state =
      {
        height = Webapi.Canvas.CanvasElement.height canvas_element;
        width = Webapi.Canvas.CanvasElement.width canvas_element;
        ctx = Webapi.Canvas.CanvasElement.getContext2d canvas_element;
      }
    in
    let ball_radius = 10 in
    loop canvas_state
      {
        black_ball =
          {
            x = (2 * cell_side) + (2 * ball_radius);
            y = canvas_state.height - (2 * ball_radius);
            dx = 2;
            dy = -2;
            radius = ball_radius;
            color = Black;
          };
        white_ball =
          {
            x = canvas_state.width - cell_side - (2 * ball_radius);
            y = ball_radius;
            dx = -2;
            dy = 2;
            radius = ball_radius;
            color = White;
          };
        cells =
          List.init 10 (fun row ->
              List.init 10 (fun col ->
                  { col; row; color = (if row > col then White else Black) }))
          |> List.flatten;
      }
| None -> ()

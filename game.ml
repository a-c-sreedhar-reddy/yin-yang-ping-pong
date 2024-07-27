type t = { balls : Ball.t list; cells : Cell.t list }

type canvas_state = {
  ctx : Webapi.Canvas.Canvas2d.t;
  height : int;
  width : int;
}

let move_balls canvas_state state =
  let balls =
    state.balls
    |> List.map (fun ball ->
           let dx =
             if ball |> Ball.should_ball_bounce_horizontal canvas_state.width
             then -ball.dx
             else ball.dx
           in
           let dy =
             if ball |> Ball.should_ball_bounce_vertical canvas_state.height
             then -ball.dy
             else ball.dy
           in
           { ball with dx; dy; x = ball.x + dx; y = ball.y + dy })
  in
  { state with balls }

let cell_and_ball_after_collision cell ball =
  if Ball.collides_with_cell cell ball then
    let ball =
      if Ball.horizontal_collision cell ball then { ball with dx = -ball.dx }
      else { ball with dy = -ball.dy }
    in

    ({ cell with color = Color.toggle_color cell.color }, ball)
  else (cell, ball)

let balls_collision_detection state =
  state.balls
  |> List.fold_left
       (fun state (ball : Ball.t) ->
         let cells =
           state.cells
           |> List.filter (fun (cell : Cell.t) -> cell.color == ball.color)
         in
         let colliding_cell =
           cells
           |> List.find_opt (fun cell -> Ball.collides_with_cell cell ball)
         in

         match colliding_cell with
         | Some cell ->
             let new_cell, ball = cell_and_ball_after_collision cell ball in
             let cells =
               state.cells
               |> List.map (fun (cell : Cell.t) ->
                      if cell.row == new_cell.row && cell.col == new_cell.col
                      then new_cell
                      else cell)
             in

             {
               cells;
               balls =
                 state.balls
                 |> List.map (fun (cball : Ball.t) ->
                        if cball.id == ball.id then ball else cball);
             }
         | None -> state)
       state

let next_state canvas_state state =
  state |> balls_collision_detection |> move_balls canvas_state

let draw_cells canvas_state state =
  state.cells
  |> List.iter (fun (cell : Cell.t) ->
         let cell_side = cell.side in
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

let draw_balls canvas_state state =
  state.balls
  |> List.iter (fun (ball : Ball.t) ->
         let ctx = canvas_state.ctx in
         let open Webapi.Canvas.Canvas2d in
         beginPath ctx;
         arc ~x:(float ball.x) ~y:(float ball.y) ~r:(float ball.radius)
           ~startAngle:0. ~endAngle:(Js.Math._PI *. 2.) ~anticw:false ctx;
         setFillStyle ctx String
           (match ball.color with White -> "grey" | Black -> "black");
         fill ctx;
         closePath ctx)

let draw_game canvas_state state =
  let open Webapi.Canvas.Canvas2d in
  clearRect ~x:0. ~y:0. ~w:(float canvas_state.width)
    ~h:(float canvas_state.height)
    canvas_state.ctx;
  draw_cells canvas_state state;
  draw_balls canvas_state state

let rec loop canvas_state state =
  draw_game canvas_state state;
  Webapi.requestAnimationFrame (fun _ ->
      loop canvas_state (next_state canvas_state state))

let start () =
  let cell_side = 40 in
  let ball_radius = 10 in
  let canvas_element =
    Webapi.Dom.Document.getElementById "canvas" Webapi.Dom.document
  in

  match canvas_element with
  | Some canvas_element ->
      let canvas_state =
        {
          height = Webapi.Canvas.CanvasElement.height canvas_element;
          width = Webapi.Canvas.CanvasElement.width canvas_element;
          ctx = Webapi.Canvas.CanvasElement.getContext2d canvas_element;
        }
      in
      let black_ball =
        Ball.make ~id:"black"
          ~x:((2 * cell_side) + (2 * ball_radius))
          ~y:(canvas_state.height - (2 * ball_radius))
          ~dx:2 ~dy:(-2) ~radius:ball_radius ~color:Color.Black
      in

      let white_ball =
        Ball.make ~id:"white"
          ~x:(canvas_state.width - cell_side - (2 * ball_radius))
          ~y:ball_radius ~dx:(-2) ~dy:2 ~radius:ball_radius ~color:Color.White
      in

      let balls = [ black_ball; white_ball ] in

      let cells =
        List.init 10 (fun row ->
            List.init 10 (fun col ->
                Cell.make ~col ~row ~side:cell_side
                  ~color:(if row > col then White else Black)))
        |> List.flatten
      in

      loop canvas_state { balls; cells }
  | None -> ()

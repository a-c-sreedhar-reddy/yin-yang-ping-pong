type t = {
  id : string;
  x : int;
  y : int;
  dx : int;
  dy : int;
  radius : int;
  color : Color.t;
}

let make ~id ~x ~y ~dx ~dy ~radius ~color = { id; x; y; dx; dy; radius; color }

let should_ball_bounce_horizontal board_width ball =
  ball.x + ball.dx > board_width - ball.radius || ball.x + ball.dx < ball.radius

let should_ball_bounce_vertical board_height ball =
  ball.y + ball.dy > board_height - ball.radius
  || ball.y + ball.dy < ball.radius

let collides_with_cell (cell : Cell.t) ball =
  let cell_side = cell.side in
  if ball.color != cell.color then false
  else
    let ball_in_cell_horizontal =
      ball.x >= (cell.col * cell_side) - ball.radius
      && ball.x <= (cell.col * cell_side) + cell_side + ball.radius
    in
    let ball_in_cell_vertical =
      ball.y >= (cell.row * cell_side) - ball.radius
      && ball.y <= (cell.row * cell_side) + cell_side + ball.radius
    in

    ball_in_cell_horizontal && ball_in_cell_vertical

let horizontal_collision (cell : Cell.t) ball =
  let cell_side = cell.side in
  ball.x + ball.radius <= cell.col * cell_side
  || ball.x - ball.radius >= (cell.col * cell_side) + cell_side

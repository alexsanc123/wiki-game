open! Core
module Maze_item = Char

module Position = struct
  module T = struct
    type t = int * int [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Maze_structure = struct
  type t =
    { start : Position.t
    ; exit : Position.t
    ; wall : Position.t list
    ; height : int
    ; width : int
    }
  [@@deriving fields]
end

let create_initial_maze input_file : Maze_structure.t =
  let maze_txt = In_channel.read_lines (File_path.to_string input_file) in
  let num_rows = List.length maze_txt in
  let maze_2d_list = List.map maze_txt ~f:(fun row -> String.to_list row) in
  let num_cols =
    match List.hd maze_2d_list with
    | Some char_list -> List.length char_list
    | _ -> 0
  in
  let start_position =
    List.foldi maze_2d_list ~init:[] ~f:(fun row position which_row ->
      List.foldi which_row ~init:position ~f:(fun col _ which_char ->
        if Char.( = ) which_char 'S'
        then position @ [ row, col ]
        else position @ []))
  in
  let exit_position =
    List.foldi maze_2d_list ~init:[] ~f:(fun row position which_row ->
      List.foldi which_row ~init:position ~f:(fun col _ which_char ->
        if Char.( = ) which_char 'E'
        then position @ [ row, col ]
        else position @ []))
  in
  let wall_positions =
    List.foldi maze_2d_list ~init:[] ~f:(fun row position_list which_row ->
      List.foldi
        which_row
        ~init:position_list
        ~f:(fun col position_list which_char ->
        if Char.( = ) which_char '#'
        then position_list @ [ row, col ]
        else position_list @ []))
  in
  { start =
      (match List.hd start_position with
       | Some start_pos -> start_pos
       | None -> 0, 0)
  ; exit =
      (match List.hd exit_position with
       | Some end_pos -> end_pos
       | None -> 0, 0)
  ; wall = wall_positions
  ; height = num_rows
  ; width = num_cols
  }
;;

let in_bounds ~height ~width position =
  let row, col = position in
  row >= 0 && col >= 0 && row < height && col < width
;;

let valid_moves (maze : Maze_structure.t) position =
  let curr_row, curr_col = position in
  let height = Maze_structure.height maze in
  let width = Maze_structure.width maze in
  let walls = Maze_structure.wall maze in
  let new_positions =
    [ curr_row - 1, curr_col
    ; curr_row + 1, curr_col
    ; curr_row, curr_col - 1
    ; curr_row, curr_col + 1
    ]
  in
  List.filter new_positions ~f:(fun position ->
    in_bounds ~height ~width position)
  |> List.filter ~f:(fun in_bounds_pos ->
       List.for_all walls ~f:(fun wall_pos ->
         not (Position.equal wall_pos in_bounds_pos)))
;;

let rec victory_check ~path ~(position : Position.t) ~visited maze =
  let exit = Maze_structure.exit maze in
  let new_path = path @ [ position ] in
  if Position.equal position exit
  then print_s [%message "path:" (new_path : Position.t list)]
  else (
    let possible_moves = valid_moves maze position in
    let unseen_moves =
      List.filter possible_moves ~f:(fun move ->
        not (Hash_set.mem visited move))
    in
    List.iter unseen_moves ~f:(fun next_move ->
      Hash_set.add visited next_move;
      victory_check ~path:new_path ~position:next_move maze ~visited))
;;

let solve_maze (maze : Maze_structure.t) =
  let start = Maze_structure.start maze in
  let visited = Position.Hash_set.create () in
  victory_check ~path:[] ~position:start ~visited maze
;;

(* let step_in_maze maze_structure direction = let actual_maze = *)

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let maze = create_initial_maze input_file in
        solve_maze maze]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;

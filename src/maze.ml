open! Core
module Maze_item = Char

module Position = struct
  type t = int * int [@@deriving compare]
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

let create_initial_maze maze_txt : Maze_structure.t =
  let maze_rows = String.split_lines maze_txt in
  let num_rows = List.length maze_rows in
  let maze_2d_list = List.map maze_rows ~f:(fun row -> String.to_list row) in
  let num_cols =
    match List.hd maze_2d_list with
    | Some char_list -> List.length char_list
    | _ -> 0
  in
  let maze_array =
    List.to_array maze_2d_list
    |> Array.map ~f:(fun row_list -> List.to_array row_list)
  in
  let start_position =
    Array.foldi maze_array ~init:None ~f:(fun row position which_row ->
      Array.foldi which_row ~init:position ~f:(fun col _ which_char ->
        if Char.( = ) which_char 'S' then Some (row, col) else None))
  in
  let exit_position =
    Array.foldi maze_array ~init:None ~f:(fun row position which_row ->
      Array.foldi which_row ~init:position ~f:(fun col _ which_char ->
        if Char.( = ) which_char 'E' then Some (row, col) else None))
  in
  let wall_positions =
    Array.foldi maze_array ~init:[] ~f:(fun row position_list which_row ->
      Array.foldi
        which_row
        ~init:position_list
        ~f:(fun col position_list which_char ->
        if Char.( = ) which_char '#'
        then position_list @ [ row, col ]
        else position_list @ []))
  in
  { start =
      (match start_position with Some start_pos -> start_pos | None -> 0, 0)
  ; exit =
      (match exit_position with Some end_pos -> end_pos | None -> 0, 0)
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
    [ (curr_row - 1, curr_col)
    ; (curr_row + 1, curr_col)
    ; (curr_row, curr_col - 1)
    ; (curr_row, curr_col + 1)
    ]
  in
  let in_bounds_moves =
    List.filter new_positions ~f:(fun position ->
      in_bounds ~height ~width position)
  in
  List.filter in_bounds_moves ~f:(fun in_bounds_pos ->
    List.for_all walls ~f:(fun wall_pos ->
      Position.compare wall_pos in_bounds_pos <> 0))
;;

let solve_maze (maze : Maze_structure.t) =
  let start = [Maze_structure.start maze] in
  let exit = Maze_structure.exit maze in
  let visited = Tuple.Hash_set.create () in
  let to_visit = Stack.create () in
  Stack.push to_visit start;
  let rec traverse () =
    match Stack.pop to_visit with
    | None -> ()
    | Some path ->
      let last_move = List.hd path in
      if not (Hash_set.mem visited last_move)
      then ( if (Position.compare last_move exit = 0) then (print_s [%message "path:" (path: Position.t List)]) else
        (Hash_set.add visited last_move;
        let adjacent_moves = valid_moves maze last_move in
        List.iter adjacent_moves ~f:(fun next_move ->
          Stack.push to_visit (next_move :: path));
      traverse ()))
  in
  traverse ()
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
        ignore (input_file : File_path.t);
        failwith "TODO"]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;

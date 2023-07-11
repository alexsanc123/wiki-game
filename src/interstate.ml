open! Core
module City = String
module Interstate = String

module Network = struct
  module Connection = struct
    module T = struct
      type t = City.t * Interstate.t * City.t [@@deriving compare, sexp]
    end

    include Comparable.Make (T)

    let of_string s =
      match String.split s ~on:',' with
      | [ city1; interstate; city2 ] ->
        Some
          ( City.of_string city1
          , Interstate.of_string interstate
          , City.of_string city2 )
      | _ -> None
    ;;
  end

  let format_connections s =
    let split_string = String.split_on_chars s ~on:[ ',' ] in
    let interstate = List.hd_exn split_string in
    let cities = List.tl_exn split_string in
    List.concat_map cities ~f:(fun city1 ->
      List.filter_map cities ~f:(fun city2 ->
        match String.( <> ) city1 city2 with
        | true ->
          Some
            ( City.of_string city1
            , Interstate.of_string interstate
            , City.of_string city2 )
        | false -> None))
  ;;

  type t = Connection.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let interstates_with_cities =
      In_channel.read_lines (File_path.to_string input_file)
    in
    let connections =
      List.concat_map interstates_with_cities ~f:(fun s ->
        format_connections s)
    in
    Connection.Set.of_list connections
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = Network.of_file input_file in
        printf !"%{sexp: Network.t}\n" network]
;;

module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (City)

module Dot = Graph.Graphviz.Dot (struct
  include G

  let edge_attributes _ = [ `Dir `None ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (city1, interstate, city2) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the
             graph if they don't already exist. *)
          G.add_edge graph city1 interstate city2);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;

open! Core
module Title = String

(* [get_linked_articles] should return a list of wikipedia article lengths
   contained in the input.

   Note that [get_linked_articles] should ONLY return things that look like
   wikipedia articles. In particular, we should discard links that are: -
   Wikipedia pages under special namespaces that are not articles (see
   https://en.wikipedia.org/wiki/Wikipedia:Namespaces) - other Wikipedia
   internal URLs that are not articles - resources that are external to
   Wikipedia - page headers

   One nice think about Wikipedia is that stringent content moderation
   results in uniformity in article format. We can expect that all Wikipedia
   article links parsed from a Wikipedia page will have the form
   "/wiki/<TITLE>". *)

let is_wiki_links link =
  let split_link = String.split_on_chars link ~on:[ '/' ] in
  List.exists split_link ~f:(fun word -> String.( = ) word "wiki")
;;

let get_linked_articles contents : string list =
  let open Soup in
  let link_nodes = parse contents $$ "a" in
  let links_nodes_list = to_list link_nodes in
  let links_list =
    List.filter_map links_nodes_list ~f:(fun link_node ->
      attribute "href" link_node)
  in
  let wiki_links =
    List.filter links_list ~f:(fun link -> is_wiki_links link)
  in
  let page_links =
    List.filter wiki_links ~f:(fun wiki_link ->
      match Wikipedia_namespace.namespace wiki_link with
      | Some _ -> false
      | None -> true)
  in
  List.dedup_and_sort page_links ~compare:String.compare
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

let get_title link =
  let split_link =
    String.split_on_chars link ~on:[ '/' ]
    |> List.map ~f:(Str.global_replace (Str.regexp {|\.|}) "")
    |> List.map ~f:(Str.global_replace (Str.regexp {|(|}) "")
    |> List.map ~f:(Str.global_replace (Str.regexp {|)|}) "")
    |> List.map ~f:(Str.global_replace (Str.regexp {| |}) "_")
    |> List.map ~f:(Str.global_replace (Str.regexp {|"|}) "")
    |> List.map ~f:(Str.global_replace (Str.regexp {|"|}) "")
  in
  match List.last split_link with Some title -> title | _ -> ""
;;

let get_content (how_to_fetch : File_fetcher.How_to_fetch.t) ~link =
  let new_link =
    match how_to_fetch with
    | Remote -> "https://en.wikipedia.org/" ^ link
    | _ -> link
  in
  get_linked_articles
    (File_fetcher.fetch_exn how_to_fetch ~resource:new_link)
;;

let rec form_link_connections ~link ~max_depth ~how_to_fetch =
  if max_depth = 0
  then []
  else (
    let their_links = get_content how_to_fetch ~link in
    List.concat_map their_links ~f:(fun link_on_page ->
      let surface_level_connection = link, link_on_page in
      if List.length (get_content how_to_fetch ~link:link_on_page) <> 0
      then
        [ surface_level_connection ]
        @ form_link_connections
            ~link:link_on_page
            ~max_depth:(max_depth - 1)
            ~how_to_fetch
      else [ surface_level_connection ]))
;;

module G = Graph.Imperative.Graph.Concrete (Title)

module Dot = Graph.Graphviz.Dot (struct
  include G

  let edge_attributes _ = [ `Dir `Forward ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

(* [visualize] should explore all linked articles up to a distance of
   [max_depth] away from the given [origin] article, and output the result as
   a DOT file. It should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let all_articles =
    form_link_connections ~link:origin ~max_depth ~how_to_fetch
  in
  let graph = G.create () in
  List.iter all_articles ~f:(fun (link1, link2) ->
    G.add_edge graph (get_title link1) (get_title link2);
    Dot.output_graph
      (Out_channel.create (File_path.to_string output_file))
      graph)
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and
   the destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the
   graph. *)
(* let rec find_article ~path ~(origin : string) ~how_to_fetch ~destination
   ~visited = let new_path = path @ [ origin ] in if String.equal origin
   destination then ( let title_path = List.map new_path ~f:(fun wiki_url ->
   get_title wiki_url) in print_s [%message "path:" (title_path : string
   list)]) else ( let linked_articles = get_content how_to_fetch ~link:origin
   in let unseen_articles = List.filter linked_articles ~f:(fun article ->
   not (Hash_set.mem visited article)) in List.iter unseen_articles ~f:(fun
   sub_link -> Hash_set.add visited sub_link; find_article ~path:new_path
   ~origin:sub_link ~visited ~destination ~how_to_fetch)) ;; *)

let find_path ?(max_depth = 10) ~origin ~destination ~how_to_fetch () =
  let destination_title = get_title destination in
  let visited = String.Hash_set.create () in
  let to_check = Queue.create () in
  Queue.enqueue to_check [ origin ];
  let rec traverse (depth : int) : string list option =
    match Queue.dequeue to_check with
    | None -> None
    | Some path ->
      if List.length path < max_depth - 1
      then (
        let parent_article =
          match List.last path with Some link -> link | None -> ""
        in
        if String.( = ) destination_title (get_title parent_article)
        then Some path
        else if not (Hash_set.mem visited parent_article)
        then (
          Hash_set.add visited parent_article;
          let their_links = get_content how_to_fetch ~link:parent_article in
          List.iter their_links ~f:(fun link_to_check ->
            Queue.enqueue to_check (path @ [ link_to_check ]));
          traverse depth)
        else traverse depth)
      else traverse depth
  in
  traverse max_depth
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace ->
          List.map trace ~f:(fun wiki -> get_title wiki)
          |> List.iter ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;

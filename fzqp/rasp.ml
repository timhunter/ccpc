#use "runner.ml"
#load "str.cma" (* for interpreting output *)

(* this file is a copy-and-modify of the stanford.ml pi.  Embarrassing,
 * but the deadline looms. 
 *)


module Rasp = struct
  let id = "Rasp"
 
  module Config = struct
    (* user-settable configurables *)
    (* right now there are no checks for filename collisions *)
    let input_filename = "Runner."^id^".tmp"
    let output_filename = input_filename ^ ".out"
    let err_filename = input_filename ^ ".err"
    let outputFormat = "u" (* just penn trees for now *)
    let nparses = "1" (* max 1 parse per sentence *)
    let parserdir = "/home/del82/projects/mcfgcky/rasp3os/scripts/"

    (* aux functions and defs *)
    let sd path = Filename.concat parserdir path (* for path substitution *)
    let stringcat x y = x^" "^y (* cat strings with a space between *)
      (* should use String.concat *)
    
    (* command-line components *)
    let cat_cmd = List.fold_left stringcat "" ["cat"; input_filename; "|"]
    let interpreter_cmd = sd "rasp.sh"
    let parser_options = ("\"-o"^outputFormat)^" "^("-n"^nparses^"\"") 
    let output_format_cmd =  stringcat "-p " parser_options
    let output_file_cmd =  "1> " ^ output_filename ^" 2> " ^ err_filename


    let parsing_command_list = [
      cat_cmd;
      interpreter_cmd;
      output_format_cmd;
      output_file_cmd;
    ]
      
    let parsing_command = String.concat " " parsing_command_list

  end

    (* this astonishing piece of code appears to be the most concise way to
       strip whitespace from the begining and end of a string in ocaml.  *)
  let trim str =   if str = "" then "" else   let search_pos init p next =
    let rec search i =
      if p i then raise(Failure "empty") else
	match str.[i] with
	  | ' ' | '\n' | '\r' | '\t' -> search (next i)
	  | _ -> i
    in
      search init   in   let len = String.length str in   try
      let left = search_pos 0 (fun i -> i >= len) (succ)
      and right = search_pos (len - 1) (fun i -> i < 0) (pred)
      in
	String.sub str left (right - left + 1)   with   
	  | Failure "empty" -> "" ;;

      
  let read_file filename = 
    let lines = ref [] in
    let chan = open_in filename in
      try
	while true; do
	  lines := input_line chan :: !lines
	done; ""
      with End_of_file ->
	close_in chan;
	List.fold_left Config.stringcat "" (List.rev !lines)


  let write_sent (sentences : Runner.sentence list) =
    let out_chan =  open_out Config.input_filename in
    let rec ws_aux sentences = match sentences with
      [] -> ()
    | h::t -> output_string out_chan ((snd h)^"\n"); ws_aux t
    in
      ws_aux sentences; close_out out_chan
      
  let run_parser_through_os () =
    ignore (Sys.command Config.parsing_command)
 
  let read_res () =
    (read_file Config.output_filename), (read_file Config.err_filename)


  (* these are for matching the output tags *)
  let possible_tags = ["tree-rasp:";"sparkle:";"susanne:";"upenn:";"alias:";"gr-list:";"weighted:";"ewg-weighted:"]

  let match_any_of_list the_list string = 
    let m = List.map Str.regexp_string the_list in (* strings -> regexps *)
    let matches = List.map (fun x -> Str.string_match x string 0) m in
      List.fold_left (or) false matches
  
  let match_tags = match_any_of_list possible_tags


  (* these next few functions are meant to be chained; each takes the former's 
     output as input. *)
  let get_parsing_result sentence_list =
    write_sent sentence_list;
    ignore (run_parser_through_os ());
    read_res ()

  let interpret_penn_output (output, errput) =
    (* right now errput is ignored *)
    let root = Str.regexp_string "upenn:" in 
    List.map (fun x -> trim x) 
      (List.filter (fun x -> trim x <> "") (Str.split root output))
      
  let extract_trees the_list = 
    let extract_number item = 
      let loc = String.index item ' ' in
      let len = String.length item in
	(String.sub item 0 loc),(String.sub item (loc+1) (len-(loc+1)))
    in
    let r = Str.regexp_string "(TOP" in
      List.filter (fun x -> Str.string_match r x 0) 
	(List.map snd 
	   (List.map extract_number the_list))


  let make_key_value penn_list = 
    let add_key item = "penn={"^item^"}" in
      List.map add_key penn_list

  let restore_ids ids keyval_list = 
    ((List.combine ids keyval_list ) : Runner.parsed_sentence list)

  let parse sentence_list = 
    let ids = fst (List.split sentence_list) in (* save the ids for later *)
      restore_ids ids
	(*(make_key_value*)
	   (extract_trees
	      (interpret_penn_output 
		 (get_parsing_result sentence_list)))


  let initialize argument_string = 0 (* noop *)
    
  let close () = [] (* noop *)


  let pi = new Runner.parser_interface
    id initialize parse	close

end
(*

    let rec split_on_dnl = function
      | []            -> ([] , [])
      | ""  ::[]      -> ([] , [])
      | h   ::[]      -> ([h], [])
      | ""  ::""  ::t -> ([] , t)
      |  "" ::  h ::t -> (split_on_dnl (h::t))
      | h1  ::  h2::t -> let a = split_on_dnl (h2::t) in ((h1::(fst a)), (snd a))
							 *)

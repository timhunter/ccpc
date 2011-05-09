
#use "runner.ml"
#load "str.cma" (* for interpreting output *)

module Stanford = struct
  let id = "Stanford"
 
  module Config = struct
    (* user-settable configurables *)
    (* right now there are no checks for filename collisions *)
    let input_filename = "Runner."^id^".tmp"
    let output_filename = input_filename ^ ".out"
    let err_filename = input_filename ^ ".err"
    let outputFormat = "penn"
    let parserdir = "/home/del82/projects/mcfgcky/stanford-parser-2010-11-30"
    let grammar_to_use = "englishPCFG.ser.gz"

    (* aux functions and defs *)
    let sd path = Filename.concat parserdir path (* for path substitution *)
    let stringcat x y = x^" "^y (* cat strings with a space between *)
    
    (* command-line components *)
    let interpreter_cmd = "java"
    let memory_limit_cmd = "-mx150m" 
    let classpath_cmd = "-cp \""^(sd "stanford-parser.jar:")^(sd "*") ^ "\""
    let parser_class = "edu.stanford.nlp.parser.lexparser.LexicalizedParser"
    let output_format_cmd = stringcat "-outputFormat" outputFormat
    let grammar_cmd = sd grammar_to_use
    let output_file_cmd =  "> " ^ output_filename ^" 2> " ^ err_filename


    let parsing_command_list = [
      interpreter_cmd;
      memory_limit_cmd;
      classpath_cmd;
      parser_class;
      output_format_cmd;
      grammar_cmd;
      input_filename;
      output_file_cmd ]

    let parsing_command = List.fold_left stringcat "" parsing_command_list

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

  (* these next few functions are meant to be chained; each takes the former's 
     output as input. *)
  let get_parsing_result sentence_list =
    write_sent sentence_list;
    ignore (run_parser_through_os ());
    read_res ()
      
  let interpret_penn_output (output, errput) =
    (* right now errput is ignored *)
    let root = Str.regexp_string "(ROOT" in 
    let reattach s = "(ROOT "^s in
      List.map reattach (List.filter (fun x -> trim x <> "") 
			   (Str.split root output))

  let make_key_value penn_list = 
    let add_key item = "penn={"^item^"}" in
      List.map add_key penn_list

  let restore_ids ids keyval_list = 
    ((List.combine ids keyval_list ) : Runner.parsed_sentence list)

  (* implementations required in order to create a pi object *)
  let parse sentence_list = 
    let ids = fst (List.split sentence_list) in (* save the ids for later *)
      restore_ids ids 
	(*(make_key_value *)
	   (interpret_penn_output 
	      (get_parsing_result sentence_list))
    

  let initialize argument_string = 0 (* noop *)
    
  let close () = [] (* noop *)


  let pi = new Runner.parser_interface
    id initialize parse	close

end


(* 
set scriptdir=`dirname $0`
java -mx150m -cp "$scriptdir/stanford-parser.jar:" edu.stanford.nlp.parser.lexparser.LexicalizedParser -outputFormat "penn" $scriptdir/englishPCFG.ser.gz -
*)

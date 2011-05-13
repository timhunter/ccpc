
(* it looks like all these paths have to be typed in directly. sheesh. *)

#directory ".."
#load "profiling.cmo";;
#load "util.cmo";;
#load "kbest/rational.cmo";;
#load "nelist.cmo";;
#load "rule.cmo";;
#load "mcfgread/read.cmo";;
#load "mcfgread/lexer.cmo";;
#load "chart.cmo";;
#load "tables.cmo";;
#load "parser.cmo";;
#load "main.cmo";;

 
#use "runner.ml";;
module Mcfg2 = struct
  let id = "mcfgcky2"

(* utilities; copied from stanford.ml (refactoring, anyone?) *)
    
  module Config = struct
    (* user-settable configurables *)
    (* right now there are no checks for filename collisions *)
    let input_filename = id^".morpher.input"
    let output_filename = id^".morpher.output"
      

    (* command-line for morphing *)
    let morph_command = "./morph_all "^input_filename^" > "^output_filename
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
	done; []
      with End_of_file ->
	close_in chan;
	(List.rev !lines)

  let write_sent (sentences : Runner.sentence list) =
    let out_chan =  open_out Config.input_filename in
    let rec ws_aux sentences = match sentences with
      [] -> ()
    | h::t -> output_string out_chan ((snd h)^"\n"); ws_aux t
    in
      ws_aux sentences; close_out out_chan

  (* now down to business: *)
    
  let ( grammar: Rule.r list ref) = ref []
    
  let morph sentences = 
    write_sent sentences;
    ignore (Sys.command Config.morph_command);
    let morphed = read_file Config.output_filename in
      (List.combine (List.map (fun x -> fst x) sentences) 
	 morphed : Runner.sentence list)
      
  let tokenize sentences = 
    List.map (fun x -> ((fst x), (Util.split ' ' (snd x)))) sentences

  let parse_one sentence = 
    let parsed = Main.timed_parse !grammar sentence in
    let items_in_chart = List.length (snd parsed) in
      (* turns out returning the whole chart is a bad idea, 'cause it's huge.*)
    (*let chart_strings = List.map 
      (fun x -> Chart.to_string x sentence) 
      (snd parsed)
    in 
    let chart_string = String.concat " " chart_strings in *)
     (string_of_float (fst parsed))^" "^
     (string_of_int items_in_chart)

  let parse_all sentences = List.map parse_one sentences

  let re_id ids sentences = List.combine ids sentences


  (* pi functions *)
  let initialize argument_string = 
    grammar := Main.get_input_grammar argument_string; 0
      (* get_input_grammar calls exit if it can't find the grammar. *)

  let close () = [] (*noop*)

  let parse sentence_list = 
    let a = List.split sentence_list in
    let tokenized = tokenize (morph (sentence_list)) in
    re_id (fst a) (parse_all (List.map (fun x -> snd x) tokenized))


  let pi = new Runner.parser_interface
    id initialize parse close
end

let g = Main.get_input_grammar "../grammars/mcfgs/larsonian.mcfg"
let s = Util.split ' ' "they have -ed forget -en that the boy who tell -ed the story be -s so young"

let i () = Mcfg2.initialize "../grammars/mcfgs/larsonian.mcfg"
let i2 () = Mcfg2.initialize "../grammars/mcfgs/larsonian.deemptied.mcfg"

(* non-deemptied:
 * 12.1701500000000014,
 *
 * deemptied: 
 * 11.914188 
 *)
  

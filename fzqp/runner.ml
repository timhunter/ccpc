module Runner = struct
  

  type sentence = (string * string)
  type parsed_sentence = (string * string)
  type result = (string * parsed_sentence)

  exception NotImplementedException of string
  exception InitializationException of (int * string)


  class parser_interface 
    (id : string ) 
    (initialize : string -> int ) 
    (parse : sentence list -> parsed_sentence list) 
    (close : unit -> parsed_sentence list) = 
  object (self : 'self)

    val parser_id = id

    method initialize = initialize

    method parse = parse
    method close = close
    method get_id = parser_id
  end
      

  let runner = 
  object (self : 'self)
    val mutable pi_list = []

    method add_pi_with_args (the_parser : parser_interface) (arguments : string) = 
      (* don't add and return 1 if ID already exists on the list *)
      if List.mem the_parser#get_id (self#list_pis ()) then  1
      else 
	let return_code = the_parser#initialize arguments in
	  if return_code <> 0
	  then raise (InitializationException (return_code, the_parser#get_id))
	  else pi_list <- the_parser :: pi_list;
	  0 (* return 0 if the pi is successfully initialized and added *)
    method add_pi the_parser = self#add_pi_with_args the_parser ""
    method remove_pi ( pid : string ) = 
      let kept, removed = List.partition (fun x -> x#get_id <> pid) pi_list
		(* String equality is not != and == but <> and = *)
      in pi_list <- kept; ((List.hd removed)#close ())

    method tag_parsed_sentences (tag : string) 
      (sentences : parsed_sentence list) = 
      (List.map (fun x -> (tag, x)) sentences : result list)

    method clear_pi_list () = 
      let leftovers = 
	List.map (fun pi -> (self#tag_parsed_sentences (pi#get_id ) 
			       (pi#close ())))
	  pi_list
      in
	pi_list <- [];
	List.flatten leftovers 

    method parse_these (sentences : sentence list) 
      (the_parser : parser_interface) = 
      self#tag_parsed_sentences (the_parser#get_id) (the_parser#parse sentences)

    method parse_with the_parser sentences = 
      self#parse_these sentences the_parser

(** Parse a list of sentences with each of the parsers that have been registered with the runner. *)
    method parse_all (sentences : sentence list) = 
      List.flatten ( List.map (self#parse_these sentences) pi_list )
    method list_pis () = List.map (fun x -> x#get_id) pi_list
    method get_pis () = pi_list
    method get_pi_by_id (name) = List.filter (fun x -> x#get_id = name) pi_list
  end

 end


(** Format of parsed_sentences
*)


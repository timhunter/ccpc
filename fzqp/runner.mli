(** A parser runner, providing types, objects and functions for running multiple parsers on a list of sentences and aggregating the results.
*)


module Runner :
  sig

  (** a sentence is a pair whose fst is a sentence identifier and whose snd is a sentence to be parsed *)
    type sentence = string * string

  (** a parsed_sentence is a pair whose fst is a sentence identifier and whose snd is the string containing the resulting parse *)
    type parsed_sentence = string * string

  (** a result is a pair whose fst is the string identifier of the parser used to parse the sentence and the second is the parsed_sentence *)
    type result = string * parsed_sentence

(** raised when a particular function is not yet implemented *)
    exception NotImplementedException

(** Raised when a parser_interface's initialize function returns a non-zero value, indicating some sort of initialization failure.  *)
    exception InitializationException of (int * string)

  (** A parser_interface is the driver that the Runner uses to parse sentences using the parser. The constructor for a parser_interface *) 
    class parser_interface :
      string ->
      (string -> int) ->
      (sentence list -> parsed_sentence list) ->
      (unit -> parsed_sentence list) ->
      object

    (** the parser_id is the string by which the parser will be identified by the runner and all of the runner's output.*)
        val parser_id : string

        method close : unit -> parsed_sentence list
        method get_id : string
        method initialize : string -> int

    (** This is the parsing function.  It will be called on a sentence list and returns a parsed_sentence list.  *)
        method parse : sentence list -> parsed_sentence list
      end
    val runner :

(** Called on a (parser_interface * argument_string) pair, add_pi causes the parser interface to be registered with the runner, and the interface's initialize function to be called with the given argument_string *)
      < add_pi : parser_interface * string -> int;

        clear_pi_list : unit -> result list;
        get_pi_by_id : string -> parser_interface list;
        get_pis : unit -> parser_interface list;
        list_pis : unit -> string list;
        parse_all : sentence list -> result list list;
        parse_these : sentence list -> parser_interface -> result list;
        parse_with : parser_interface -> sentence list -> result list;
        remove_pi : string -> parsed_sentence list;
        tag_parsed_sentences : string -> parsed_sentence list -> result list >
  end

module Runner :
  sig
    type sentence = string * string
    type parsed_sentence = string * string
    type result = string * parsed_sentence
    exception NotImplementedException of string
    exception InitializationException of (int * string)
    class parser_interface :
      string ->
      (string -> int) ->
      (sentence list -> parsed_sentence list) ->
      (unit -> parsed_sentence list) ->
      object
        val parser_id : string
        method close : unit -> parsed_sentence list
        method get_id : string
        method initialize : string -> int
        method parse : sentence list -> parsed_sentence list
      end
    module Util :
      sig
        val gensym_id : unit -> string
        val make_sentence_list : string list -> sentence list
        val range : int -> int list
        val n_of_each : int -> (string * 'a) list -> (string * 'a) list
        val trim : string -> string
        val read_file : string -> string list
      end
    val runner :
      < add_pi : parser_interface -> int;
        add_pi_with_args : parser_interface -> string -> int;
        clear_pi_list : unit -> result list;
        get_pi_by_id : string -> parser_interface list;
        get_pis : unit -> parser_interface list;
        list_pis : unit -> string list;
        parse_all : sentence list -> result list;
        parse_these : sentence list -> parser_interface -> result list;
        parse_with : parser_interface -> sentence list -> result list;
        remove_pi : string -> parsed_sentence list;
        tag_parsed_sentences : string -> parsed_sentence list -> result list >
  end

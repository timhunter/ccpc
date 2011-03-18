module Item_Key =  
        struct
	  type t = MCFG_ParserGen.item
	  let compare item_1 item_2 = 
	    match item_1 with 
	      | MCFG_ParserGen.ParseItem (a,b) -> 
                  match item_2 with 
		    | MCFG_ParserGen.ParseItem (c,d) -> 0
	      
        end

module Chart_Table = Set.Make(Item_Key)
(*
module Rule_Key =
        struct
	  type t = Rule.r

	  let compare rule_1 rule_2 =
	    match rule_1 with
	      | Rule.NonTerminating (a,b,c) -> 0 
	      | _ -> 0
	end

module Rule_Table = Set.Make(Rule_Key)
*)

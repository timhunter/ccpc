type t = (string, int list) Hashtbl.t
    
let create () = Hashtbl.create 101
  
let add tbl tok component = 
  try
    let comp_list = Hashtbl.find tbl tok in
    if not (List.mem component comp_list)
    then Hashtbl.add tbl tok (component::comp_list)
    else ()
  with Not_found -> 
    Hashtbl.add tbl tok [component]
      
let lookup tbl tok  =
  try
    Hashtbl.find tbl tok
  with Not_found ->
    []
      
let mem tbl tok component =
  List.mem component (lookup tbl tok)
    
let remove tbl tok component =
  try
    let comp_list = Hashtbl.find tbl tok in
    Hashtbl.add tbl tok (List.filter (fun a -> a != component) comp_list)
  with Not_found -> 
    ()

let clear tbl =
  Hashtbl.clear tbl

let get_items tbl =
  Hashtbl.fold (fun k v acc -> (k,v)::acc) tbl []




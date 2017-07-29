(* Takes in a string and a separator, and separates the string into a list of
 * substrings where each substring is between two separators or between a
 * separator and the beginning/end of the string *)
let explode (s: string) (space: string) : string list =
  let rec build (curr: string) (buffer: string) (lst: string list) : string list =
    let len = String.length curr in
    if len = 0 then buffer::lst
    else 
      let c = String.sub curr (len - 1) 1 in
      if len = 1 then (c ^ buffer)::lst
      else 
        let s' = String.sub curr 0 (len - 1) in
        if c = space then build s' "" (buffer::lst)
        else build s' (c ^ buffer) lst in
  build (String.trim s) "" []



let expander is_expr contents = "Util.debug_fast (lazy (Printf.sprintf " ^ contents ^ "))" in
Quotation.add "DEBUG" (Quotation.ExStr expander)

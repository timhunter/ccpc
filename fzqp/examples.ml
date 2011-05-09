
  let mydummypi = new Runner.parser_interface "dummypi"  (* id is dummypi *)
    (fun x -> 0) (* initialize is a noop *)
    (fun x -> List.map (fun (z, y) -> (z, "dummypi parsed "^y)) x) (*  parse *)
    (fun x -> []) (* close is also a noop *)


  let mydummypi2 = new Runner.parser_interface "dummypi2"  (* id is dummypi *)
    (fun x -> 0) (* initialize is a noop *)
    (fun x -> List.map (fun (z, y) -> (z, "dummypi2 parsed "^y)) x)(* parse *)
    (fun x -> [("pi2_resid", 
		 "this is a residual sentence returned by dummpi2 on close")])		 

  let sentences = [   
    ("id0", "This is sentence 0.");
    ("id1", "This is sentence 1.");
    ("id2", "This is sentence 2.");
    ("id3", "This is sentence 3.");
    ("id4", "This is sentence 4.");
    ("id5", "This is sentence 5.");
    ("id6", "This is sentence 6.");
    ("id7", "This is sentence 7.");
    ("id8", "This is sentence 8.");
    ("id9", "This is sentence 9.");
    ("id10", "This is sentence 10.");
    ("id11", "This is sentence 11.");
    ("id12", "This is sentence 12.");
    ("id13", "This is sentence 13.");
    ("id14", "This is sentence 14.");
    ("id15", "This is sentence 15.");
  ]

  let sentences2 = [
    ("first-line", "The limerick is furtive and mean.");
    ("second-line", "You must keep it in close quarantine.");
    ("third-line", "Or it sneaks to the slums, and promptly becomes disorderly, drunk, and obscene.")
  ]

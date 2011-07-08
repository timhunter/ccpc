let clean =[
 0.2, "NP", ["SPECNP"; "NBAR"];
 0.4, "NP",["I"];
 0.4, "NP",["John"];

 1.0, "SPECNP", ["DT"];
 0.5, "NBAR", ["NBAR"; "S[+R]"];
 0.5, "NBAR", ["N"];
 1.0, "S", ["NP"; "VP"];
 0.86864638, "S[+R]", ["NP[+R]"; "VP"];
 0.13135362, "S[+R]", ["NP[+R]"; "S/NP"];
 1.0, "S/NP", ["NP"; "VP/NP"];

 (1.0 /. 2.0), "VP/NP", ["V[SUBCAT2]";"NP/NP"];
 (1.0 /. 2.0), "VP/NP", ["V[SUBCAT3]";"NP/NP"; "PP[to]"];

 (1.0 /. 3.0), "VP", ["V[SUBCAT2]";"NP"];
 (1.0 /. 3.0), "VP", ["V[SUBCAT3]"; "NP"; "PP[to]"];
 (1.0 /. 3.0), "VP", ["V[SUBCAT4]"; "PP[for]"];

 (1.0 /. 3.0), "V[SUBCAT2]",["met"];
 (1.0 /. 3.0), "V[SUBCAT2]",["attacked"];
 (1.0 /. 3.0), "V[SUBCAT2]",["disliked"];

 1.0, "V[SUBCAT3]", ["sent"];
 1.0, "V[SUBCAT4]", ["hoped"];
 1.0, "PP[to]", ["PBAR[to]"; "NP"];
 1.0, "PBAR[to]", ["P[to]"];
 1.0, "P[to]", ["to"];
 1.0, "PP[for]", ["PBAR[for]";"NP"];
 1.0, "PBAR[for]", ["P[for]"];
 1.0, "P[for]", ["for"];
 1.0, "NP[+R]", ["who"];
 0.5, "DT", ["the"];
 0.5, "DT", ["a"];
 (1.0 /. 6.0), "N", ["editor"];
 (1.0 /. 6.0), "N", ["senator"];
 (1.0 /. 6.0), "N", ["reporter"];
 (1.0 /. 6.0), "N", ["photographer"];
 (1.0 /. 6.0), "N", ["story"];
 (1.0 /. 6.0), "N", ["ADJ";"N"];
 1.0, "ADJ", ["good"];
 1.0, "NP/NP", [""]
]
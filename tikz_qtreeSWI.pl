/* tikz_qtree.pl
 *
 * Written by Ryan Musa, March 2011
 * Based on package tikz-qtree, by David Chiang
 *
 * A LaTeX/eepic interface for the drawTree tree-drawing package.
 *
 * latex_tree/1 writes a LaTeX file tiks-qtree.tex in the current
 * directory.  It is easy to extract the picture from this
 * to paste into a file.
 *
 * Example:
 * tikz_qtree(s/[np/[maria/[]],vp/[sings/[]]]).
 */
 
 :- module(tikz_qtree, [tikz_qtree/1, tikz_qtree/2]).
 
 tikz_qtree(Tree) :- openstream(Tree, Stream0, 'tikz_qtree.tex'), format(Stream0, "\\Tree ", []), decodetree(Tree, Stream0), closestream(Stream0), !.
 tikz_qtree(Tree, Name) :- openstream(Tree, Stream0, Name), format(Stream0, "\\Tree ", []), decodetree(Tree, Stream0), closestream(Stream0), !, halt.
 
 openstream(Tree, Stream, Name) :-
	open(Name, write, Stream),
        format(Stream, "\\documentclass{article}~n", []),
		format(Stream, "\\usepackage{tikz}~n", []),
		format(Stream, "\\usepackage{tikz-qtree-compat}~n", []),
		format(Stream, "\\usepackage[paperwidth=20in,paperheight=15in]{geometry}~n", []),

        format(Stream, "\\begin{document}~n", []),
        format(Stream, "\\begin{tikzpicture}~n", []),
				format(Stream, "\\tikzset{level distance=30pt, sibling distance=30pt}~n", []),
				format(Stream, "\\begin{tiny}~n", []),
				format(Stream, "\\hspace{-3.0in}~n", []),
        format(Stream, "%tikz_qtree(~w).~n%~n", [Tree]).

writetree(Tree, Stream) :-
    format(Stream, "\\Tree ~w~n", [Tree]).
	
decodetree([Leaf]/[], Stream) :-
	format(Stream, "[.~w ] ", [Leaf]).

decodetree(Leaf/[], Stream) :-
	format(Stream, "[.~w ] ", [Leaf]).
	
decodetree(Node/Children, Stream) :-
	format(Stream, "[.~w ", [Node]),
    decodetree(Children, Stream), 
	format(Stream, "] ", []).
	
decodetree([], Stream) :-
	format(Stream, "", []).

decodetree([Tree], Stream) :-
	decodetree(Tree, Stream).
	
decodetree([Tree|Trees], Stream) :-
	decodetree(Tree, Stream),
	decodetree(Trees, Stream).
	
 closestream(Stream) :-
				format(Stream, "~n\\end{tiny}~n", []),
        format(Stream, "\\end{tikzpicture}~n", []),
		format(Stream, "\\end{document}~n", []),
	close(Stream).


README for Drawing Derived Structure Project
Ryan Musa ram372@cornell.edu
LING 4485, Spring 2011

DDS Contents:
	openccg: directory for OpenCCG Visualization
	mgcky-swi: directory for Stabler, et al., SWI-Prolog MG system 

*** NB: You must have pdflatex (as well as packages tikz, tikz-qtree, and tikz-qtree-compat installed / on-the-fly package installation) on your PATH ***

openccg:
	Usage:
	> java -jar ccgvis.jar
	
	You will be prompted to select an OpenCCG grammar file (.xml extension) to use for this session
	Example (toy) grammars exist as ./grammars/LANGUAGE_NAME/grammar.xml
	
	Once the grammar is loaded, enter possible constituents in the text box, and click 'Parse' to see results, if any exist
	Derived trees are stored in 'results.pdf' (overwritten each session)
	

mgcky-swi:
	Contents:
	setup.pl: toplevel Prolog environment
	grammars: directory containing grammars reference in setup.pl
	tree-display: directory containing implementation of various tree-drawing utilities
	parser: Prolog parser files (selected in setup.pl)
	
	Usage:
	setup.pl
	> showParse([this, is, my, sentence]).
	'h' for help menu; 'w' runs all three utilities
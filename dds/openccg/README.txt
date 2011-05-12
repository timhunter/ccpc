README for OpenCCG Visualization
Ryan Musa ram372@cornell.edu
LING 4485, Spring 2011

OpenCCG Contents:
	grammars: directory containing sample OpenCCG grammars (classic XML format, not DotCCG)
	ccgvis.jar: java executable for basic visualization system
	pl.bat: basic script to run pdflatex (workaround for java Runtime,exec() command)

*** NB: You must have pdflatex (as well as packages tikz, tikz-qtree, and tikz-qtree-compat installed / on-the-fly package installation) on your PATH ***

Usage:
	> java -jar ccgvis.jar
	
	You will be prompted to select an OpenCCG grammar file (.xml extension) to use for this session
	Example (toy) grammars exist as ./grammars/LANGUAGE_NAME/grammar.xml
	
	Once the grammar is loaded, enter possible constituents in the text box, and click 'Parse' to see results, if any exist
	Derived trees are stored in 'results.pdf' (overwritten each session)

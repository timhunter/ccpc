%   Module : read_in
%   Author : Richard A. O'Keefe
%   Updated: 21 Apr 1987
%   Purpose: Read in a sentence as a list of words.

%   There is a shared version of this file written by Lawrence Byrd.
%   This version was written entirely from scratch for Quintus.
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- module(read_in, [
	read_in/1
   ]).
:- mode
	read_in(?),
	ri_sent(+, -),
	ri_stop(+, -),
	ri_word(+, -, -),
	ri_nmbr(+, -, -).

sccs_id('"@(#)87/04/21 readin.pl	9.1"').


%   read_in(Words)
%   reads characters until it finds a period (. ? or !) at the end of a
%   line, that is, followed by any number of spaces but nothing else.
%   Words are sequences of letters (in either case, forced to lower
%   case), or strings of digits, or punctuation marks.  Other characters
%   act as word separators, and are otherwise ignored.  The subroutines
%   in this file all start with ri_, to avoid a collision with a user's
%   predicates.  Note that this predicate may read more than one
%   sentence; it is best thought of as reading paragraphs.



read_in(Words) :-
	get0(C),
	ri_sent(C, Found),
	Words = Found.


ri_sent(C, Words) :-
	(   C < 0	-> Words = ['']
	;   C =< " "	-> get0(D), ri_sent(D, Words)
	;   Words = [Word|Rest], D is C\/ 32,
	    (   C =:= "." -> Word = ., get0(F), ri_stop(F, Rest)
	    ;   C =:= "!" -> Word = !, get0(F), ri_stop(F, Rest)
	    ;   C =:= "?" -> Word = ?, get0(F), ri_stop(F, Rest)
	    ;   D >= "a", D =< "z" ->
		get0(E), ri_word(E, Chars, F),
		name(Word, [D|Chars]), ri_sent(F, Rest)
	    ;   C >= "0", C =< "9" ->
		get0(E), ri_nmbr(E, Chars, F),
		name(Word, [C|Chars]), ri_sent(F, Rest)
	    ;   get0(F),
		name(Word, [C]),       ri_sent(F, Rest)
	    )
	).


ri_stop(C, Rest) :-
	(   C =:= " " -> get0(D), ri_stop(D, Rest)	% space
	;   C =:=  9  -> get0(D), ri_stop(D, Rest)	% tab
	;   C  <  " " -> Rest = []			% "end of line"
	;   ri_sent(C, Rest)
	).


ri_word(C, [Lower|Lowers], F) :-
	(   C >= "a", C =< "z"	-> Lower  = C
	;   C =:= "_"		-> Lower  = C
	;   C >= "A", C =< "Z"	-> Lower is C-"A"+"a"
	;   C >= "0", C =< "9"	-> Lower  = C
	), !,
	get0(D),
	ri_word(D, Lowers, F).
ri_word(F, [], F).


ri_nmbr(C, Digits, F) :-
	(   C >= "0", C =< "9" ->
	    Digits = [C|Digits1],
	    get0(D),
	    ri_nmbr(D, Digits1, F)
	;   C =:= "_" ->
	    get0(D),
	    ri_nmbr(D, Digits, F)
	;   F = C, Digits = []
	).



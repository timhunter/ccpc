%   File   : chineseabs_promotion.pl
%   Author : Zhong Chen
%   Updated: July 22nd 2012
%   altered by John to differentiate between overt Nouns and covert Nouns
% It follows Den Dikken & Singhapreecha (2004)'s theory of de (cf. Ou (2007), pp.922). de is treated as a LINKER.


% Proper Nouns
 ['Pronoun']::['N',-case].

% Nouns
 ['Noun']::['N'].    % in possessives
 ['Noun']::['N',-case].
 ['Noun']::['N',-case,-wh].  % nouns that can be relativized

% Null argument (pro)
 []::['N-null'].            %JTH
 []::['N-null',-case].      %JTH
 []::['N-null',-case,-wh].  %JTH

% One-place predicates (intransitive verbs and adjectives)
 ['Vi']::['V'].	% had bad intensions


% Two-place predicates (transitive verbs)
 ['Vt']::[='N',+case,'V'].
 ['Vt']::[='N-null',+case,'V'].
 ['Vt']::[='Poss',+case,'V'].	% take possessive as arguments

% ['Vt']::[='F',+case,'V'].

% little v
 []::[=>'V',='N','v']. %v
 []::[=>'V',='N-null','v']. %v
 []::[=>'V',='Poss','v']. %v


% []::[=>'V',='F','v']. %v


% tense
 []::[='v',+case,'T']. %tense


 []::[='T','C'].


% Relative Clauses
% SR:   showParse([Vt,Noun,de,Noun,Vt,Noun]).
% OR:   showParse([Noun,Vt,de,Noun,Vt,Noun]).

 []::[=>'V',='N','vRel'].   	%v
 []::[=>'V',='N-null','vRel'].   	%v  %JTH
 []::[=>'V',='Poss','vRel'].	%v
 []::[='vRel',+case,'TRel',-f]. %tense


 []::[='TRel',+wh,'CRel'].

 [de]::[='CRel',+f,'F'].

 []::[='F','N',-case].

% relativizer de hoist TRel to Spec CP
% [de]::[='TRel',+f,'CRel',-k].

% raise argument NP to head position
% []::[='CRel',+wh,'N'].

% need to move CP to Spec NP
% []::[='N',+k,'N',-case].


% possessive de
 [de]::[='N',='N','Poss',-case].


% The root.
startCategory('C').

%   File   : chineseabs_promotion.pl
%   Author : Zhong Chen
%   Updated: Mar 7th 2012


% Proper Nouns
 ['Pronoun']::['N',-case].

% Nouns
 ['Noun']::['N'].    % in possessives
 ['Noun']::['N',-case].
 ['Noun']::['N',-case,-wh].  % nouns that can be relativized

% Null argument (pro)
 []::['N'].
 []::['N',-case].
 []::['N',-case,-wh].

% One-place predicates (intransitive verbs and adjectives)
 ['Vi']::['V'].	% had bad intensions


% Two-place predicates (transitive verbs)
 ['Vt']::[='N',+case,'V'].	
 ['Vt']::[='Poss',+case,'V'].	% take possessive as arguments

% little v
 []::[=>'V',='N','v']. %v
 []::[=>'V',='Poss','v']. %v

% tense
 []::[='v',+case,'T']. %tense


 []::[='T','C'].


% Relative Clauses
% SR:   showParse([Vt,Noun,de,Noun,Vt,Noun]).
% OR:   showParse([Noun,Vt,de,Noun,Vt,Noun]).

 []::[=>'V',='N','vRel'].   	%v
 []::[=>'V',='Poss','vRel'].	%v
 []::[='vRel',+case,'TRel',-f]. %tense

% relativizer de hoist TRel to Spec CP
 [de]::[='TRel',+f,'CRel',-k].

% raise argument NP to head position
 []::[='CRel',+wh,'N'].

% need to move CP to Spec NP
 []::[='N',+k,'N',-case].


% possessive de
 [de]::[='N',='N','Poss',-case].


% The root.
startCategory('C').

%   File   : chinese3abs.pl
%   Author : Zhong Chen
%   Updated: Nov 14th 2011
% This grammar assumes there is no DP in Chinese.


% pronouns

['Pronoun']::['N',-case].

% Common Nouns
['Noun']::['N',-case].

% Common Nouns in PossP
['Noun']::['N'].


% Null argument (pro/the relativized NP)
[]::['N-null',-case]. 

% One-place predicates (intransitive verbs and adjective predicates)
['Vi']::['V'].

% Two-place predicates (transitive verbs)
% used in an RC, only have NP arguments
['Vt']::[='N',+case,'V'].

['Vt']::[='N-null',+case,'V'].

['Vt']::[='Poss',+case,'V'].


[]::[=>'V',='N','v']. %v
[]::[=>'V',='N','vRel']. %v

[]::[=>'V',='N-null','v']. %v
[]::[=>'V',='N-null','vRel']. %v

[]::[=>'V',='Poss','v']. %v
[]::[=>'V',='Poss','vRel']. %v


[]::[='v',+case,'T']. %tense


[]::[='T','C'].



% Relative Clauses: an Adjunction Analysis
% SSR showParse([Vt,Noun,de,Noun,Vt,Noun]).
% SOR showParse([Noun,Vt,de,Noun,Vt,Noun]).
% OSR showParse([Noun,Vt,Vt,Noun,de,Noun]).
% OOR showParse([Noun,Vt,Noun,Vt,de,Noun]).

% Nothing special for T; -f is used to ensure RC preceeds de
 []::[='vRel',+case,'TRel',-f].

 [de]::[='TRel',+f,'CRel'].

% Relative CP can left-adjoin onto the NP head
 ['CRel']>>['N'].
['CRel']>>['N-null'].
% % genitive de
 [de]::[='N',='N','Poss',-case].


% The root.
startCategory('C').

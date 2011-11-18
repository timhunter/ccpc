%   File   : chinese3abs.pl
%   Author : Zhong Chen
%   Updated: Nov 14th 2011
% This grammar assumes there is no DP in Chinese.

% Proper Nouns
['Pronoun']::['N',-case].

% Common Nouns
['Noun']::['N',-case].

['Noun']::['N'].	


% Null argument (pro/the relativized NP)
[]::['N',-case]. 

% One-place predicates (intransitive verbs and adjective predicates)
['Vi']::['V-Subjless'].	% had bad intensions

% Two-place predicates (transitive verbs)
% used in an RC, only have NP arguments
 ['Vt']::[='N',+case,'V-Subjless'].	% like
['Vt']::[='Poss',+case,'V-Subjless'].

[]::[=>'V-Subjless',='N','v']. %v
[]::[=>'V-Subjless',='N','vRel']. %v

[]::[=>'V-Subjless',='Poss','v']. %v
[]::[=>'V-Subjless',='Poss','vRel']. %v

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

% % genitive de
 [de]::[='N',='N','Poss',-case].

% The root.
startCategory('C').

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
['Vi']::['V'].	% had bad intensions

% Two-place predicates (transitive verbs)
% used in an RC, only have NP arguments
 ['Vt']::[='N',+case,'V'].	% like
['Vt']::[='Poss',+case,'V'].

[]::[=>'V',='N','v']. %v
[]::[=>'V',='N','vRel']. %v

[]::[=>'V',='Poss','v']. %v
[]::[=>'V',='Poss','vRel']. %v

[]::[='v',+case,'T']. %tense

[]::[='T','C'].



% Relative Clauses: an Adjunction Analysis
% SSR showParse([yaoqing,guanyuan,de,fuhao,dale,jizhe]).
% SOR showParse([guanyuan,yaoqing,de,fuhao,dale,jizhe]).
% OSR showParse([jizhe,dale,yaoqing,guanyuan,de,fuhao]).
% OOR showParse([jizhe,dale,guanyuan,yaoqing,de,fuhao]).

% Nothing special for T; -f is used to ensure RC preceeds de
 []::[='vRel',+case,'TRel',-f].

 [de]::[='TRel',+f,'CRel'].

% Relative CP can left-adjoin onto the NP head
 ['CRel']>>['N'].

% % genitive de
 [de]::[='N',='N','Poss',-case].

% The root.
startCategory('C').

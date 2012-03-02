%   File   : chinese.pl
%   Author : Zhong Chen
%   Updated: Nov 14th 2011
% This grammar assumes there is no DP in Chinese.

% Proper Nouns
['Zhangsan']::['N',-case].
['Lisi']::['N',-case].

% pronouns
[wo]::['N',-case].		% I/me
[women]::['N',-case].	% we/us
[ni]::['N',-case].		% you (sg.)
[nimen]::['N',-case].	% you (pl.)
[ta]::['N',-case].		% he/she
[tamen]::['N',-case].	% them
%['Pronoun']::['N',-case].

% Common Nouns
[fuhao]::['N',-case].		% tycoon
[guanyuan]::['N',-case].	% official
[jizhe]::['N',-case].	% reporter
[dangao]::['N',-case].	% cake
[pengyou]::['N',-case].	% friend
[shishi]::['N',-case].			%fact

%['Noun']::['N',-case].

[fuhao]::['N'].		% tycoon
[guanyuan]::['N'].	% official
[jizhe]::['N'].	% reporter
[dangao]::['N'].	% cake
[pengyou]::['N'].	% friend


%['Noun']::['N'].


% Null argument (pro/the relativized NP)
[]::['N-null',-case]. 

% One-place predicates (intransitive verbs and adjective predicates)
[xinhuaibugui]::['V'].	% had bad intensions
[wuyongzhiyi]::['V'].	% undoubted
[shuiluoshichu]::['V'].  % revealed: used with 'zhenxiang' truth
[chenggongle]::['V']. %succeed

%['Vi']::['V'].

% Two-place predicates (transitive verbs)
% used in an RC, only have NP arguments
 [xihuan]::[='N',+case,'V'].	% like
 [yaoqing]::[='N',+case,'V'].	% invite
 [dale]::[='N',+case,'V'].	% hit

%['Vt']::[='N',+case,'V'].


 [xihuan]::[='N-null',+case,'V'].	% like
 [yaoqing]::[='N-null',+case,'V'].	% invite
 [dale]::[='N-null',+case,'V'].	% hit

%['Vt']::[='N-null',+case,'V'].

 [xihuan]::[='Poss',+case,'V'].	% like
 [yaoqing]::[='Poss',+case,'V'].	% invite
 [dale]::[='Poss',+case,'V'].	% hit

%['Vt']::[='Poss',+case,'V'].


[]::[=>'V',='N','v']. %v
[]::[=>'V',='N','vRel']. %v

[]::[=>'V',='N-null','v']. %v
[]::[=>'V',='N-null','vRel']. %v

[]::[=>'V',='Poss','v']. %v
[]::[=>'V',='Poss','vRel']. %v


[]::[='v',+case,'T']. %tense


[]::[='T','C'].



% Relative Clauses: an Adjunction Analysis
% SSR showParse([yaoqing,guanyuan,de,fuhao,dale,jizhe]).
% SOR showParse([guanyuan,yaoqing,de,fuhao,dale,jizhe]).
% OSR showParse([jizhe,dale,yaoqing,guanyuan,de,fuhao]).
% OOR showParse([jizhe,dale,guanyuan,yaoqing,de,fuhao]).
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

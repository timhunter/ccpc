%   File   : chinese.pl
%   Author : Zhong Chen
%   Updated: May 5th 2010

%[]::[='V',mystart].


% Proper Nouns
['Zhangsan']::['D',-case].
['Lisi']::['D',-case].

% pronouns
[wo]::['D',-case].		% I/me
[women]::['D',-case].	% we/us
[ni]::['D',-case].		% you (sg.)
[nimen]::['D',-case].	% you (pl.)
[ta]::['D',-case].		% he/she
[tamen]::['D',-case].	% them

% Common Nouns
[fuhao]::['N'].		% tycoon
[guanyuan]::['N'].	% official
[jizhe]::['N'].	% reporter
[dangao]::['N'].	% cake
[pengyou]::['N'].	% friend

% Determiner (null)
[]::[='N','D',-case].

% Null argument (pro)
[]::['D',-case].

% One-place predicates (intransitive verbs and adjectives)
% Chinese adjectives syntactically behave like intransitive verbs (some of them may need a degree word i.e. hen or feichang).
% compared with korean, there is no morphological change here so we simple use v. 
[xinhuaibugui]::['V'].	% had bad intensions
[wuyongzhiyi]::['V'].	% undoubted
[shuiluoshichu]::['V'].  % revealed: used with 'zhenxiang' truth

% Two-place predicates (transitive verbs)
[xihuan]::[='D',+case,'V'].	% like
[yaoqing]::[='D',+case,'V'].	% invite
[dale]::[='D',+case,'V'].	% hit




[]::[=>'V',='D','v']. %v
[]::[=>'V',='D','vRel']. %v
[]::[=>'V',='D','vEmb'].


[]::[='v',+case,'T']. %tense
[]::[='vRel',+case,'TRel']. %tense
[]::[='vEmb',+case,'TEmb']. %tense

[]::[='T','C'].




% Relative Clauses


% Determiner (null) with -wh hoisting feature
 []::[='N','D',-case,-wh].

% TP is raised to Spec-DP due to EPP
 []::[='vRel',+case,'TRel',-epp].

% Wh-hoisting complementizer
 []::[='TRel',+wh,'CRel'].

% the LINKER de (Den Dikken & Singhapreecha (2004)) can take CP as its complement and 
% move the TP to the spec of FP to satisfy the EPP feature.
 [de]::[='CRel',+epp,'F'].
 []::[='F','D',-case].

% Complement clauses (fact-clause)

% Common nouns that need complements
% Assumption: Only dependent nouns can take CP-complement.
 [zhenxiang]::[='CEmb','NDep'].		% truth
 [shishi]::[='CEmb','NDep'].		% fact

% Special determiner that takes as argument an NP that takes CP-complement
% needs to check out Cheng and Sybesma (2010)
 [de]::[='NDep',+epp,'D',-case].

% TP is raised to Spec-DP due to EPP
 []::[='vEmb',+case,'TEmb',-epp].

% Complementizer (embedded clause)
 []::[='TEmb','CEmb'].


% alternate complement clause example: Zhangsan qiangjie de shishi wuyongzhiyi



% The root.
startCategory('C').

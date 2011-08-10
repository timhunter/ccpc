%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : korean.pl
%   Author : Jiwon Yun
%   Last Updated: August 9, 2011

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% With this grammar, you can parse the followings:
% - simple SV sentences
% - simple SOV sentences
% - pro-drop sentences (simple/adjunct/complement)
% - relative clauses (under both promotion and adjunction analyses)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Grammar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Korean (or every language) has DPs. 

% Proper Nouns
['Jiwon']::['D',-f].
['Seongyeon']::['D',-f].

% Common Nouns
[kica]::['N'].			% reporter
[uywon]::['N'].			% senator
[phyencipcang]::['N'].	% editor
[hyengsa]::['N'].		% detective

% Determiner (null)
[]::[='N','D',-f].

% Case particles are heads of CaseP (cf. Hoshi 2004)
% "morphological case is realized at the Case head position"
%
% DP moves to Spec-CaseP due to the word-order. 

[i]::[='D',+f,'Case',-nom].
[ka]::[='D',+f,'Case',-nom].
[ul]::[='D',+f,'Case',-acc].
[lul]::[='D',+f,'Case',-acc].

% Null argument (pro)
[]::['Case',-nom].
[]::['Case',-acc].

% One-place predicates (intransitive verbs and adjectives)
% In Korean, adjectives syntactically behave like intransitive verbs.
[hwanassta]::['V-Decl'].			% got angry
[yumyenghata]::['V-Decl'].			% famous
[yumyenghaycyessta]::['V-Decl'].	% became famous
[palkhyecyessta]::['V-Decl'].		% was revealed

% Two-place predicates (transitive verbs)
% , which assign accusative case
[cohahanta]::[='Case',+acc,'V-Decl'].	% like
[silhehanta]::[='Case',+acc,'V-Decl'].	% hate
[kongkyekhayssta]::[='Case',+acc,'V-Decl'].	% attacked
[hyeppakhayssta]::[='Case',+acc,'V-Decl'].	% threatened
[ttaylyessta]::[='Case',+acc,'V-Decl'].	% beat

% adnominal forms of predicates
% The same adnominal form is used for both relative and complement clauses.
[kongkyekhan]::[='Case',+acc,'V-Rel'].	% (the person that ...) attacked
[kongkyekhan]::[='Case',+acc,'V-Emb'].	% (the fact that ...) attacked 

% adjunctive forms of predicates
[kongkyekhayse]::[='Case',+acc,'V-Adj'].	% (because ...) attacked

% Little v
% The subject starts from vP-Spec
[]::[='V-Decl',='Case','v-Decl'].
[]::[='V-Rel',='Case','v-Rel'].
[]::[='V-Emb',='Case','v-Emb'].
[]::[='V-Adj',='Case','v-Adj'].

% Tense 
% , which assigns nominative case
[]::[='v-Decl',+nom,'T-Decl'].
[]::[='v-Emb',+nom,'T-Emb'].
[]::[='v-Adj',+nom,'T-Adj'].

% Complimentizer for declarative clauses
[]::[='T-Decl','C-Decl'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Relative Clauses
% showParse([uywon,ul,kongkyekhan,kica,ka,phyencipcang,ul,silhehanta]). 
% 'The reporter who attacked the senator hates the editor.'
% showParse([uywon,i,kongkyekhan,kica,ka,phyencipcang,ul,silhehanta]). 
% 'The reporter who the senator attacked hates the editor.'

% null wh Case - the raised CaseP does not have an overt case marker.
 []::[='D',+f,'Case',-nom,-wh].
 []::[='D',+f,'Case',-acc,-wh].

% TP is raised to Spec-DP due to EPP
 []::[='v-Rel',+nom,'T-Rel',-epp].

% Wh-hoisting complementizer
 []::[='T-Rel',+wh,'C-Rel'].

% DP can take CP as its complement and 
% move the TP to its spec to satisfy the EPP feature.
 []::[='C-Rel',+epp,'D',-f].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Complement clauses (fact-clause)
% showParse([kica,ka,uywon,ul,kongkyekhan,sasil,i,palkhyecyessta]). 
% 'The fact that the reporter attacked the senator was revealed.'

% Common nouns that need complements
% Assumption: Only dependent nouns can take CP-complement.
 [sasil]::[='C-Emb','N-Dep'].		% fact
 [kes]::[='C-Emb','N-Dep'].		% does not have any specific meaning...

% Special determiner that takes as argument an NP that takes CP-complement
 []::[='N-Dep',+epp,'D',-f].

% TP is raised to Spec-DP due to EPP
 []::[='v-Emb',+nom,'T-Emb',-epp].

% Complementizer (embedded clause)
 []::[='T-Emb','C-Emb'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adjunct clauses (because-clause)
% showParse([kica,ka,uywon,ul,kongkyekhayse,phyencipcang,i,hwanassta]). 
% 'The editor was angry because the reporter attacked the senator.'
% showParse([uywon,ul,kongkyekhayse,kica,ka,yumyenghaycyessta]). 
% 'The reporter became famous because he attacked the senator.'

% Complementizer (adjunct clause)
 []::[='T-Adj','C-Adj'].

% Adjunct CPs can left-adjoin another CP
 ['C-Adj']>>['C-Decl'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The Root
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 startCategory('C-Decl').

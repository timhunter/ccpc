%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : korean_adjunction.pl
%   Author : Jiwon Yun
%   Last Updated: July 22, 2012
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% With this grammar, you can parse the followings:
% - simple SV sentences
% - simple SOV sentences
% - pro-drop sentences (in matrix/adjunct/complement clauses)
% - relative clauses (under a adjunction analysis)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Arguments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% NP
% Common Nouns
[kica]::['N'].			% reporter
[uywon]::['N'].			% senator
[soselka]::['N'].		% novelist
[phyencipcang]::['N'].	% editor
[hyengsa]::['N'].		% detective
[kyengchal]::['N'].		% policeman
[cippaywon]::['N'].		% mailman
[yaksa]::['N'].			% pharmacist
[ceycakca]::['N'].		% producer
[kamtok]::['N'].		% film director

[sinmwun]::['N'].		% newspaper
[yenghwa]::['N'].		% movie
[sosel]::['N'].			% novel
[noymwul]::['N'].		% bribe
[cosa]::['N'].			% investigation

['N']::['N'].			% noun (abstracted)


% DP
% Let us assume that Korean (or every language) has DP. 

% Proper Nouns
[Jiwon]::['D',-f].
[Seongyeon]::['D',-f].
[Ken]::['D',-f].
[Hana]::['D',-f].
[Nayoung]::['D',-f].
[John]::['D',-f].

% Determiners
[]::[='N','D',-f].	% null
['D']::['D',-f].	% determiner (abstracted)


% CaseP
% Case particles are heads of CaseP:
% "morphological case is realized at the Case head position." (Hoshi 2004)
% DP moves to Spec-CaseP due to the word-order. 

% Case particles
[i]::[='D',+f,'Case',-nom].		% nominative case particle after a consonant
[ka]::[='D',+f,'Case',-nom].	% nominative case particle after a verb
[ul]::[='D',+f,'Case',-acc].	% accusative case particle after a consonant
[lul]::[='D',+f,'Case',-acc].	% accusative case particle after a verb

['nom']::[='D',+f,'Case',-nom].	% nominative (abstracted)
['acc']::[='D',+f,'Case',-acc].	% accusative (abstracted)

% pro
%[]::['Case',-nom].
%[]::['Case',-acc].
[]::['null-Case',-nom].
[]::['null-Case',-acc].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% declarative forms of predicates
% -ss-: past tense
% -ta: declarative suffix

% One-place predicates (intransitive verbs and adjectives)
% Adjectives are syntactically similar with intransitive verbs in Korean.
[yumyenghata]::['V-Decl'].			% be famous
[yumyenghaycyessta]::['V-Decl'].	% become famous
[palkhyecyessta]::['V-Decl'].		% be revealed
[hwanassta]::['V-Decl'].			% get angry
['Vid']::['V-Decl'].				% intransitive verb in declarative form (abstracted)

% Two-place predicates (transitive verbs)
% , which assign accusative case
[cohahanta]::[='Case',+acc,'V-Decl'].		% like
[silhehanta]::[='Case',+acc,'V-Decl'].		% hate
[kongkyekhayssta]::[='Case',+acc,'V-Decl'].	% attack
[hyeppakhayssta]::[='Case',+acc,'V-Decl'].	% threaten
[ttaylyessta]::[='Case',+acc,'V-Decl'].		% beat
['Vtd']::[='Case',+acc,'V-Decl'].			% transitive verb in declarative form (abstracted)
['Vtd']::[='null-Case',+acc,'V-Decl'].		% transitive verb in declarative form (abstracted)

% adnominal forms of predicates
% -n: adnominal suffix
% The same adnominal form is used for both relative and complement clauses.

% One-place predicates
[yumyenghan]::['V-Rel'].	% be famous
[yumyenghan]::['V-Comp'].	% be famous
[hwanan]::['V-Rel'].		% get angry
[hwanan]::['V-Comp'].		% get angry
['Vin']::['V-Rel'].			% intransitive verb in adnominal form in relative clauses (abstracted)
['Vin']::['V-Comp'].		% intransitive verb in adnominal form in complement clauses (abstracted)

% Two-place predicates
[kongkyekhan]::[='Case',+acc,'V-Rel'].	% attack
[kongkyekhan]::[='Case',+acc,'V-Comp'].	% attack
[hyeppakhan]::[='Case',+acc,'V-Rel'].	% threaten
[hyeppakhan]::[='Case',+acc,'V-Comp'].	% threaten
['Vtn']::[='Case',+acc,'V-Rel'].		% transitive verb in adnominal form in relative clauses (abstracted)
['Vtn']::[='Case',+acc,'V-Comp'].		% transitive verb in adnominal form in complement clauses (abstracted)
['Vtn']::[='null-Case',+acc,'V-Rel'].		% transitive verb in adnominal form in relative clauses (abstracted)
['Vtn']::[='null-Case',+acc,'V-Comp'].		% transitive verb in adnominal form in complement clauses (abstracted)


% adjunctive forms of predicates 
% -se: 'because' 

% One-place predicates
[yumyenghayse]::['V-Adj'].	% be famous
[hwanase]::['V-Adj'].		% get angry
['Via']::['V-Adj'].			% transitive verb in adjunctive form (abstracted)

% Two-place predicates
[kongkyekhayse]::[='Case',+acc,'V-Adj'].	% attack
[hyeppakhayse]::[='Case',+acc,'V-Adj'].		% threaten
['Vta']::[='Case',+acc,'V-Adj'].			% transitive verb in adjunctive form (abstracted)
['Vta']::[='null-Case',+acc,'V-Adj'].			% transitive verb in adjunctive form (abstracted)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functional Categories
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Little v
% The subject starts from vP-Spec
[]::[='V-Decl',='Case','v-Decl'].
[]::[='V-Rel',='Case','v-Rel'].
[]::[='V-Comp',='Case','v-Comp'].
[]::[='V-Adj',='Case','v-Adj'].

[]::[='V-Decl',='null-Case','v-Decl'].
[]::[='V-Rel',='null-Case','v-Rel'].
[]::[='V-Comp',='null-Case','v-Comp'].
[]::[='V-Adj',='null-Case','v-Adj'].

% Tense 
% , which assigns nominative case
[]::[='v-Decl',+nom,'T-Decl'].
[]::[='v-Comp',+nom,'T-Comp'].
[]::[='v-Adj',+nom,'T-Adj'].

% Complimentizer for declarative clauses
[]::[='T-Decl','C-Decl'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Relative Clauses
%
% e.g.
% showParse([uywon,ul,kongkyekhan,kica,ka,phyencipcang,ul,silhehanta]). 
% 'The reporter who attacked the senator hates the editor.'
% showParse([uywon,i,kongkyekhan,kica,ka,phyencipcang,ul,silhehanta]). 
% 'The reporter who the senator attacked hates the editor.'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2. adjunction analysis
% Null wh operator (Case/D)
 []::['null-Case',-nom,-wh].
 []::['null-Case',-acc,-wh].

% Nothing special for T
 []::[='v-Rel',+nom,'T-Rel'].

% Wh-hoisting complementizer
 []::[='T-Rel',+wh,'C-Rel'].

% Relative CP can left-adjoin onto the head noun
 ['C-Rel']>>['Case'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Complement clauses (fact-clause)
% 
% e.g.
% showParse([kica,ka,uywon,ul,kongkyekhan,sasil,i,palkhyecyessta]). 
% 'The fact that the reporter attacked the senator was revealed.'
% showParse([uywon,ul,kongkyekhan,sasil,i,palkhyecyessta]). 
% 'The fact that (someone) attacked the senator was revealed.'
% showParse([kica,ka,kongkyekhan,sasil,i,palkhyecyessta]). 
% 'The fact that the reporter attacked (someone) was revealed.'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Common nouns that need complements
% Only dependent nouns can take CP-complement.
['sasil']::[='C-Comp','N-Dep'].		% fact
[fact]::[='C-Comp','N-Dep'].		% dependent noun taking a complement (abstracted)

% Special determiner that takes as argument an NP that takes CP-complement
[]::[='N-Dep',+epp,'D',-f].

% TP is raised to Spec-DP due to EPP
[]::[='v-Comp',+nom,'T-Comp',-epp].

% Complementizer (embedded clause)
[]::[='T-Comp','C-Comp'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adjunct clauses (because-clause)
%
% e.g.
% showParse([kica,ka,uywon,ul,kongkyekhayse,phyencipcang,i,hwanassta]). 
% 'The editor was angry because the reporter attacked the senator.'
% showParse([uywon,ul,kongkyekhayse,kica,ka,yumyenghaycyessta]). 
% 'The reporter became famous because (he) attacked the senator.'
% showParse([kica,ka,kongkyekhayse,uywon,i,yumyenghaycyessta]). 
% 'The senator became famous because the reporter attacked (him).'
% pro in the adjunct clause tends to be interpreted as co-indexed with 
% an argment in the matrix clause.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Complementizer for adjunct clauses
[]::[='T-Adj','C-Adj'].

% Adjunct CPs can left-adjoin another CP
['C-Adj']>>['C-Decl'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The Root
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
startCategory('C-Decl').

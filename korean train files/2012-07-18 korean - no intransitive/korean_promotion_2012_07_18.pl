%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : korean.pl
%   Author : Jiwon Yun
%   Last Updated: August 23, 2011

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% With this grammar, you can parse the followings:
% - simple SV sentences
% - simple SOV sentences
% - pro-drop sentences (simple/adjunct/complement)
% - relative clauses (under promotion analysis)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Grammar
% : abstracted version of korean.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Korean (or every language) has DPs. 

% Common Nouns
['Noun']::['N'].

% Determiners
['Det']::['D',-f]. % proper nouns
[]::[='N','D',-f]. % null

% Case particles
['Nom']::[='D',+f,'Case',-nom].
['Acc']::[='D',+f,'Case',-acc].

% pro
[]::['Case',-nom].
[]::['Case',-acc].

% all the predicates are limited to transitives

% One-place declarative predicates (intransitive verbs and adjectives)
% In Korean, adjectives syntactically behave like intransitive verbs.
 ['Vi']::['V-Decl'].

% Two-place declarative predicates (transitive verbs)
% , which assign accusative case
 ['Vt']::[='Case',+acc,'V-Decl'].

% declarative form predicates (transitive verbs)
 ['VTDecl']::[='Case',+acc,'V-Decl'].

% adnominal form of predicates
% The same adnominal form is used for both relative and complement clauses.
['Vadn']::[='Case',+acc,'V-Rel'].	
['Vadn']::[='Case',+acc,'V-Emb'].	

% adjunctive form of predicates
['Vadj']::[='Case',+acc,'V-Adj'].	% (because ...) attacked

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
 ['fact']::[='C-Emb','N-Dep'].		% fact

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

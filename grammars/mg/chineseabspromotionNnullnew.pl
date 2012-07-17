%   File   : chineseabs_promotion.pl
%   Author : Zhong Chen
%   Updated: June 12nd 2012
%   altered by John to differentiate between overt Nouns and covert Nouns
%   altered by Chen to differentiate between RCs in subject position and RCs in object position


% Proper Nouns
 ['Pronoun']::['N',-case].

% Nouns
 ['Noun']::['N'].    % in possessives
 ['Noun']::['N',-case].
 ['Noun']::['N-Rel',-case,-wh].  % nouns that can be relativized

% Null argument (pro)
 []::['N-null'].            %JTH
 []::['N-null',-case].      %JTH
 []::['N-null-Rel',-case,-wh].  %JTH

% One-place predicates (intransitive verbs and adjectives)
 ['Vi']::['V'].

% Two-place predicates (transitive verbs)
 ['Vt']::[='N',+case,'V'].
 ['Vt']::[='N-null',+case,'V'].
 ['Vt']::[='Poss',+case,'V'].	% take possessive as arguments
 ['Vt']::[='N-SR',+case,'V']. % SR as matrix object
 ['Vt']::[='N-OR',+case,'V']. % OR as matrix object

% little v
 []::[=>'V',='N','v']. %v
 []::[=>'V',='N-null','v']. %v
 []::[=>'V',='Poss','v']. %v
 []::[=>'V',='N-SR','v']. % SR as matrix subject
 []::[=>'V',='N-OR','v']. % OR as matrix subject

% tense
 []::[='v',+case,'T']. %tense


 []::[='T','C'].


% Relative Clauses
% SR:   showParse([Vt,Noun,de,Noun,Vt,Noun]).
% OR:   showParse([Noun,Vt,de,Noun,Vt,Noun]).

%['Vt']::[='N-null',+case,'V-Rel'].  % obj-pro-drop is not allowed in RCs
 ['Vt']::[='N-null-Rel',+case,'V-OR'].
 ['Vt']::[='Poss',+case,'V-SR'].
 ['Vt']::[='N-Rel',+case,'V-OR'].
 ['Vt']::[='N',+case,'V-SR'].

 ['Vi']::['V-SR'].

 []::[=>'V-OR',='N','v-OR'].         %v
 []::[=>'V-OR',='N-null','v-OR'].    % subj-pro-drop is permitted in RCs
 []::[=>'V-OR',='Poss','v-OR'].
 []::[=>'V-SR',='N-null-Rel','v-SR'].  
 []::[=>'V-SR',='N-Rel','v-SR'].

 []::[='v-SR',+case,'T-SR',-f].	%tense
 []::[='v-OR',+case,'T-OR',-f].	%tense

% relativizer de hoist TRel to Spec CP
 [de]::[='T-SR',+f,'C-SR',-k].
 [de]::[='T-OR',+f,'C-OR',-k].

% raise argument NP to head position
 []::[='C-SR',+wh,'N-SR'].
 []::[='C-OR',+wh,'N-OR'].

% need to move CP to Spec NP
 []::[='N-SR',+k,'N-SR',-case].
 []::[='N-OR',+k,'N-OR',-case].


% possessive de
 [de]::[='N',='N','Poss',-case].
 [de]::[='N',='N-SR','Poss',-case].
 [de]::[='N',='N-OR','Poss',-case].

% The root.
startCategory('C').

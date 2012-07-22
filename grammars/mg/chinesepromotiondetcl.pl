%   File   : chineseabs_promotion.pl
%   Author : Zhong Chen
%   Updated: Mar 28th 2012
%   altered by John to differentiate between overt Nouns and covert Nouns
%   updated by Chen to cover conditions used in Chen, Jaeger, Li, Vasishth (2012)

% Pronouns
 ['Pronoun']::['D',-case].

% Null Determiners
 []::[='N','D',-case].
 []::[='N-null','D-null',-case].   % pro also has a null D
 []::[='N-SR','D-SR',-case]. % NP-SR could have a null D
 []::[='N-OR','D-OR',-case]. % NP-OR could have a null D

% Overt Determiners, such as "zhe", "na".
% It needs to work with a classifer in Chinese.
 ['Det']::[='CL','D',-case].
 ['Det']::[='CL-SR','D-SR',-case].
 ['Det']::[='CL-OR','D-OR',-case].
% ['Det']::[='CL-Rel','D-Rel'].  % D-Rel as possessor

% Classifiers
 ['Cl']::[='N','CL'].
 ['Cl']::[='N-SR','CL-SR'].
 ['Cl']::[='N-OR','CL-OR'].

% Nouns
 ['Noun']::['N'].	     % in possessives
 ['Noun']::['N',-case].
 ['Noun']::['N-Rel',-case,-wh].  % nouns that can be relativized

% Null argument (pro)
 []::['N-null'].            %JTH
 []::['N-null',-case].      %JTH
 []::['N-null-Rel',-case,-wh].  %JTH

% One-place predicates (intransitive verbs and adjectives)
 ['Vi']::['V'].	  	

% Two-place predicates (transitive verbs)
 ['Vt']::[='Poss',+case,'V'].	% take possessive as arguments
 ['Vt']::[='D',+case,'V']. % for Det
 ['Vt']::[='D-null',+case,'V'].
 ['Vt']::[='D-SR',+case,'V']. % SR as matrix object
 ['Vt']::[='D-OR',+case,'V']. % OR as matrix object



% Transitive verbs allow duration/freq phrases.
% Huang et al (2009) treats FP as the adjunct of V'.
% We implement it as complement of Vt and raise it (+/-m) for word order purposes.
 ['Vt']::[='F',+m,='Poss',+case,'V'].	% take possessive as arguments
 ['Vt']::[='F',+m,='D',+case,'V']. % for Det
 ['Vt']::[='F',+m,='D-SR',+case,'V']. % for Det
 ['Vt']::[='F',+m,='D-OR',+case,'V']. % for Det

 ['Freq']::['F',-m].

% little v
 []::[=>'V',='Poss','v'].
 []::[=>'V',='D','v']. 
 []::[=>'V',='D-null','v'].
 []::[=>'V',='D-SR','v']. % SR as matrix subject
 []::[=>'V',='D-OR','v']. % OR as matrix subject

% tense
 []::[='v',+case,'T']. %tense
 []::[='T','C'].

% Temporal Phrase as adjunct
 ['Time']::['Temp'].
 ['Temp']>>['T'].

% Relative Clauses
% SR:   showParse([Vt,Noun,de,Noun,Vt,Noun]).
% OR:   showParse([Noun,Vt,de,Noun,Vt,Noun]).
% SR:   showParse([Det,Cl,Time,Vt,Noun,Freq,de,Noun,Vt,Noun]).
% OR:   showParse([Det,Cl,Time,Noun,Vt,Freq,de,Noun,Vt,Noun]).


%['Vt']::[='N-null',+case,'V-Rel'].  % obj-pro-drop is not allowed in RCs
 ['Vt']::[='N-null-Rel',+case,'V-OR'].
 ['Vt']::[='Poss',+case,'V-SR'].
 ['Vt']::[='N-Rel',+case,'V-OR'].
 ['Vt']::[='N',+case,'V-SR'].


 ['Vt']::[='F',+m,='N',+case,'V-SR'].
 ['Vt']::[='F',+m,='N-null-Rel',+case,'V-OR'].
 ['Vt']::[='F',+m,='Poss',+case,'V-SR'].
 ['Vt']::[='F',+m,='N-Rel',+case,'V-OR'].

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


 ['Temp']>>['T-SR'].
 ['Temp']>>['T-OR'].

% raise argument NP to head position
 []::[='C-SR',+wh,'N-SR'].
 []::[='C-OR',+wh,'N-OR'].

% need to move CP to Spec NP
 []::[='N-SR',+k,'N-SR'].
 []::[='N-OR',+k,'N-OR'].

% possessive de
 [de]::[='N',='N','Poss',-case].
 [de]::[='N',='D-SR','Poss',-case].
 [de]::[='N',='D-OR','Poss',-case].

% The root.
startCategory('C').

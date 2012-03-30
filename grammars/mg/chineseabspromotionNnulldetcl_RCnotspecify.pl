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
 []::[='N-Rel','D',-case]. % NP-Rel could have a null D

% Overt Determiners, such as "zhe", "na".
% It needs to work with a classifer in Chinese.
 ['Det']::[='CL','D',-case].
 ['Det']::[='CL-Rel','D-Rel',-case].
 ['Det']::[='CL-Rel','D-Rel'].  % D-Rel as possessor

% Classifiers
 ['Cl']::[='N','CL'].
 ['Cl']::[='N-Rel','CL-Rel'].

% Nouns
 ['Noun']::['N'].	     % in possessives
 ['Noun']::['N',-case].
 ['Noun']::['N',-case,-wh].  % nouns that can be relativized

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
 ['Vt']::[='D-Rel',+case,'V']. % RC as matrix object

% Transitive verbs allow duration/freq phrases.
% Huang et al (2009) treats FP as the adjunct of V'.
% We implement it as complement of Vt and raise it (+/-m) for word order purposes.
 ['Vt']::[='F',+m,='Poss',+case,'V'].	% take possessive as arguments
 ['Vt']::[='F',+m,='D',+case,'V']. % for Det
 ['Vt']::[='F',+m,='D-Rel',+case,'V']. % for Det

 ['Freq']::['F',-m].

% little v
 []::[=>'V',='Poss','v'].
 []::[=>'V',='D','v']. 
 []::[=>'V',='D-null','v'].
 []::[=>'V',='D-Rel','v']. % RC as matrix subject

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


 ['Vt']::[='N',+case,'V-Rel'].
%['Vt']::[='N-null',+case,'V-Rel'].  % obj-pro-drop is not allowed in RCs
 ['Vt']::[='N-null-Rel',+case,'V-Rel'].
 ['Vt']::[='Poss',+case,'V-Rel'].

 ['Vt']::[='F',+m,='N',+case,'V-Rel'].
 ['Vt']::[='F',+m,='N-null-Rel',+case,'V-Rel'].
 ['Vt']::[='F',+m,='Poss',+case,'V-Rel'].

 ['Vi']::['V-Rel'].


 []::[=>'V-Rel',='N','vRel'].         %v
 []::[=>'V-Rel',='N-null','vRel'].    % subj-pro-drop is permitted in RCs
 []::[=>'V-Rel',='N-null-Rel','vRel'].  
 []::[=>'V-Rel',='Poss','vRel'].

 []::[='vRel',+case,'TRel',-f].	%tense

% relativizer de hoist TRel to Spec CP
 [de]::[='TRel',+f,'CRel',-k].

 ['Temp']>>['TRel'].

% raise argument NP to head position
 []::[='CRel',+wh,'N'].

% need to move CP to Spec NP
 []::[='N',+k,'N-Rel'].

% possessive de
 [de]::[='N',='N','Poss',-case].
 [de]::[='N',='D-Rel','Poss',-case].

% The root.
startCategory('C').

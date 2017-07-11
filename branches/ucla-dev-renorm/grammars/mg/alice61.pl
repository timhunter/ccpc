% file: larsonian1.pl
% author: John Hale
% created: Thu Jul 18 11:46:02 EDT 2002
% updated: Sun Jul 21 15:40:05 EDT 2002
% updated 31 Jul 02:  small modifications for a couple of examples -- EPS
%
%There are no mice in the air, I'm afraid.
%Test:  showParse([there,are,no,mice,in,the,air]).
%showParse(['I','-m',afraid,there,are,no,mice,in,the,air]).
%showParse([there,are,no,mice,in,the,air,'I','-m',afraid]).

[]::[='T',+f,'C'].                  % empty complementizer with focus feature
[]::[='T','Ce',-f].           % embedding complementizer with focus feature


%determiners
[no]::[='Num','D',-case].     % ordinary determiner
[the]::[='Num','D',-case].     % ordinary determiner

% number
[]::[='N','Num'].              ['-s']::['N'==>,'Num'].

% pronouns
['I']::['D',-case].
[there]::['D',-case].

% common nouns
[mice]::['N'].
[air]::['N'].

% preposition is a case assigner (Haegeman p193)
[in]::[='D',+case,'Pin'].

% little p
[]::[=>'Pin',='D',p_in].

% CP-taking adjectives
[afraid]::[='Ce','A']. 

%predicate be
[are]::[=p_in,+case,'Raise'].
['-m']::[=a,'Raise'].

%existential head
[]::[=>'Raise',='D','Ex'].

% tense
[]::[=>'Ex',+case,'T'].
[]::[=>'Raise',+case,'T'].

% little a
[]::[='A',='D',a].

startCategory('C').


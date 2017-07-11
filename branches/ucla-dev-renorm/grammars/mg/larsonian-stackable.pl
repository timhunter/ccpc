% file: larsonian.pl
% author: John Hale
% created: Thu Jul 18 11:46:02 EDT 2002
% updated: Sun Jul 21 15:40:05 EDT 2002
% updated: Thu Aug 22 12:05:52 EDT 2002
%
% make preposition and tense case assigners, require the dp's move to "check"
% case as was done in Stabler's g6.pl
%
% treat dative alternation as in Baker's UTAH chapter in
% Haegeman 1997 "Elements of Grammar" i.e. in dative,
% preposition is empty and the dative argument moves to get case.
%
% note that Ura's proposal is backwards from Larson's 88 account
% while Ura assumes the IO must move to get case, Larson construes
% 'to as "Case-marking"' (section 5.1). adopt Larsonian arrangment.
%
% retain, as always, the Kaynian treatment of RCs

[]::[=t,c].                  % regular empty complementizer
[]::[=t,+wh_rel,c_rel].      % wh-hoisting complementizer
%%% added 5/13/04 to try to get stacked relatives JTH
[]::[=t,+wh_rel,c_rel,-f].      % so CP can be promoted too...
[that]::[=t,'Ce'].           % embedding complementizer

[the]::[=c_rel,d,-case].     % relative determiner
[the]::[='Num',d,-case].     % ordinary determiner
[a]::[='Num',d,-case].
[my]::[='Num',d,-case].      % genitive determiner
[one]::[='Num',d,-case].     % numerical determiner

% number
[]::[=n,'Num'].              ['-s']::[n==>,'Num'].

% proper noun
['John']::[d,-case]. ['Chris']::[d,-case].
['Dick']::[d,-case]. ['David']::[d,-case]. ['Penny']::[d,-case].
['Sally']::[d,-case]. ['Paul']::[d,-case]. ['Stephen']::[d,-case].
['Mary']::[d,-case]. ['Pat']::[d,-case]. ['Sue']::[d,-case].
['Joe']::[d,-case]. ['Patrick']::[d,-case]. ['Jim']::[d,-case].
['Jenny']::[d,-case]. ['Clare']::[d,-case]. ['Ann']::[d,-case].

% indefinites, nominalization
[eggs] :: [d,-case]. [apples] :: [d,-case]. [lies] :: [d,-case].
[reading] :: [d,-case].

% pronouns
[he]::[d,-case]. [they]::[d,-case]. ['I']::[d,-case]. [it]::[d,-case].
[us]::[d,-case].

% possessive
% should be the complement of an external determiner
% in order to be pied-piped during wh-promotion
['s']::[='Num',='Num',n].

[which]::[='Num',+f,d,-case,-wh_rel].     % `promoting' wh-words
  [who]::[='Num',+f,d,-case,-wh_rel].     % semantic selection features (?)
%% added to get stacked relatives JTH 5/13/04
  [who]::[=c_rel,+f,d,-case,-wh_rel].

% common nouns
[story]::[n].    [story]::[n,-f].         % w and w/o promotion feature
[boy]::[n].      [boy]::[n,-f].
[girl]::[n].     [girl]::[n,-f].
[man]::[n].      [man]::[n,-f].
[woman]::[n].    [woman]::[n,-f].
[dog]::[n].      [dog]::[n,-f].
[cat]::[n].      [cat]::[n,-f].
[book]::[n].     [book]::[n,-f].
[house]::[n].    [house]::[n,-f].
[town]::[n].     [town]::[n,-f].
[trick]::[n].    [trick]::[n,-f].
[treat]::[n].    [treat]::[n,-f].
[lie]::[n].      [lie]::[n,-f].
[sweet]::[n].    [sweet]::[n,-f].
[present]::[n].  [present]::[n,-f].
[ticket]::[n].   [ticket]::[n,-f].
[letter]::[n].   [letter]::[n,-f].
[picture]::[n].  [picture]::[n,-f].
[answer]::[n].   [answer]::[n,-f].
[accident]::[n]. [accident]::[n,-f].
[box]::[n].      [box]::[n,-f].
[apple]::[n].    [apple]::[n,-f].
[food]::[n].     [food]::[n,-f].
[cake]::[n].     [cake]::[n,-f].
[ship]::[n].     [ship]::[n,-f].
[car]::[n].      [car]::[n,-f].
[sailor]::[n].   [sailor]::[n,-f].
[uncle]::[n].    [uncle]::[n,-f].
[mother]::[n].   [mother]::[n,-f].
[father]::[n].   [father]::[n,-f].
[brother]::[n].  [brother]::[n,-f].
[friend]::[n].   [friend]::[n,-f].
[bill]::[n].     [bill]::[n,-f].
[leg]::[n].      [leg]::[n,-f].
[clothe]::[n].   [clothe]::[n,-f].

% ....with complements
[fact]::[='Ce','n'].

% preposition is a case assigner (Haegeman p193)
[to]::[=d,+case,'Pto']. [on]::[=d,+case,'Pon']. [for]::[=d,+case,'Pfor'].
[with]::[=d,+case,'Pwith']. [in]::[=d,+case,'Pin'].
[]::[=d,+case,'Pto',-dat].	% P is empty in dative

% little p
[]::[=>'Pto',p_to]. []::[=>'Pin',p_in].  []::[=>'Pwith',p_with].
[]::[=>'Pon',p_on]. []::[=>'Pfor',p_for].

% ditransitive verbs - dative alternating
[tell]::[=p_to,=d,+case,+dat,v]. [give]::[=p_to,=d,+case,+dat,v].
[show]::[=p_to,=d,+case,+dat,v]. [explain]::[=p_to,=d,+case,+dat,v].
[teach]::[=p_to,=d,+case,+dat,v]. [sell]::[=p_to,=d,+case,+dat,v].

% ditransitive verbs - not
[tell]::[=p_to,=d,+case,v]. [give]::[=p_to,=d,+case,v]. [show]::[=p_to,=d,+case,v].
[explain]::[=p_to,=d,+case,v]. [teach]::[=p_to,=d,+case,v]. [sell]::[=p_to,=d,+case,v].

% intransitive verbs
[matter]::[v]. [wait]::[v]. [rule]::[v]. % yeah...yeah...like Slayer!

% transitive verbs
[tell]::[=d,+case,v]. [love]::[=d,+case,v]. [hate]::[=d,+case,v]. [write]::[=d,+case,v].
[sell]::[=d,+case,v]. [get]::[=d,+case,v]. [leave]::[=d,+case,v]. [like]::[=d,+case,v].
[buy]::[=d,+case,v]. [bring]::[=d,+case,v]. [take]::[=d,+case,v]. [send]::[=d,+case,v].
[have]::[=d,+case,v]. [surprise]::[=d,+case,v].

[pay]::[=p_for,v].		% pay...for services
[pay]::[=d,+case,v].		% pay...the piper

[come]::[='A',v].		% came late

% CP-taking verbs
[know]::[='Ce',v]. [forget]::[='Ce',v]. [remember]::[='Ce',v].

% auxilliary verbs
[will]::[='Have','Modal'].   [will]::[='Be','Modal'].    [will]::[=little_v,'Modal'].
[have]::[='Been','Have'].    [have]::[=ven,'Have'].
[be]::[=ving,'Be'].          [been]::[=ving,'Been'].

% little v gets the subject
[]::[=>v,=d,little_v].  % it's all optional 
['-en']::[=>v,=d,ven].  % PERFECT en is the symbol for past participle
      % Lester, p64:  next most common past participle ending is
      % vowel change along, e.g. begin-began-begun, sink-sank-sunk etc
      % However, for many irregular verbs and for all regular verbs the form of the
      % past participle is identical with the form of the simple past
      % e.g. tell-told-told, leave-left-left play-played-played, talk-talked-talked
      % so the string
      %            have -en talk
      % yields
      %            have talk -en
      % by morphophological rules
      %            have talked
      % likewise, 'have -en swim' derives 'have swum'
['-ing']::[=>v,=d,ving].% PROGRESSIVE ing is the symbol for present participle

% tense
[]::[=>little_v,+case,t]. ['-s']::[=>little_v,+case,t]. ['-ed']::[=>little_v,+case,t].
[]::[=>'Modal',+case,t].  ['-s']::[=>'Modal',+case,t].  ['-ed']::[=>'Modal',+case,t].
[]::[=>'Have',+case,t].   ['-s']::[=>'Have',+case,t].   ['-ed']::[=>'Have',+case,t].
[]::[=>'Be',+case,t].     ['-s']::[=>'Be',+case,t].     ['-ed']::[=>'Be',+case,t].

% ignore do-support and negation for now
['doesnt']::[=little_v,+case,t].

% predicative/copular be
[be]::[=a,'Be'].

% little a
[]::[='A',=d,a].
[]::[=d,+case,=d,a].   % [ADJ the bomb] [ADJ a dear]

% adjectives
[young]::['A']. [poor]::['A']. [clever]::['A']. [gentle]::['A'].
[kind]::['A']. [proud]::['A']. [lost]::['A']. [cheap]::['A'].
[interesting]::['A']. [sad]::['A']. [late]::['A']. [ill]::['A'].
[important]::['A']. [angry]::['A']. [pretty]::['A']. [honest]::['A'].
[right]::['A']. [strange]::['A']. [old]::['A']. [long]::['A'].

% optional intensifiers
[so]::[deg]. [very]::[deg]. [always]::[deg]. [too]::[deg].

% which can left-adjoin to adjectives
[deg]>>['A'].

% adjectives can also left-adjoin onto nouns
['A']>>[n].

% temporal modifiers
[yesterday]::[tmp]. [today]::[tmp].

% which right adjoin to verbs
[v]<<[tmp].

% oblique modifiers can right adjoin to VPs
[v]<<[p_in]. [v]<<[p_with]. [v]<<[p_on]. [v]<<[p_for].

startCategory(c).


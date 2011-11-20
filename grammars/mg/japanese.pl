% basic japanese grammar
% put together by John Hale
% trying to re-use as much as possible from
% Jiwon Yun's korean.pl
% and guided largely by section 5.9 of Natsuko Tsujimura's _Introduction to Japanese Linguistics_
%
% design ideas
%   1. syntactic arguments are uniformly CasePs (Hoshi)
%     the absence of casemarking is analyzed as empty structure
%   2. head-raising analysis of relative clauses (Schacter, Brame, Kayne)
%     the headnoun is based generated as an arg of the embedded V. relativization
%     is Wh-movement of this head to a position at the edge
%   3. no-wa clefts are a type of relativization. "no" is ``formal pronoun'' of category D
%  
% auxiliary assumptions
%  a. the copula doesn't assign structural case = doesn't select a CaseP
%  b. classifiers turn nouns into more complex nouns
%  c. accusative arguments can short-scramble past adjuncts
%
%  d. the particle "to" is a complementizer (p172 of Tsujimura)
%  e. "to" can scramble past a subject, or an object to the left edge of TP

% Proper Nouns
['Taroo']::['D',-f].           % m
['Ziroo']::['D',-f].           % m
['Satiko']::['D',-f].          % f
['Masako']::['D',-f].          % f
['Hanako']::['D',-f].          % f
['Ken']::['D',-f].
['Mari']::['D',-f].

% like a pronoun
[no]::['D',-f].

% Common Nouns
[doroboo]::['N'].			% thief
[gakusei]::['N'].			% student
[komodo]::['N'].			% child
[hahaoya]::['N'].			% mother
[sobo]::['N'].				% grandmother
[shinseki]::['N'].			% relative
[susi]::['N'].				% sushi
[hon]::['N'].				% book
[yubiwa]::['N'].			% wedding-ring
[hanbaagaa]::['N'].			% hamburger
[ronbun]::['N'].			% article (as in "academic paper")
[uma]::['N'].			% horse
[roba]::['N'].			% mule
[neko]::['N'].			% cat
[inu]::['N'].			% dog
[inaka]::['N']. 		% village
[jiko]::['N'].			% accident
['N']::['N']. % abstracted

% Determiner (null)
[]::[='N','D',-f].

% Case particles are heads of CaseP (cf. Hoshi 2004)
% "morphological case is realized at the Case head position"
%
% DP moves to Spec-CaseP due to the word-order. 
[ga]::[='D',+f,'Case',-nom].

[o]::[='D',+f,'Case',-acc,-scramble].  % "short" scrambling
[o]::[='D',+f,'Case',-acc].

[ni]::[='D',+f,'Case',-dat].

[de]::[='D',+f,'Loc']. [de]::[='D',+f,'Manner'].

%[to]::[='C-Decl',+f,'Case',-acc].

%%[to]::[='C-Decl',+f,'Case',-acc,-scramble].  %ADDED
% ignore other particles for now

% classifiers as noun-transformers
[san]::['Num',-f].	       % three
[satu]::[='Num',+f,='N','N'].  % classifier for books

% predicate nominal
[da]::[='D',+f,'V-Decl'].  % copula prefers uncasemarked arg
%% [da]::[='Case',+acc,'V-Decl'].  % maybe the copula could, in some dialects

% Null argument (pro)
[]::['Case',-nom].
[]::['Case',-acc].

% these are all in the past tense, I think
% one-place predicate
[kita]::['V-Decl'].                     % come
[asonda]::['V-Decl'].                   % play
[kusatta]::['V-Decl'].                  % spoil (as in food)
[sinda]::['V-Decl'].                    % die
[nakunatta]::['V-Decl'].                % another way to die
['Vi']::['V-Decl'].

% two-place, which assign accusative case
[katta]::[='Case',+acc,'V-Decl'].	% buy
[tabeta]::[='Case',+acc,'V-Decl'].	% eat
[yonda]::[='Case',+acc,'V-Decl'].	% read
[kaita]::[='Case',+acc,'V-Decl'].	% write
[tukutta]::[='Case',+acc,'V-Decl'].	% make (as in prepare food)
[ketta]::[='Case',+acc,'V-Decl'].	% kick
[oikakeru]::[='Case',+acc,'V-Decl'].	% chase
[kaihoushita]::[='Case',+acc,'V-Decl'].	% nurse (needed for Kahraman examples)
[itta]::[='Case',+acc,'V-Decl'].	          % say (needed for Kahraman examples)
[mita]::[='Case',+acc,'V-Decl'].	% see
['Vt']::[='Case',+acc,'V-Decl'].

% three-place. dative arg is naturally preverbal since nonfirst args go in specifiers stacked to the left
[ageta]::[='Case',+acc,='Case',+dat,'V-Decl']. % eat


% "modern Japanese lacks the affixal complementizers characteristic of adnominal clauses in Korean" (Kaplan & Whitman 95)
[tukutta]::[='Case',+acc,'V-Rel'].	% make (as in prepare food)
[kaita]::[='Case',+acc,'V-Rel'].	% write
[ketta]::[='Case',+acc,'V-Rel'].	% kick
[katta]::[='Case',+acc,'V-Rel'].	% buy (needed for Cho/Whitman/Yanagida examples)
[kaihoushita]::[='Case',+acc,'V-Rel'].	% nurse (needed for Kahraman examples)
[oikakeru]::[='Case',+acc,'V-Rel'].	% chase (could also have been relativized)
['Vt']::[='Case',+acc,'V-Rel'].

['Vt']::[='Case',+acc,'V-Emb'].

% Little v
% The subject starts from vP-Spec
[]::[='V-Decl',='Case','v-Decl'].
%[]::[='V-Decl',+scramble,='Case','v-Decl'].   % optional short-scrambling
[]::[='V-Rel',='Case','v-Rel'].
%[]::[='V-Rel',+scramble,='Case','v-Rel'].     % optional short-scrambling
[]::[='V-Emb',='Case','v-Emb'].
% []::[='V-Adj',='Case','v-Adj'].

% Tense 
% , which assigns nominative case
[]::[='v-Decl',+nom,'T-Decl'].
[]::[='v-Decl',+nom,+scramble,'T-Decl'].
[]::[='v-Emb',+nom,'T-Emb',-f].
% []::[='v-Adj',+nom,'T-Adj'].

% Complementizer for declarative clauses
[]::[='T-Decl','C-Decl'].

% the other `no' JBW says is used with the equivalent of gerunds and event nominalizations
[no]::[='T-Emb',+f,'C-Emb',-f].

% Determiner (null)
[]::[='C-Emb',+f,'D',-f].

% complementizer can host topic
[wa]::[='T-Decl',+topic,'C-Decl'].
%[wa]::[='T-Decl',+topic,'C-Decl',-f]. % re-orderable if complement of "to"


% null wh Case - the raised CaseP does not have an overt case marker.
  []::[='D',+f,'Case',-nom,-wh].
  []::[='D',+f,'Case',-acc,-wh].

% also null topic case. only for nominatives
 []::[='Dcleft',+f,'Case',-nom,-topic].    % record the fact that the topic is a cleft vs C-Emb
   % this enforces the contraint that wa can only take clefts.

% TP is raised to Spec-DP due to EPP
 []::[='v-Rel',+nom,'T-Rel',-epp].

% Wh-hoisting complementizer
 []::[='T-Rel',+wh,'C-Rel'].

% DP can take CP as its complement and 
% move the TP to its spec to satisfy the EPP feature.
% Hoshi says this on page 14
 []::[='C-Rel',+epp,'D',-f].
 []::[='C-Rel',+epp,'Dcleft',-f].         % record the fact that the topic is a cleft vs C-Emb

% adjectives
[omosiroi]::[adj].   % interesting
[oisii]::[adj].      % delicious
[tooi]::[adj].       % distant (kinship)


% adjectives can left adjoin
[adj]>>['N'].

% adverbs can left adjoin to CP
[kyonen]::[adv].  % yesterday
[adv]>>['C-Decl'].


% locative modifiers can left-adjoin to VP
['Loc']>>['V-Rel']. ['Loc']>>['V-Decl'].

% same for manner
['Manner']>>['V-Rel']. ['Manner']>>['V-Decl'].

startCategory('C-Decl').
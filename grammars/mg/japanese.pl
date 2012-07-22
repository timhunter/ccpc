% adjunction analysis of RCs
%
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
%  
% auxiliary assumptions
%  a. the copula doesn't assign structural case = doesn't select a CaseP
%  b. classifiers turn nouns into more complex nouns
%  c. accusative arguments can short-scramble past adjuncts
%
%  d. the particle "to" is a complementizer (p172 of Tsujimura)
%  e. "to" can scramble past a subject, or an object to the left edge of TP

% modified by Jiwon Yun
% adjunction analysis of RCs is added for testing
 
% Proper Nouns
['Taroo']::['D',-f].			% m
['Ziroo']::['D',-f].			% m
['Satiko']::['D',-f].			% f
['Masako']::['D',-f].			% f
['Hanako']::['D',-f].			% f
['Ken']::['D',-f].				% m
['Mari']::['D',-f].				% f

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
[yubiwa]::['N'].			% ring
[hanbaagaa]::['N'].			% hamburger
[ronbun]::['N'].			% article (as in "academic paper")
[uma]::['N'].			% horse
[roba]::['N'].			% mule
[neko]::['N'].			% cat
[inu]::['N'].			% dog
[inaka]::['N']. 		% village
[jiko]::['N'].			% accident
['N']::['N'].			% (abstracted)

% Determiner (null)
[]::[='N','D',-f].

% Case particles are heads of CaseP (cf. Hoshi 2004)
% "morphological case is realized at the Case head position"
%
% DP moves to Spec-CaseP due to the word-order. 
[ga]::[='D',+f,'Case',-nom].

%[o]::[='D',+f,'Case',-acc,-scramble].  % "short" scrambling
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
%[]::['Case',-nom].
%[]::['Case',-acc].
[]::['null-Case',-nom].
[]::['null-Case',-acc].

% verbs are all in the past tense

% one-place predicate
[kita]::['V-Decl'].                     % come
[asonda]::['V-Decl'].                   % play
[kusatta]::['V-Decl'].                  % spoil (as in food)
[sinda]::['V-Decl'].                    % die
[nakunatta]::['V-Decl'].                % pass away
['Vi']::['V-Decl'].						% (abstracted)

% two-place, which assign accusative case
[katta]::[='Case',+acc,'V-Decl'].	% buy
[tabeta]::[='Case',+acc,'V-Decl'].	% eat
[yonda]::[='Case',+acc,'V-Decl'].	% read
[kaita]::[='Case',+acc,'V-Decl'].	% write
[tukutta]::[='Case',+acc,'V-Decl'].	% make (as in prepare food)
[ketta]::[='Case',+acc,'V-Decl'].	% kick
[oikaketa]::[='Case',+acc,'V-Decl'].	% chase
[kaihoushita]::[='Case',+acc,'V-Decl'].	% nurse (needed for Kahraman examples)
[itta]::[='Case',+acc,'V-Decl'].		% say (needed for Kahraman examples)
[mita]::[='Case',+acc,'V-Decl'].		% see
['Vt']::[='Case',+acc,'V-Decl'].		% (abstracted)
['Vt']::[='null-Case',+acc,'V-Decl'].		% (abstracted)

% three-place. dative arg is naturally preverbal since nonfirst args go in specifiers stacked to the left
[ageta]::[='Case',+acc,='Case',+dat,'V-Decl']. % give


% "modern Japanese lacks the affixal complementizers characteristic of adnominal clauses in Korean" (Kaplan & Whitman 95)
% ...except for adjectival nouns (keiyoo-doosi), which is ignored in the current grammar (Jiwon)
[tukutta]::[='Case',+acc,'V-Rel'].	% make (as in prepare food)
[kaita]::[='Case',+acc,'V-Rel'].	% write
[ketta]::[='Case',+acc,'V-Rel'].	% kick
[katta]::[='Case',+acc,'V-Rel'].	% buy (needed for Cho/Whitman/Yanagida examples)
[kaihoushita]::[='Case',+acc,'V-Rel'].	% nurse (needed for Kahraman examples)
[oikaketa]::[='Case',+acc,'V-Rel'].	% chase (could also have been relativized)
['Vi']::['V-Rel'].		% (abstracted)
['Vi']::['V-Comp'].		% (abstracted)
['Vt']::[='Case',+acc,'V-Rel'].		% (abstracted)
['Vt']::[='Case',+acc,'V-Comp'].	% (abstracted)
['Vt']::[='null-Case',+acc,'V-Rel'].	% (abstracted)
['Vt']::[='null-Case',+acc,'V-Comp'].	% (abstracted)

% Little v
% The subject starts from vP-Spec
[]::[='V-Decl',='Case','v-Decl'].
%[]::[='V-Decl',+scramble,='Case','v-Decl'].   % optional short-scrambling
[]::[='V-Rel',='Case','v-Rel'].
%[]::[='V-Rel',+scramble,='Case','v-Rel'].     % optional short-scrambling
[]::[='V-Comp',='Case','v-Comp'].
% []::[='V-Adj',='Case','v-Adj'].

[]::[='V-Decl',='null-Case','v-Decl'].
[]::[='V-Rel',='null-Case','v-Rel'].
[]::[='V-Comp',='null-Case','v-Comp'].

% Tense 
% , which assigns nominative case
[]::[='v-Decl',+nom,'T-Decl'].
%[]::[='v-Decl',+nom,+scramble,'T-Decl'].
[]::[='v-Comp',+nom,'T-Comp',-f].
% []::[='v-Adj',+nom,'T-Adj'].

% Complementizer for declarative clauses
[]::[='T-Decl','C-Decl'].

% the other `no' JBW says is used with the equivalent of gerunds and event nominalizations
[no]::[='T-Comp',+f,'C-Comp',-f].

% Determiner (null)
[]::[='C-Comp',+f,'D',-f].

%% RC - promotion analysis

% null wh Case - the raised CaseP does not have an overt case marker.
%  []::[='D',+f,'null-Case',-nom,-wh].
%  []::[='D',+f,'null-Case',-acc,-wh].

% TP is raised to Spec-DP due to EPP
% []::[='v-Rel',+nom,'T-Rel',-epp].

% Wh-hoisting complementizer
% []::[='T-Rel',+wh,'C-Rel'].

% DP can take CP as its complement and 
% move the TP to its spec to satisfy the EPP feature.
% Hoshi says this on page 14
% []::[='C-Rel',+epp,'D',-f].

%% RC - adjunction analysis

% Null wh operator (Case/D)
 []::['null-Case',-nom,-wh].
 []::['null-Case',-acc,-wh].

% Nothing special for T
 []::[='v-Rel',+nom,'T-Rel'].

% Wh-hoisting complementizer
 []::[='T-Rel',+wh,'C-Rel'].

% Relative CP can left-adjoin onto the head noun
 ['C-Rel']>>['Case'].

%%

% adjectives
[omosiroi]::[adj].   % interesting
[oisii]::[adj].      % delicious
[tooi]::[adj].       % distant


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
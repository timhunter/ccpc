% simple.pl
% author: John Hale
% date: Fri Sep  9 18:08:23 EDT 2011
% purpose: create a situation where it is uncertain whether the initial noun is in the specifier or complement

% very abstracted version of Jiwon Yun's korean.pl
% generates sentences like
%    noun acc noun nom verb
%    noun nom verb
%    noun accverb


% the +hoist feature just makes sure the noun comes before the case ending
[acc]::[=n,+hoist,accusative,-orderacc].  [nom]::[=n,+hoist,nominative].

[noun]::[n,-hoist].


% either may be omitted
[]::[accusative,-orderacc]. []::[nominative].


% the +orderacc feature makes sure the verb's accusative complement precedes it
[verb]::[=accusative, +orderacc, v].

[]::[=v,=nominative,t].

startCategory(t).
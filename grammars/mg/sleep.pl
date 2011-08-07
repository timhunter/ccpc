[mary]::[n,-f].

[john]::[n,-f].


[sleeps]::[=n,+f,v].

% recursive postmodification
[tuesday]::[date].

[on]::[=date,p_on].

% [from]::[=place,p_from].

% [ohio]::[place].


[v]<<[p_on].
%[n]<<[p_from].

startCategory(v).
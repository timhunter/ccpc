%   File   : g-nta.pl - naive Tamil SOVI
%   Author : E Stabler
%   Updated: Mar 00

['-s'] :: [=v,+i,+k,'T'].
[] :: [='V',='D',v,-i].
[praise] :: [='D',+k,'V'].  [criticize] :: [='D',+k,'V'].
[laugh] :: ['V'].           [cry] :: ['V'].
['Beatrice'] :: ['D',-k].   ['Benedick'] :: ['D',-k].

%['-s'] :: [=v,+i,'T'].
%[] :: [='V',='D',='D',v,-i].
%[praise] :: ['V'].
%['Beatrice'] :: ['D'].   ['Benedick'] :: ['D'].

%['-s'] :: [=v,+i,'T'].
%[] :: [='V',='D',='D',v,-i].
%[praise] :: ['V'].         
%['Beatrice'] :: ['D'].   ['Benedick'] :: ['D'].

startCategory('T').

%[praise]::[='D','V',-i].    [criticize]::[='D','V',-i].
%['Beatrice'] :: ['D',-case].   ['Benedick'] :: ['D',-case].
%['-s']::[=v,+i,+case,'T'].   []::[='V',+case,='D',v].
[and]::[='T',='T','T'].

% only above items needed for "Beatrice criticize -s Benedick"

startCategory('T').


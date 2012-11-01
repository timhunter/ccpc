%   File   : g-nza.pl - naive zapotec VISO
%   Author : E Stabler
%   Updated: Mar 00

%['-s'] :: [=pred,+i,'T'].
%[] :: [=vt,=d,=d,pred].        [] :: [=v,=d,pred].
%[praise] :: [vt,-i].           [laugh] :: [v,-i].
%[lavinia] :: [d].              [titus] :: [d].

['-s'] :: [=v,+i,'T'].
[] :: [='V',='D',='D',v].
[praise] :: ['V',-i].  [criticize] :: ['V',-i].
['Beatrice'] :: ['D'].   ['Benedick'] :: ['D'].
[and]::[='T',='T','T'].

%['-s'] :: [=v,+i,'T'].
%[] :: [='V',='D',='D',v,-i].
%[praise] :: ['V'].
%['Beatrice'] :: ['D'].   ['Benedick'] :: ['D'].

%['-s'] :: [=v,+i,'T'].
%[] :: [='V',='D',='D',v,-i].
%[praise] :: ['V'].         
%['Beatrice'] :: ['D'].   ['Benedick'] :: ['D'].

%[praise]::[='D','V',-i].    [criticize]::[='D','V',-i].
%['Beatrice'] :: ['D',-case].   ['Benedick'] :: ['D',-case].
%['-s']::[=v,+i,+case,'T'].   []::[='V',+case,='D',v].

% only above items needed for "Beatrice criticize -s Benedick"

startCategory('T').


// Copyright (C) 2007-2008 - INRIA, DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

/////////////////////////////////////////////////
// fonction activée quand le curseur est bougé //
/////////////////////////////////////////////////
function update_aire()
global a b N hrmin hrmax croit ferreur;
global lock slider valueDisplay minDisplay maxDisplay line hr;
global sliderMin sliderMax sliderSteps;

// on ne fait rien quand le lock est mis
if lock then return, end;

// on ne fait rien quand il y a une erruer dans les données d'entrée
if ferreur then return, end;

// on met le lock
lock = %t ;
drawlater();
if hrmin <> [] then
  delete(hrmin);
  delete(hrmax);
end

// on affiche la valeur courante de N
curVal=get(slider,"Value");
N=curVal*(sliderMax-sliderMin)/sliderSteps+1;
N=floor(N);
set(valueDisplay,"String","Divisions : "+string(N));

// tracé de la droite et affichage de l'erreur
[s,S]=approche(a,b,N);
if croit then
  set(maxDisplay,"String","Valeur max : "+string(S));
  set(minDisplay,"String","Valeur min : "+string(s));
else
  set(maxDisplay,"String","Valeur max : "+string(s));
  set(minDisplay,"String","Valeur min : "+string(S));
end
drawnow();

// on enlève le lock
lock=%f;
endfunction

function [s,S]=approche(a,b,n)
global fonc hrmin hrmax croit;
s=0;
for k=0:n-1;
  s=s+fonc(a+k*(b-a)/n)
end
s=s*(b-a)/n;
S=0;
for k=1:n
  x(k)=a+(k-1)*(b-a)/n; y(k)=fonc(x(k));
  S=S+fonc(a+k*(b-a)/n)
end
x(n+1)=b; y(n+1)=fonc(x(n+1));
S=S*(b-a)/n;
Xmin=[];Ymin=[]; Xmax=[]; Ymax=[];
if croit then
  for i=1:n
    Xmin=[Xmin;x(i);x(i);x(i);x(i+1)];
    Ymin=[Ymin;0;y(i);y(i);y(i)];
    Xmax=[Xmax;x(i);x(i);x(i);x(i+1)];
    Ymax=[Ymax;y(i);y(i+1);y(i+1);y(i+1)];
  end
  Xmin=[Xmin;x(n+1);x(n+1)];
  Ymin=[Ymin;y(n);0];
  Xmax=[Xmax;x(n+1);x(n+1)];
  Ymax=[Ymax;y(n+1);y(n)];
else
  for j=1:n
    i=n+2-j;
    Xmin=[Xmin;x(i);x(i);x(i);x(i-1)];
    Ymin=[Ymin;0;y(i);y(i);y(i)];
    Xmax=[Xmax;x(i);x(i);x(i);x(i-1)];
    Ymax=[Ymax;y(i);y(i-1);y(i-1);y(i-1)];
  end
  Xmin=[Xmin;x(1);x(1)];
  Ymin=[Ymin;y(2);0];
  Xmax=[Xmax;x(1);x(1)];
  Ymax=[Ymax;y(1);y(2)];
end

xpoly(Xmin,Ymin); hrmin=gce();
hrmin.foreground=color("green");
xpoly(Xmax,Ymax); hrmax=gce();
hrmax.foreground=color("red");

endfunction


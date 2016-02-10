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
function update_ajuste()
global lock slider valueDisplay errorDisplay line hr;
global sliderMin sliderMax sliderSteps;

// on ne fait rien quand le lock est mis
if lock then return, end;

// on met le lock
lock = %t ;
drawlater();
if line <> [] then
  delete(line);
  delete(hr);
end

// on affiche la valeur courante de k
curVal=get(slider,"Value");
k=curVal*(sliderMax-sliderMin)/sliderSteps;
set(valueDisplay,"String","Pente : "+string(k));

// tracé de la droite et affichage de l'erreur
E=ajuste(k);
axes=gca(); axes.axes_visible=["off","off","on"];
drawnow();
set(errorDisplay,"String","Erreur : "+string(E));

// on enlève le lock
lock=%f;
endfunction

function E=ajuste(k)
global x y n line X xbar ybar hr;
Y=k*(X-xbar)+ybar;
d=y-(k*(x-xbar)+ybar); E=sum(d.*d);
plot(X,Y,"r","thickness",2); line=gce();
g=color("green");
xpolys([x;x],[y;k*(x-xbar)+ybar],g*ones(1,n)); hr=gce();
endfunction


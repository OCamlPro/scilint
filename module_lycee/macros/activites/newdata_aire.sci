// Copyright (C) 2007-2008 - INRIA, DIGITEO - Claude Gomez, Vincent Couvert
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

////////////////////////////////////////////////////////////////////////
// fonction activée quand le bouton ok du nombre de points est cliqué //
////////////////////////////////////////////////////////////////////////
function newdata_aire()
global a b N fonc hrmin hrmax croit ferreur;
global lock line aedit bedit fcedit;
global ff;

ferreur=%F;
fc=get(fcedit,"String");
try
  deff("[r]=fonc(x)","r="+fc);
catch
  messagebox("Mauvaise syntaxe dans la définition de la fonction","Erreur","error","modal");
  ferreur=%T; return;
end

a=get(aedit,"String");
[a,ierr]=evstr(a);
if ierr<>0 then
  messagebox("La borne a doit être un nombre","Erreur","error","modal");
  ferreur=%T; return;
end
if type(a)<>1|size(a,"*")<>1 then
  messagebox("La borne a doit être un nombre","Erreur","error","modal");
  ferreur=%T; return;
end

b=get(bedit,"String");
[b,ierr]=evstr(b);
if ierr<>0 then
  messagebox("La borne b doit être un nombre","Erreur","error","modal");
  ferreur=%T; return;
end
if type(b)<>1|size(b,"*")<>1 then
  messagebox("La borne b doit être un nombre","Erreur","error","modal");
  ferreur=%T; return;
end

if a >= b then
  ferreur=%T; messagebox("b doit être supérieur à a","Erreur","error","modal"); return;
end;

// test de la fonction : attention à la syntaxe non vectorielle
n=1000;
xx=linspace(a,b,n);
try
  for k=1:n, yy(k)=fonc(xx(k)); end;
  if ~isreal(yy) then
    messagebox("Le calcul de la fonction donne un nombre complexe","Erreur","error","modal");
    ferreur=%T; return
  end
catch
  [str,ierr]=lasterror();
  select ierr
  case 27 then
    messagebox("Division par 0 dans le calcul de la fonction","Erreur","error","modal");
  case 32 then
    messagebox("Singularité dans le calcul de ln ou de tan","Erreur","error","modal");
  else
    messagebox("Erreur dans le calcul de la fonction","Erreur","error","modal");
  end
  ferreur=%T; return
end
if min(yy) < 0 then
  messagebox("La fonction doit être positive","Erreur","error","modal");
  ferreur=%T; return;
end;
croit=%F; decroit=%F;
for k=2:n
 if yy(k) >= yy(k-1) then croit=%T; else decroit=%T; end;
end
if croit & decroit then
  messagebox("La fonction doit être monotone","Erreur","error","modal");
  ferreur=%T; return;
end;

// tracé de la fonction
drawlater();
scf(ff);
axes=gca();
if ~isempty(axes.children) then
  delete(axes.children);
end
// nouvelles bornes pour les axes après avoir enlevé les Inf et les Nan
infxx=find(isinf(xx)==%T); xx(infxx)=[]; yy(infxx)=[];
nanxx=find(isnan(xx)==%T); xx(nanxx)=[]; yy(nanxx)=[];
infyy=find(isinf(yy)==%T); xx(infyy)=[]; yy(infyy)=[];
nanyy=find(isnan(yy)==%T); xx(nanyy)=[]; yy(nanyy)=[];
axes.data_bounds=[min(xx),min(yy);max(xx),max(yy)];
plot(xx,yy);
drawnow();
lock=%f; hrmin=[];
update_aire();
endfunction


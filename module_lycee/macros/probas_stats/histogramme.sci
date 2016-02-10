// Copyright (C) 2002-2009 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function histogramme(a,n,couleur,varargin)
  [lhs,rhs]=argn();
  if rhs>3 then error("La fonction ""histogramme"" doit avoir au plus trois arguments."); end
  if rhs==2 then
    couleur="b";
  end
  if rhs<2 then error("La fonction ""histogramme"" doit avoir au moins deux arguments."); end
  if type(a)<>1|find(size(a)==1)==[]|type(n)<>1|find(size(n)==1)==[] then
    error("Les deux premiers arguments de la fonction ""histogramme"" doivent être des vecteurs de nombres.");
  end
  if size(a,"*")<>size(n,"*")+1 then
    error("Le premier argument de la fonction ""histogramme"" doit avoir une dimension de plus que le deuxième argument.");
  end
  if min(n)<0 then
    error("Le deuxième argument de la fonction ""histogramme"" doit être un vecteur de nombres positifs ou nuls.");
  end
  if sum(n)<%eps then
    error("Le deuxième argument de la fonction ""histogramme"" ne doit pas être le vecteur nul.");
  end
  vcol=["r","g","b","c","m","y","k","w"]; icol=find(couleur==vcol);
  if icol==[] then
    error("Le troisième argument de la fonction ""histogramme"" doit être une couleur.");
  end
  vcol2=["red","green","blue","cyan","magenta","yellow","black","white"];
  couleur=color(vcol2(icol));
  nrect=size(a,"*")-1;
  for i=1:nrect
    if a(i)>=a(i+1) then
      error("Le premier argument de la fonction ""histogramme"" doit avoir être un vecteur de nombres croissants.");
    end
  end
  xv=[]; yv=[];
  for i=1:nrect
    xv=[xv,a(i),a(i),a(i+1),a(i+1)];
    h(i)=n(i)/(a(i+1)-a(i));
    yv=[yv,0,h(i),h(i),0];
  end
  drawlater();
  plot(a(1),0); plot(a($),max(h));
  xpoly(xv,yv);
  hrect=gce(); hrect.foreground=couleur;
  axes=gca(); axes.axes_visible=["on","off","on"];
  drawnow();
endfunction


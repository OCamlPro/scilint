// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function graduations(pas,sous_grad,varargin)
  a=gca();
  [lhs,rhs]=argn();
  if rhs==0 then a.auto_ticks="on"; return; end
  if rhs>2 then error("La fonction ""graduations"" doit avoir au plus deux arguments."); end
  if type(pas)<>1|find(size(pas)==1)==[]|size(pas,"*")>2 then
    error("Le premier argument de la fonction ""graduations"" doit être un nombre ou un vecteur de nombres de dimension 2.");
  end
  if size(pas,"*")==1&pas<=0 then
    error("Le premier argument de la fonction ""graduations"" doit être un nombre positif.")
  end
  if size(pas,"*")==2&max(pas)<=0 then
    error("Le premier argument de la fonction ""graduations"" doit être un vecteur de deux nombres positifs.")
  end
  if rhs==1 then
    sous_grad=max(a.sub_ticks,[0,0]);
  else
    if type(sous_grad)<>1|find(size(sous_grad)==1)==[]|size(sous_grad,"*")>2 then
      error("Le deuxième argument de la fonction ""graduations"" doit être un nombre ou un vecteur de nombres de dimension 2.");
    end
    if size(sous_grad,"*")==1&(int(sous_grad)-sous_grad<>0|sous_grad<0) then
      error("Le deuxième argument de la fonction ""graduations"" doit être un entier positif ou nul.")
    end
    if size(sous_grad,"*")==2 then
      if int(sous_grad(1))-sous_grad(1)<>0|int(sous_grad(2))-sous_grad(2)<>0|sous_grad(1)<0|sous_grad(2)<0 then
      error("Le deuxième argument de la fonction ""graduations"" doit être un vecteur de deux nombres positifs ou nuls.")
      end
    end
  end
  if size(pas,"*")==1 then
    pas=[pas,pas];
  end
  if size(sous_grad,"*")==1 then
    sous_grad=[sous_grad,sous_grad];
  end
  x0=a.x_ticks.locations(1); x1=a.data_bounds(2,1);
  y0=a.y_ticks.locations(1); y1=a.data_bounds(2,2);
  x=x0:pas(1):x1; y=y0:pas(2):y1;
  a.x_ticks=tlist(["ticks","locations","labels"],x,string(x));
  a.y_ticks=tlist(["ticks","locations","labels"],y,string(y));
  a.sub_ticks=sous_grad;
endfunction


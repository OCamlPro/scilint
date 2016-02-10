// Copyright (C) 2010 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function cercle(a,b,r,varargin)
  [lhs,rhs]=argn();
  if rhs<>3&rhs<>4 then error("La fonction ""cercle"" doit avoir trois ou quatre arguments."); end
  if r<=0 then error("Le troisième argument de la fonction ""cercle"" doit être un nombre positif"); end
  theta=linspace(0,2*%pi,100);
  x=a+r*cos(theta);
  y=b+r*sin(theta);
  if rhs==3 then
    plot(x,y)
  else
    c=varargin(1);
    if c<>"r"&c<>"g"&c<>"b"&c<>"c"&c<>"m"&c<>"y"&c<>"k"&c<>"w" then
        error("Le quatrième argument doit représenter une couleur")
    end
    plot(x,y,c);
  end
endfunction

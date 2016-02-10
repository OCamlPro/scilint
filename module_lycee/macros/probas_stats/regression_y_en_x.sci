// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=regression_y_en_x(x,y,varargin)
  [lhs,rhs]=argn();
  if rhs<>2 then error("La fonction ""regression_y_en_x"" doit avoir deux arguments."); end
  if type(x)<>1|find(size(x)==1)==[]|type(y)<>1|find(size(y)==1)==[] then
    error("Les arguments de la fonction ""regression_y_en_x"" doivent être des vecteurs de nombres.");
  end
  if size(x,"*")<>size(y,"*") then
    error("Les arguments de la fonction ""regression_y_en_x"" doivent être des vecteurs de même dimension.");
  end
  if abs(min(x)-max(x))<%eps then
    error("Le premier argument de la fonction ""regression_y_en_x"" ne doit pas avoir tous ses éléments égaux.");
  end
  n=size(x,"*");
  xbar=sum(x)/n;
  ybar=sum(y)/n;
  cov=sum(x.*y)/n-xbar*ybar;
  varx=sum(x.*x)/n-xbar*xbar;
  a=cov/varx; b=ybar-a*xbar;
  r=[a,b];
endfunction


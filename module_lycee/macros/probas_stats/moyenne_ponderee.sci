// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=moyenne_ponderee(x,n,varargin)
  [lhs,rhs]=argn();
  if rhs<>2 then error("La fonction ""moyenne_ponderee"" doit avoir deux arguments."); end
  if type(x)<>1|find(size(x)==1)==[]|type(n)<>1|find(size(n)==1)==[] then
    error("Les arguments de la fonction ""moyenne_ponderee"" doivent être des vecteurs de nombres.");
  end
  if size(x,"*")<>size(n,"*") then
    error("Les arguments de la fonction ""moyenne_ponderee"" doivent être des vecteurs de même dimension.");
  end
  if min(n)<0 then
    error("Le deuxième argument de la fonction ""moyenne_ponderee"" doit être un vecteur de nombres positifs ou nuls.");
  end
  if sum(n)<%eps then
    error("Le deuxième argument de la fonction ""moyenne_ponderee"" ne doit pas être le vecteur nul.");
  end
  r=(x*n')/sum(n);
endfunction


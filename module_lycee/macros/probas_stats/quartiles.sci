// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function q=quartiles(x,varargin)
  [lhs,rhs]=argn();
  if rhs<>1 then error("La fonction ""quartiles"" doit avoir un argument."); end
  if type(x)<>1|find(size(x)==1)==[] then
    error("L''argument de la fonction ""quartiles"" doit être un vecteur de nombres.");
  end
  n=size(x,"*"); x=gsort(x,"g","i");
  iq1=ceil(n/4); iq3=ceil(3*n/4);
  q=[x(iq1),x(iq3)];
endfunction


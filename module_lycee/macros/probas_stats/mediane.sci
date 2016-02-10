// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=mediane(x,varargin)
  [lhs,rhs]=argn();
  if rhs<>1 then error("La fonction ""mediane"" doit avoir un argument."); end
  if type(x)<>1|find(size(x)==1)==[] then
    error("L''argument de la fonction ""mediane"" doit être un vecteur de nombres.");
  end
  n=size(x,"*"); x=gsort(x,"g","i");
  if 2*int(n/2)==n then
    r=(x(n/2)+x(n/2+1))/2;
  else
    r=x((n+1)/2);
  end
endfunction


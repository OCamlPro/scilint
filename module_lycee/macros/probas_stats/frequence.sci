// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function f=frequence(n,s,varargin)
  [lhs,rhs]=argn();
  if rhs<>2 then error("La fonction ""frequence"" doit avoir deux arguments."); end
  if type(n)<>1|size(n,"*")<>1|int(n)-n<>0 then
    error("Le premier argument de la fonction ""frequence"" doit être un entier.");
  end
  if type(s)<>1|find(size(s)==1)==[] then
    error("Le deuxième argument de la fonction ""frequence"" doit être un vecteur de nombres.");
  end
  f=length(find(s==n))/length(s);
endfunction


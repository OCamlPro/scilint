// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=numero_premier(n,varargin)
  [lhs,rhs]=argn();
  if rhs<>1 then error("La fonction ""numero_premier"" doit avoir un argument."); end
  if type(n)<>1|size(n,"*")<>1|int(n)-n<>0|n<0 then
    error("L''argument de la fonction ""numero_premier"" doit être un entier positif ou nul.");
  end
  nmax=664579;
  if n>nmax then
    error("L''argument de la fonction ""numero_premier"" doit être plus petit que "+sprintf("%.0f",nmax+1)+".");
  end
  p=liste_premiers(10000000);
  r=p(n);
endfunction


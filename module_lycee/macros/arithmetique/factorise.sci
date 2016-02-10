// Copyright (C) 2002-2008 - DIGITEO - Farid Belahcene, Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=factorise(n,varargin)
  [lhs,rhs]=argn();
  if rhs<>1 then error("La fonction ""factorise"" doit avoir un argument."); end
  if type(n)<>1|size(n,"*")<>1|int(n)-n<>0|n<0 then
    error("L''argument de la fonction ""factorise"" doit �tre un entier positif ou nul.");
  end
  c=3.1;
  r=factor(n)
endfunction

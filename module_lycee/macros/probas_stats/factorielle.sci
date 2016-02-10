// Copyright (C) 2002-2008 - DIGITEO - Farid Belahcene et Claude Gomez
// Copyright (C) 2010 - DIGITEO - Michael Baudin
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=factorielle(n,varargin)
  [lhs,rhs]=argn();
  if rhs<>1 then error("La fonction ""factorielle"" doit avoir un argument."); end
  if type(n)<>1|size(n,"*")<>1|int(n)-n<>0|n<0 then
    error("L''argument de la fonction ""factorielle"" doit �tre un entier positif ou nul.");
  end
  r=factorial(n)
endfunction

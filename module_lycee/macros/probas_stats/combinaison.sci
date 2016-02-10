// Copyright (C) 2002-2009 - DIGITEO - Michael Baudin et Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=combinaison(n,p,varargin)
  [lhs,rhs]=argn();
  if rhs<>2 then error("La fonction ""combinaison"" doit avoir deux arguments."); end
  if type(n)<>1|size(n,"*")<>1|int(n)-n<>0|n<0|type(p)<>1|size(p,"*")<>1|int(p)-p<>0|p<0 then
    error("Les arguments de la fonction ""combinaison"" doivent être des entiers positifs ou nuls.");
  end
  if p>n then
    error("Le deuxième argument de la fonction ""combinaison"" doit être inférieur ou égal au premier.");
  end
  r=round(exp(gammaln(n+1)-gammaln(p+1)-gammaln(n-p+1)));
endfunction


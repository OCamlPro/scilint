// Copyright (C) 2002-2008 - DIGITEO - Farid Belahcene et Claude Gomez
// Copyright (C) 2010 - DIGITEO - Michael Baudin
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=arrangement(n,p,varargin)
  [lhs,rhs]=argn()
  if ( rhs<>2 ) then
    error("La fonction ""arrangement"" doit avoir deux arguments.")
  end
  if ( type(n)<>1|size(n,"*")<>1|int(n)-n<>0|n<0|type(p)<>1|size(p,"*")<>1|int(p)-p<>0|p<0 ) then
    error("Les arguments de la fonction ""arrangement"" doivent être des entiers positifs ou nuls.")
  end
  if ( p>n ) then
    error("Le deuxi�me argument de la fonction ""arrangement"" doit être inférieur ou égal au premier.")
  end
  r = exp(gammaln(n+1)-gammaln(n-p+1))
  if ( and(round(n)==n) & and(round(p)==p) ) then
    r = round ( r )
  end
endfunction

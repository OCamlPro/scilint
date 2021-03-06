// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
// Copyright (C) 2010 - DIGITEO - Michael Baudin
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=ecart_type(x,varargin)
  [lhs,rhs]=argn();
  if rhs<>1 then error("La fonction ""ecart_type"" doit avoir un argument."); end
  if type(x)<>1|find(size(x)==1)==[] then
    error("L''argument de la fonction ""ecart_type"" doit être un vecteur de nombres.");
  end
  r = nanstdev(x)
endfunction

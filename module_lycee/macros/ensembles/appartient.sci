// Copyright (C) 2002-2009 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=appartient(elem,ens,varargin)
  [lhs,rhs]=argn();
  if rhs<>2 then error("La fonction ""appartient"" doit avoir deux arguments."); end
  if type(elem)<>10|size(elem,"*")<>1 then
    error("Le premier argument de la fonction ""appartient"" doit être une chaîne de caractères.");
  end
  if typeof(ens)<>"ens" then
    error("Le deuxième argument de la fonction ""appartient"" doit être un ensemble.");
  end
  if find(ens.elem==elem)<>[] then r=%T; return; end
  r=%F;
endfunction


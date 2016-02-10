// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=taille(x,varargin)
  [lhs,rhs]=argn();
  if rhs<>1 then error("La fonction ""taille"" doit avoir un argument."); end
  if x==[] then
      r=0;
  else
      r=size(x);
      if find(r==1)<>[] then r=size(x,"*"); end
  end
endfunction

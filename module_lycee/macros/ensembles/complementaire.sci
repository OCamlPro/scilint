// Copyright (C) 2002-2009 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function s=complementaire(ens1,ens2,varargin)
  [lhs,rhs]=argn();
  if rhs<>2 then error("La fonction ""complementaire"" doit avoir deux arguments."); end
  if typeof(ens1)<>"ens"|typeof(ens2)<>"ens" then
    error("Les arguments de la fonction ""complementaire"" doivent être des ensembles.");
  end
  v1=ens1.elem; s=ens2;
  for e=v1'
    if ~appartient(e,s) then
      error("Le premier ensemble doit être inclus dans le second.");
    end
    s=enlever(e,s);
  end
endfunction


// Copyright (C) 2002-2009 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=inclus(ens1,ens2,varargin)
  [lhs,rhs]=argn();
  if rhs<>2 then error("La fonction ""inclus"" doit avoir deux arguments."); end
  if typeof(ens1)<>"ens"|typeof(ens2)<>"ens" then
    error("Les arguments de la fonction ""inclus"" doivent être des ensembles.");
  end
  r=%F;
  n=size(ens1);
  if n>size(ens2) then
    return;
  end
  v1=ens1.elem; v2=ens2.elem;
  for i=1:n
    if find(v1(i)==v2)==[] then
      return;
    end
  end
  r=%T;
endfunction


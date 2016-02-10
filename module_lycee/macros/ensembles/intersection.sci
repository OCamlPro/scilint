// Copyright (C) 2002-2009 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function s=intersection(ens1,ens2,varargin)
  [lhs,rhs]=argn();
  if rhs<>2 then error("La fonction ""intersection"" doit avoir deux arguments."); end
  if typeof(ens1)<>"ens"|typeof(ens2)<>"ens" then
    error("Les arguments de la fonction ""intersection"" doivent être des ensembles.");
  end
  if size(ens1)>size(ens2) then
    v1=ens2.elem;
    v2=ens1.elem;
  else
    v1=ens1.elem;
    v2=ens2.elem;
  end
  v=[];
  for i=1:size(v1,"*")
    if find(v1(i)==v2)<>[] then
      v=[v;v1(i)];
    end
  end
  s=mlist(["ens","elem"],v);
endfunction


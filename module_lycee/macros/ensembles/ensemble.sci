// Copyright (C) 2002-2010 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function s=ensemble(varargin)
  [lhs,rhs]=argn();
  if rhs==0 then
    s=mlist(["ens","elem"],[]); return;
  end
  i=0;
  for elem=varargin
    if type(elem)<>10|find(size(elem)==1)==[] then
      error("Les arguments de la fonction ""ensemble"" doivent être des chaînes de caractères ou des vecteurs de chaînes de caractères.");
    end
    for j=1:size(elem,"*")
      i=i+1; v(i)=elem(j);
    end
  end
  s=ensemble_vecteur(v);
endfunction

// Copyright (C) 2002-2009 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=produit(a,b)
  r=[];
  for i=a
    for j=b
      r=[r,i*j];
    end
  end
endfunction

function r=diviseurs(n,varargin)
  [lhs,rhs]=argn();
  if rhs<>1 then error("La fonction ""diviseurs"" doit avoir un argument."); end
  if type(n)<>1|size(n,"*")<>1|int(n)-n<>0|n<=0 then
    error("L''argument de la fonction ""diviseurs"" doit être un entier positif.");
  end
  if n==1 then r=1; return; end;
  f=factorise(n);
  fu=unique(f);
  r=[1];
  for z=fu
    nf=length(find(z==f));
    s=[1];
    for i=1:nf
      s=[s,z^i];
    end
    r=produit(r,s)
  end
  r=gsort(r,"g","i")
endfunction


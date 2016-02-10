// Copyright (C) 2002-2009 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function v=valeur(a,varargin)
  [lhs,rhs]=argn();
  if rhs<>1 then error("La fonction ""valeur"" doit avoir un argument."); end
  if typeof(a)<>"ens" then
    if type(a)<>10|size(a,"*")<>1 then
      error("L''argument de la fonction ""valeur"" doit être un ensemble ou une chaîne de caractères.");
    end
  end
  if typeof(a)=="ens" then
    ens=a;
    n=size(ens); v=zeros(1,n);
    for i=1:n
      e=ens.elem(i);
      j=strindex(e,"(");
      if j<>[] & part(e,length(e))==")" then
        str=part(e,j+1:length(e)-1);
        [nbr,ierr]=evstr(str);
        if ierr==0 then
          v(i)=nbr;
        else
          error("Mauvaise valeur d''un élément de l''ensemble.");
        end
      end
    end
  else
    v=ascii(a)-48;
    if max(abs(v))>9 then
      error("Mauvaise chaîne de caractères.");
    end
    v=v';
  end
endfunction


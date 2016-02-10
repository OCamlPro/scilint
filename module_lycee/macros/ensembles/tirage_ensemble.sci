// Copyright (C) 2002-2010 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function t=tirage_ensemble(n,ens,varargin)
  [lhs,rhs]=argn();
  if rhs<>2 then error("La fonction ""tirage_ensemble"" doit avoir deux arguments."); end
  if type(n)<>1|size(n,"*")<>1|int(n)-n<>0|n<=0 then
    error("Le premier argument de la fonction ""tirage_ensemble"" doit être un entier positif.");
  end
  if n>size(ens) then
    error("Le premier argument de la fonction ""tirage_ensemble"" ne doit pas être plus grand que la taille de l''ensemble.");
  end
  if typeof(ens)<>"ens" then
    error("Le deuxième argument de la fonction ""tirage_ensemble"" doit être un ensemble.");
  end
  i=[];
  while size(i,"*")<>n
    i=unique(grand(1,n,"uin",1,size(ens)));
  end
  v=ens.elem(i);
  v=gsort(v,"g","i");
  t=mlist(["ens","elem"],v);
endfunction


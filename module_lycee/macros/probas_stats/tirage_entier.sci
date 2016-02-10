// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=tirage_entier(n,n1,n2,varargin)
  [lhs,rhs]=argn();
  if rhs<>3 then error("La fonction ""tirage_entier"" doit avoir trois arguments."); end
  if type(n)<>1|size(n,"*")<>1|int(n)-n<>0|n<1 then
    error("Le premier argument de la fonction ""tirage_entier"" doit être un entier positif.");
  end
  if type(n1)<>1|size(n1,"*")<>1|int(n1)-n1<>0|type(n2)<>1|size(n2,"*")<>1|int(n2)-n2<>0 then
    error("Les arguments de la fonction ""tirage_entier"" doivent être des entiers.");
  end
  if n1>n2 then
    error("Le deuxième argument de la fonction ""tirage_entier"" doit être inférieur au troisième argument.");
  end
  r=grand(1,n,"uin",n1,n2);
endfunction


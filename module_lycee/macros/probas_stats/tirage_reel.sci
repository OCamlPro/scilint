// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=tirage_reel(n,a,b,varargin)
  [lhs,rhs]=argn();
  if rhs<>3 then error("La fonction ""tirage_reel"" doit avoir trois arguments."); end
  if type(n)<>1|size(n,"*")<>1|int(n)-n<>0|n<1 then
    error("Le premier argument de la fonction ""tirage_reel"" doit être un entier positif.");
  end
  if type(a)<>1|size(a,"*")<>1|type(b)<>1|size(b,"*")<>1 then
    error("Les arguments de la fonction ""tirage_reel"" doivent être des nombres.");
  end
  if a>b then error("Le deuxième argument de la fonction ""tirage_reel"" doit être inférieur au troisième argument."); end
  r=grand(1,n,"unf",a,b);
endfunction


// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=reste(a,b,varargin)
  [lhs,rhs]=argn();
  if rhs<>2 then error("La fonction ""reste"" doit avoir deux arguments."); end
  if type(a)<>1|size(a,"*")<>1|int(a)-a<>0|type(b)<>1|size(b,"*")<>1|int(b)-b<>0 then
    error("Les arguments de la fonction ""reste"" doivent être des entiers.");
  end
  if b==0 then
    error("Le deuxième argument de la fonction ""reste"" ne doit pas être nul.");
  end
  if b>0 then
    r=a-b*floor(a/b);
  else
    r=a-b*ceil(a/b);
  end
endfunction


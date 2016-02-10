// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=loi_exp(lambda,t,varargin)
  [lhs,rhs]=argn();
  if rhs<>2 then error("La fonction ""loi_exp"" doit avoir deux arguments."); end
  if type(lambda)<>1|size(lambda,"*")<>1|lambda<=0 then
    error("Le premier argument de la fonction ""loi_exp"" doit être un nombre positif.");
  end
  if type(t)<>1|size(t,"*")<>1 then
    error("Le deuxième argument de la fonction ""loi_exp"" doit être un nombre.");
  end
  if t<=0 then
    r=0;
  else
    r=1-exp(-lambda*t);
  end
endfunction


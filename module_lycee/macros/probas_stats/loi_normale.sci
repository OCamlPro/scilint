// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=loi_normale(t,xbar,sigma,varargin)
  [lhs,rhs]=argn();
  if rhs<>3 then error("La fonction ""loi_normale"" doit avoir trois arguments."); end
  if type(sigma)<>1|size(sigma,"*")<>1|sigma<=0 then
    error("Le troisième argument de la fonction ""loi_normale"" doit être un nombre positif.");
  end
  if type(t)<>1|size(t,"*")<>1|type(xbar)<>1|size(xbar,"*")<>1 then
    error("Les arguments de la fonction ""loi_normale"" doit être des nombres.");
  end
  r=cdfnor("PQ",t,xbar,sigma)
endfunction


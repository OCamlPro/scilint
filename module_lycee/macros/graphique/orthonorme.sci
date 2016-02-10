// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function orthonorme(a,varargin)
  [lhs,rhs]=argn();
  if rhs==0 then a=gca(); end
  if rhs>1 then error("La fonction ""orthonorme"" doit avoir au plus un argument."); end
  if type(a)<>9 then
    error("Le premier argument de la fonction ""orthonorme"" doit être un objet de type ""Axes"".");
  end
  if a.isoview=="off" then a.isoview="on"; return; end;
  if a.isoview=="on" then a.isoview="off"; return; end;
endfunction


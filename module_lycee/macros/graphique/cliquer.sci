// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=cliquer(varargin)
  [lhs,rhs]=argn();
  if rhs<>0 then error("La fonction ""cliquer"" ne doit pas avoir d''argument."); end
  [c,x,y]=xclick();
  r=[x,y];
endfunction


// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function quadrillage(varargin)
  [lhs,rhs]=argn();
  if rhs<>0 then error("La fonction ""quadrillage"" ne doit pas avoir d''argument."); end
  a=gca();
  g=a.grid;
  if and(g==[0,0]) then a.grid=[-1,-1]; return; end;
  if and(g==[-1,-1]) then a.grid=[0,0]; return; end;
endfunction


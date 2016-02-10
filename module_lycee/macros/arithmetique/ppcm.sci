// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=ppcm(m,n,varargin)
  [lhs,rhs]=argn();
  if rhs<>2 then error("La fonction ""ppcm"" doit avoir deux arguments."); end
  if type(m)<>1|size(m,"*")<>1|type(n)<>1|size(n,"*")<>1|int(m)-m<>0|int(n)-n<>0 then
    error("Les arguments de la fonction ""ppcm"" doivent être des entiers.");
  end
  imin=-46340; imax=46340;
  if m<imin|m>imax|n<imin|n>imax then
    error("Les arguments de la fonction ""ppcm"" doivent être compris entre "+sprintf("%.0f",imin)+..
    " et "+sprintf("%.0f",imax)+".");
  end
  r=double(abs(lcm(int32([m,n]))));
endfunction


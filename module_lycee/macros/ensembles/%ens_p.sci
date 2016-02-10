// Copyright (C) 2002-2009 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function %ens_p(ens)
  l=size(ens.elem,"*");
  if l==0 then
    str="{}";
  elseif l==1 then
    str="{"+ens.elem(1)+"}";
  else
    str="{";
    for i=1:l-1
      str=str+ens.elem(i)+",";
    end
    str=str+ens.elem(l)+"}";
  end
  disp(str);
endfunction


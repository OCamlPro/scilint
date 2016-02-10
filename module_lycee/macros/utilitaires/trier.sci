// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=trier(x,sens,varargin)
  [lhs,rhs]=argn();
  if rhs>2 then error("La fonction ""trier"" doit avoir au plus deux arguments."); end
  if type(x)<>1&type(x)<>10 then
    error("Le premier argument de la fonction ""trier"" doit être un vecteur de nombres ou de chaînes de caractères.");
  end
  if rhs==2 then
    if sens<>">"&sens<>"<" then
      error("Le deuxième argument de la fonction ""trier"" doit être "">"" ou ""<"".");
    end
  end
  if rhs==1 then sens=">"; end
  if sens==">" then
    r=gsort(x,"g","i");
  else
    r=gsort(x,"g","d");
  end
endfunction


// Copyright (C) 2002-2009 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function s=ensemble_vecteur(v)
  v=unique(v); v=gsort(v,"g","i");
  s=mlist(["ens","elem"],v);
endfunction


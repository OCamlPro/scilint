// Copyright (C) 2010 - DIGITEO - Michael Baudin
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

//
// %TVKBDATA_p --
//   Prints the string containing the virtual keyboard
//
function %TVKBDATA_p ( this )
  str = string(this)
  nbrows = size(str,"r")
  for i = 1 : nbrows
    mprintf("%s\n",str(i))
  end
endfunction


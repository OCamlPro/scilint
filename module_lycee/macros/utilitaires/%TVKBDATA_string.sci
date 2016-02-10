// Copyright (C) 2010 - DIGITEO - Michael Baudin
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

//
// %TVKBDATA_string --
//   Returns the string containing the virtual keyboard data
//
function str = %TVKBDATA_string ( this )
  str = []
  k = 1
  str(k) = sprintf("Virtual Keyboard:")
  k = k + 1
  str(k) = sprintf("======================")
  k = k + 1
  str(k) = sprintf("s = %s", string(this.s))
  if ( this.stack == [] ) then
    k = k + 1
    str(k) = sprintf("stack = []")
  else
  for i = 1 : this.s
    k = k + 1
    str(k) = sprintf("stack(%d) = ""%s""", i , this.stack(i))
  end
  end
endfunction


// Copyright (C) 2007-2008 - INRIA, DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

////////////////////////////////////////////////////////////////////////
// fonction activée quand le bouton ok du nombre de points est cliqué //
////////////////////////////////////////////////////////////////////////
function newnp_ajuste()
global lock line npedit;
np=get(npedit,"String");
[np,ierr]=evstr(np);
if ierr<>0 then
  messagebox("Le nombre de points doit être un nombre entre 2 et 1000","Erreur","error","modal");
  return;
end
if type(np)<>1|size(np,"*")<>1 then
   messagebox("Le nombre de points doit être un nombre entre 2 et 1000","Erreur","error","modal");
  return;
end
if np<2 then
  np=2;
elseif np>1000 then
  np=1000;
else
  np=floor(np);
end;
set(npedit,"String",string(np));
points_ajuste(np);
lock=%f; line=[];
update_ajuste();
endfunction


// Copyright (C) 2009 - DIGITEO
// Copyright (C) 2010 - DIGITEO - Michael Baudin
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

directories = [ ..
    "activites"    ; ..
    "analyse"      ; ..
    "arithmetique" ; ..
    "graphique"    ; ..
    "probas_stats" ; ..
    "ensembles"    ; ..
    "utilitaires" ];

cwd = pwd();
directories = pathconvert(get_absolute_file_path("buildmacros.sce")+"/"+directories);

for i=1:size(directories,"*")
	chdir(directories(i));
	exec("buildmacros.sce");
end
chdir(cwd);



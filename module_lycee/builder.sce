// Copyright (C) 2009 - DIGITEO
// Copyright (C) 2011 - DIGITEO - Allan CORNET
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

mode(-1);
lines(0);

function main_builder()
    TOOLBOX_NAME  = "module_lycee";
    TOOLBOX_TITLE = "Module Scilab pour les lycées";

    toolbox_dir = get_absolute_file_path("builder.sce");

    tbx_builder_macros(toolbox_dir);
    tbx_builder_help(toolbox_dir);
    tbx_build_loader(TOOLBOX_NAME, toolbox_dir);
    tbx_build_cleaner(TOOLBOX_NAME, toolbox_dir);
endfunction

main_builder();
clear main_builder

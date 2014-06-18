// Copyright (C) 2008 - INRIA
// Copyright (C) 2009-2011 - DIGITEO

// This file is released under the 3-clause BSD license. See COPYING-BSD.

mode(-1);
lines(0);

TOOLBOX_NAME  = "jit_ocaml3";
TOOLBOX_TITLE = "JitOCaml";
toolbox_dir   = get_absolute_file_path("builder.sce");

// Check Scilab's version
// =============================================================================

try
v = getversion("scilab");
catch
  error(gettext("Scilab 6 or more is required."));
end

if v(1) < 5 then
  // new API in scilab 6
  error(gettext('Scilab 6 or more is required.'));
end

// Check modules_manager module availability
// =============================================================================

if ~isdef('tbx_build_loader') then
  error(msprintf(gettext("%s module not installed."), 'modules_manager'));
end

tbx_builder_macros(toolbox_dir);
tbx_builder_src(toolbox_dir);
tbx_builder_gateway(toolbox_dir);
//tbx_builder_help(toolbox_dir);
tbx_build_loader(TOOLBOX_NAME, toolbox_dir);
tbx_build_cleaner(TOOLBOX_NAME, toolbox_dir);




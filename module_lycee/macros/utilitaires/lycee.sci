// Copyright (C) 2009 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function lycee(b)

  if b then

    ieee(2);
    format(16);
    funcprot(0);

    if getscilabmode()=="STD" then
      delmenu("Lycée.");
      addmenu("Lycée.",["Ajustement affine","Calcul d''aire"],list(2,"actions")); // le "." en plus résoud le problème d'encodage
    end

    if getscilabmode() <> "NWNI" then
			a              = gda();
			a.x_location   = "origin";
			a.y_location   = "origin";
			a.box          = "off";
			a.tight_limits = "on";
	end
		
  else

    ieee(0);
    format(10);
    funcprot(1);

    if getscilabmode()=="STD" then
      delmenu("Lycée");
    end

    if getscilabmode() <> "NWNI" then
			a              = gda();
			a.x_location   = "bottom";
			a.y_location   = "left";
			a.box          = "on";
			a.tight_limits = "off";
    end

  end

endfunction

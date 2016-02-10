//
// Copyright (C) 2009-2009 - DIGITEO - Bruno JOFRET
// Copyright (C) 2010 - DIGITEO - Michael Baudin
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
//
//

function [] = virtualKeyboard()
	global vkbdata
        vkbdata = vkbdata_new()

        // Set the virtual keyboard
	kbd = ...
	[
          "("   "sin"  "7" "8"     "9"    "/"
          ")"   "cos"  "4" "5"     "6"    "*"
          "^"   "tan"  "1" "2"     "3"    "-"
          "1/x" "log"  ""  "0"     "."    "+"
          "=="  "sqrt" ""  "Show" "Reset" "Enter"
          ]
        // Size of the buttons : width, height
	buttonSize = [40 40];
	[nbLines, nbColumns] = size(kbd)
        // Axes width, height
        axwi = nbColumns * buttonSize(1)
        axhe = nbLines * buttonSize(2) + buttonSize(2)
	drawlater()
	f=gcf()
        f.figure_name = "Calculator"
        // Add text
        x = 0
        y = 420
        wi = nbColumns * buttonSize(2)
        he = buttonSize(1)
        fontsize = 12
        kbdtext = uicontrol ( f , "style" , "text" )
        // Configure options
        kbdtext.position = [x y wi he]
        kbdtext.BackgroundColor = [1 1 1]
        kbdtext.Horizontalalignment = "right"
        kbdtext.FontSize = fontsize
        kbdtext.String = ""
        // Store the path
        vkbdata.txtfig = kbdtext
        // Add the virtual keyboard
        f.axes_size = [axwi axhe]
	button = uicontrol("Style", "pushbutton")
	for i = 1:nbLines
		for j = 1:nbColumns
			button(i,j) = uicontrol("Style", "pushbutton")
			button(i,j).callback = "keyboardAction("""+kbd(nbLines - i + 1, j)+""")"
			button(i,j).String = kbd(nbLines - i + 1, j)
			button(i,j).position = [
                          (j - 1) * buttonSize(1) ...
			  (i - 1) * buttonSize(2) ...
                          buttonSize(1) ...
                          buttonSize(2)
                        ]
		end
	end
	drawnow()

endfunction

function this = vkbdata_new ( )
        this = tlist(["TVKBDATA"
        "s"
        "stack"
        "txtfig"
        ])
        // The size of the stack
        this.s = 0
        // The stack, a matrix of strings
        this.stack = []
        // The text
        this.txtfig = []
endfunction


//
// Copyright (C) 2010 - DIGITEO - Michael Baudin
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
//
//


function keyboardAction(var)
	global vkbdata

	verbose = %f

        if ( verbose ) then
		disp(var)
                disp(typeof(var))
        end
	select var
        case "Show" then
        	// Print the stack
		disp(vkbdata)
        case "Reset" then
        	// Reset the stack
		vkbdata = vkbdata_reset ( vkbdata )
                // Display the stack
                disp(vkbdata)
                // Update the display
                vkbdata.txtfig.String = ""
	case "Enter" then
		// Evaluate the stack, from start to end
                if ( vkbdata.stack == [] ) then
                  mprintf("\n=====================\n")
                  mprintf("Nothing to compute.\n")
                  return
                end
                instr = "result = " + vkbdata_computeinstr ( vkbdata )
                ierr = execstr(instr,"errcatch")
                if ( ierr <> 0 ) then
                  mprintf("\n=====================\n")
                  mprintf("Calculator Error!\n")
                  return
                else
                  mprintf("\n=====================\n")
                  mprintf("result = \n")
                  disp(result)
                end
                // Reset the stack
                vkbdata = vkbdata_reset ( vkbdata )
                // Put the result into the stack
                vkbdata.s = 1
                vkbdata.stack = string(double(result))
                // Update the display
                vkbdata.txtfig.String = vkbdata_computeinstr ( vkbdata )
                // Display the stack
                disp(vkbdata)
	else
		// Push the string at the end of the stack
		vkbdata.stack($+1) = var
		vkbdata.s = vkbdata.s + 1
                // Display the stack
		disp(vkbdata)
                // Update the display
                vkbdata.txtfig.String = vkbdata_computeinstr ( vkbdata )
	end
        if ( verbose ) then
		disp(vkbdata)
        end
endfunction

function this = vkbdata_reset ( this )
        this.s = 0
        this.stack = []
endfunction
function instr = vkbdata_computeinstr ( vkbdata )
	instr = ""
        for i = 1 : vkbdata.s
   	     instr = instr + vkbdata.stack(i)
        end
endfunction


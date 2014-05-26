#include "api_scilab.h"

int sci_cjit_ocaml(char *fname)
{
     SciErr sciErr;

     int iRet, iRows, iCols, complex;

     int *piAddrArg = NULL;

     double pdblReal;
     sciprint("DUDE\n");
     CheckInputArgument(pvApiCtx, 1, 1) ;
     CheckOutputArgument(pvApiCtx, 1, 1) ;
     sciprint("DUDE1\n");
     /* get variable name which is 1st argument */
     sciErr = getVarAddressFromPosition(pvApiCtx, 1, &piAddrArg);
     if (sciErr.iErr)
     {
          sciprint("DUDE2\n");
          printError(&sciErr, 0);
          return 0;
     }
     
     if ( !isDoubleType(pvApiCtx, piAddrArg) )
     {
          Scierror(999, 
                   "%s: Wrong type for input argument #%d: A double expected.\n", 
                   fname, 
                   1);
          return 1;
     }

     sciErr = getVarDimension(pvApiCtx, piAddrArg, &iRows, &iCols);   
     
     if (iRows != 1 || iCols != 1) {
          Scierror(999, 
                   "%s: Wrong type for input argument #%d: A double (1 x 1) expected.\n", 
                   fname, 
                   1);
          return 1;
     }

     complex = isVarComplex(pvApiCtx, piAddrArg);
     
     if (complex) {
          Scierror(999, 
                   "%s: Wrong type for input argument #%d: don't know what to do with complex value.\n", 
                   fname, 
                   1);
          return 1;
     }

     iRet = getScalarDouble(pvApiCtx, piAddrArg, &pdblReal);

     if (iRet)
     {
          return iRet;
     }

     // Call OCaml code !!!
     
     caml_startup(argv);
     /* Do some computation */
     int result = fib((int) pdblReal);
     sciprint("res = %i\n", result);
     // ===================
     
     iRet = createScalarDouble(pvApiCtx, nbInputArgument(pvApiCtx) + 1, pdblReal);
     
     if (iRet)
     {
          return iRet;
     }

     AssignOutputVariable(pvApiCtx, 1) = nbInputArgument(pvApiCtx) + 1;
     ReturnArguments(pvApiCtx);
}

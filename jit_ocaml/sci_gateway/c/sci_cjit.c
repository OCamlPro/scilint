#include "api_scilab.h"

/* ==================================================================== */
extern int cjit_call_function(int function_id);
/* ==================================================================== */

int sci_cjit(char *fname, void *pvApiCtx)
{
     SciErr sciErr;
     int *piAddr = NULL;
     int iType, complex, res;
     int iRows		= 0;
     int iCols		= 0;
     double dbl;
     CheckInputArgument(pvApiCtx, 1, 1);
     CheckOutputArgument(pvApiCtx, 1, 1);

     sciErr = getVarAddressFromPosition(pvApiCtx, 1, &piAddr);
     if (sciErr.iErr) {
       printError(&sciErr, 0);
       return 1;
     }
     
     if ( !isDoubleType(pvApiCtx, piAddr) ) {
       Scierror(999, "%s: Wrong type for input argument #%d: A double expected.\n", fname, 1);
       return 1;
     }
     
     sciErr = getVarDimension(pvApiCtx, piAddr, &iRows, &iCols);
     if(sciErr.iErr) {
       Scierror(999, 
		"%s: can't get dimensions for the free variables.\n", 
		fname, 
		1);
       return 1;
     }
     
     if (iRows != 1 || iCols != 1) {
       Scierror(999,
     		"%s: argument should be a scalar.\n",
     		fname,
     		1);
       return 1;
     }

     if ( isVarComplex(pvApiCtx, piAddr) ) {
       Scierror(999, "%s: argument should be a real.\n", fname, 1);
       return 1;
     }
     res = getScalarDouble(pvApiCtx, piAddr, &dbl);
     res = cjit_call_function((int)dbl);

     res = createScalarDouble(pvApiCtx, nbInputArgument(pvApiCtx) + 1, res);
     
     AssignOutputVariable(pvApiCtx, 1) = nbInputArgument(pvApiCtx) + 1;
     ReturnArguments(pvApiCtx);  
}


#include "api_scilab.h"

/* ==================================================================== */
extern int ctest(char *in, char *out);
/* ==================================================================== */
int sci_ctest(char *fname)
{
     SciErr sciErr;
     int iRet;

     int *piAddr = NULL;
     char* pstData;
     
     
     char* dOut = malloc(1024);

     CheckInputArgument(pvApiCtx, 1, 1) ;
     CheckOutputArgument(pvApiCtx, 1, 1) ;

     sciErr = getVarAddressFromPosition(pvApiCtx, 1, &piAddr);
     if (sciErr.iErr)
     {
          printError(&sciErr, 0);
          return 0;
     }
     
     if ( !isStringType(pvApiCtx, piAddr) )
     {
          Scierror(999, "%s: Wrong type for input argument #%d: A scalar expected.\n", fname, 1);
          return 0;
     }

     iRet = getAllocatedSingleString(pvApiCtx, piAddr, &pstData);
     if(iRet)
     {
          freeAllocatedSingleString(pstData);
          return iRet;
     }

     ctest(pstData, dOut);
     /* Scierror(999, "pstData = %s; dOut = %s\n", pstData, dOut); */
     
     iRet = createSingleString(pvApiCtx, nbInputArgument(pvApiCtx) + 1, dOut);
     if(iRet)
     {
          freeAllocatedSingleString(pstData);
          return iRet;
     }
     freeAllocatedSingleString(pstData);
     /* freeAllocatedSingleString(dOut); */
     
     AssignOutputVariable(pvApiCtx, 1) = nbInputArgument(pvApiCtx) + 1;

     ReturnArguments(pvApiCtx);

     return 0;

     
}

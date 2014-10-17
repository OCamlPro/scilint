#include "api_scilab.h"
#include <string.h>


/* ==================================================================== */
extern int cjit_load(void* pivApiCtx, char * filename);
/* ==================================================================== */

int sci_cjit_load(char *fname, void *pvApiCtx)
{
  SciErr sciErr;
  
  int iRet;
  int *piAddrArg = NULL;
  char* pstData;
  
  CheckInputArgument(pvApiCtx, 1, 1) ;
  CheckOutputArgument(pvApiCtx, 1, 1) ;
  
  /* get variable name which is 1st argument */
  sciErr = getVarAddressFromPosition(pvApiCtx, 1, &piAddrArg);
  if (sciErr.iErr)
    {
      printError(&sciErr, 0);
      return 0;
    }
  
  if ( !isStringType(pvApiCtx, piAddrArg) )
    {
      Scierror(999, 
	       "%s: Wrong type for input argument #%d: A string expected.\n", 
	       fname, 
	       1);
      return 0;
    }
  
  iRet = getAllocatedSingleString(pvApiCtx, piAddrArg, &pstData);
  if(iRet)
    {
      freeAllocatedSingleString(pstData);
      return iRet;
    }
  /* ============================================================== */
  
  return cjit_load(pvApiCtx, pstData);
}

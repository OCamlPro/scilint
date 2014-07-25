#include "api_scilab.h"

/* ==================================================================== */
extern int cjit(char * expr, char **vars, int * types, int* complexs, int** dims, int length);
/* ==================================================================== */

int sci_cjit(char *fname, void *pvApiCtx)
{
     SciErr sciErr;
     int *piAddrExpr = NULL;
     int *piAddrVars = NULL;
     int *piAddrVar = NULL;
     char* pstData;
     int iRet;
     int i,j;
     int iType;
     int iRows		= 0;
     int iCols		= 0;
     int dim1           = 0;
     int dim2           = 0;
     int* piLen		= NULL;
     char** strData	= NULL;
     int* types         = NULL;
     int* complexs      = NULL;
     int** dims         = NULL;
     char** pstOut	= NULL;

     CheckInputArgument(pvApiCtx, 2, 2);
     CheckOutputArgument(pvApiCtx, 0, 1);

     sciErr = getVarAddressFromPosition(pvApiCtx, 1, &piAddrExpr);
     if (sciErr.iErr) {
       printError(&sciErr, 0);
       return 0;
     }
     
     if ( !isStringType(pvApiCtx, piAddrExpr) ) {
       Scierror(999, "%s: Wrong type for input argument #%d: A string expected.\n", fname, 1);
       return 0;
     }
     
     iRet = getAllocatedSingleString(pvApiCtx, piAddrExpr, &pstData);
     if(iRet) {
       freeAllocatedSingleString(pstData);
       return iRet;
     }
     
     sciErr = getVarAddressFromPosition(pvApiCtx, 2, &piAddrVars);
     if (sciErr.iErr) {
       printError(&sciErr, 0);
       return 0;
     }

     if ( !isStringType(pvApiCtx, piAddrVars) ) {
       Scierror(999, "%s: Wrong type for input argument #%d: A string matrix expected.\n", fname, 2);
       return 0;
     }

     sciErr = getVarDimension(pvApiCtx, piAddrVars, &iRows, &iCols);
     if(sciErr.iErr) {
       Scierror(999, 
		"%s: can't get dimensions for the free variables.\n", 
		fname, 
		1);
       return 1;
     }

     if (iRows != 1 && iCols != 1) {
       Scierror(999,
     		"%s: 2nd arguments should be a vector.\n",
     		fname,
     		1);
       return 1;
     }

     piLen = (int*)malloc(sizeof(int) * iRows * iCols);

     sciErr = getMatrixOfString(pvApiCtx, 
                                piAddrVars, 
                                &iRows, 
                                &iCols, 
                                piLen, 
                                NULL);
     if(sciErr.iErr) {
       Scierror(999, 
		"%s: can't get strings length for the free variables.\n", 
		fname, 
		1);
       free(piLen);
       return 1;
     }

     strData = (char**)malloc(sizeof(char*) * (iRows * iCols + 1));

     for(i = 0 ; i < iRows * iCols ; i++) {
       strData[i] = (char*)malloc(sizeof(char) * (piLen[i] + 1));
     }
     strData[iRows * iCols] = (char*)malloc(sizeof(char));

     sciErr = getMatrixOfString(pvApiCtx, 
                                piAddrVars, 
                                &iRows, 
                                &iCols, 
                                piLen, 
                                strData);
     strData[iRows * iCols] = '\0';

     if(sciErr.iErr) {
       Scierror(999, 
		"%s: can't get strings value for %s.\n", 
		fname, 
		pstData,
		1);
       free(piLen);
       for(i = 0 ; i < iRows * iCols ; i++) {
	 free(strData[i]);
       }
       free(strData);
       return 1;
     }
     types = (int*)malloc(sizeof(int) * (iRows * iCols));
     //get_types_from_names(fname, pvApiCtx, strData, types, iRows * iCols);
     complexs = (int*)malloc(sizeof(int) * (iRows * iCols));
     //get_complex_from_names(fname, pvApiCtx, strData, complexs, iRows * iCols);
     dims = (int*)malloc(sizeof(int) * (iRows * iCols * 2));

     for (i = 0; i < iRows * iCols ; i++) {
       sciErr = getVarAddressFromName(pvApiCtx, strData[i], &piAddrVar);
       if (sciErr.iErr) {
	 sciprint("warning : variable \'%s\' not found.\n", strData[i]);
	 types[i] = -1;
	 complexs[i] = -1;
	 dims[i * 2] = -1;
	 dims[(i * 2) + 1] = -1;
	 continue;
       }
       sciErr = getVarType(pvApiCtx, piAddrVar, &iType);
       if(sciErr.iErr) {
	 sciprint("warning : can't type variable %s.\n", strData[i]);
	 types[i] = -1;
	 complexs[i] = -1;
	 dims[i * 2] = -1;
	 dims[(i * 2) + 1] = -1;
	 continue;
       }
       types[i] = get_camlint_from_scitype(iType); 
       complexs[i] = isVarComplex(pvApiCtx, piAddrVar);
       sciErr = getVarDimension(pvApiCtx, piAddrVar, &dim1, &dim2);
       if(sciErr.iErr) {
	 sciprint("warning : can't get dimension from variable %s.\n", strData[i]);
	 dims[i * 2] = -1;
	 dims[(i * 2) + 1] = -1;
	 continue;
       }
       dims[i * 2] = dim1;
       dims[(i * 2) + 1] = dim2;
       //printf("%s %d (%d * %d) %d", strData[i], iType, dims[i * 2], dims[(i * 2) + 1], complexs[i]);
     }
     cjit(pstData, strData, types, complexs, dims, iRows * iCols);

     free(piLen);
     for(i = 0 ; i < iRows * iCols ; i++)
     {
          free(strData[i]);
     }
     free(dims);
     free(types);
     free(complexs);
     free(strData);
     free(pstData);
     return 0;
}

int get_camlint_from_scitype(int scitype) {
  switch(scitype)
    {
    case sci_matrix : return 0;
    case sci_poly : return 1;
    case sci_boolean : return 2;
    case sci_sparse : return 3;
    case sci_boolean_sparse : return 4;
    case sci_ints : return 5;
    case sci_strings : return 6;
    case sci_list : return 7;
    case sci_tlist : return 8;
    case sci_mlist : return 9;
    default : return -1;
    }
}

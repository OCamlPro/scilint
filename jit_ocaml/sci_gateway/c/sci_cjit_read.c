#include "api_scilab.h"
#include <string.h>

int get_value(char* fname, void* pvApiCtx, char* pstData, int* _piAddr);

int sci_cjit_read(char* fname, void* pvApiCtx)
{
     SciErr sciErr;

     int iRet;
     int *piAddrArg = NULL;
     int *piAddrVar = NULL;
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

     /* get the address */
     sciErr = getVarAddressFromName(pvApiCtx, pstData, &piAddrVar);
     if (sciErr.iErr)
     {
          Scierror(999, "%s: variable %s not found.\n", fname, pstData, 1);
          return 0;
     }
     /* ============================================================= */
     
     /* print infos about var */
     iRet = get_value(fname, pvApiCtx, pstData, piAddrVar);
     if(iRet)
     {
          Scierror(999, 
                   "%s: can't get value for %s.\n", 
                   fname, 
                   pstData, 
                   1);
          return 0;
     }
     free(pstData);
     return 0;
}

int get_value_double(char *fname, void* pvApiCtx, char* pstData, int* piAddr){
     SciErr sciErr;
     int iRet;

     int complex = isVarComplex(pvApiCtx, piAddr);
     
     int iRows, iCols;
     
     double* dblValMat;
     double* dblValImgMat;

     sciErr = getVarDimension(pvApiCtx, piAddr, &iRows, &iCols);     
     
     if (complex)
     {
          sciErr = getComplexMatrixOfDouble(pvApiCtx, 
                                            piAddr, 
                                            &iRows, 
                                            &iCols, 
                                            &dblValMat, 
                                            &dblValImgMat);
          if(sciErr.iErr)
          {
               Scierror(999, "%s: can't get double value for %s.\n", fname, 1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;
          }
          sciErr = createComplexMatrixOfDouble(pvApiCtx, 
                                               nbInputArgument(pvApiCtx) + 1, 
                                               iRows, 
                                               iCols, 
                                               dblValMat, 
                                               dblValImgMat);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't return double value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;
          }
     }
     else 
     {
          sciErr = getMatrixOfDouble(pvApiCtx, 
                                     piAddr, 
                                     &iRows, 
                                     &iCols, 
                                     &dblValMat);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't get double value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;
          }
          sciErr = createMatrixOfDouble(pvApiCtx, 
                                        nbInputArgument(pvApiCtx) + 1, 
                                        iRows, 
                                        iCols, 
                                        dblValMat);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't return double value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;
          }
     }
     AssignOutputVariable(pvApiCtx, 1) = nbInputArgument(pvApiCtx) + 1;
     ReturnArguments(pvApiCtx);  
}

int get_value_poly(char *fname, void* pvApiCtx, char* pstData, int* piAddr){
     SciErr sciErr;

     int complex = isVarComplex(pvApiCtx, piAddr);
     
     int iRows, iCols;
     int *piNbCoef;
     double **pdblReal;
     double **pdblImg;
     
     int iVarLen = 0;
     int i;
     char* pstVarname;


     sciErr = getPolyVariableName(pvApiCtx, piAddr, NULL, &iVarLen);
     if(sciErr.iErr)
     {
          Scierror(999, 
                   "%s: can't get polynomial variable length for %s.\n", 
                   fname, 
                   pstData,
                   1);
          AssignOutputVariable(pvApiCtx, 1) = 0;
          return 0;
     }
     
     //alloc buff to receive variable name
     pstVarname = (char*)malloc(sizeof(char) * (iVarLen + 1));//1 for null termination
     //get variable name
     sciErr = getPolyVariableName(pvApiCtx, piAddr, pstVarname, &iVarLen);
     if(sciErr.iErr)
     {
          Scierror(999, 
                   "%s: can't get polynomial variable name for %s.\n", 
                   fname, 
                   pstData,
                   1);
          free(pstVarname);
          AssignOutputVariable(pvApiCtx, 1) = 0;
          return 0;  
     }

     if (!complex)
     {
          //First call: retrieve dimmension
          sciErr = getMatrixOfPoly(pvApiCtx, piAddr, &iRows, &iCols, NULL, NULL);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't get polynomial dimension for %s.\n", 
                        fname, 
                        pstData,
                        1);
               free(pstVarname);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 0;
          }
     
          //alloc array of coefficient
          piNbCoef = (int*)malloc(sizeof(int) * iRows * iCols);
          sciErr = getMatrixOfPoly(pvApiCtx, piAddr, &iRows, &iCols, piNbCoef, NULL);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't get polynomial coef for %s.\n", 
                        fname, 
                        pstData,
                        1);
               free(piNbCoef);
               free(pstVarname);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 0;
          }
          
          //alloc arrays of data
          pdblReal    = (double**)malloc(sizeof(double*) * iRows * iCols);
          for(i = 0 ; i < iRows * iCols ; i++)
          {
               pdblReal[i] = (double*)malloc(sizeof(double) * piNbCoef[i]);
          }
          //Third call: retrieve data
          sciErr = getMatrixOfPoly(pvApiCtx, piAddr, &iRows, &iCols, piNbCoef, pdblReal);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't return complex poly value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               
               free(piNbCoef);
               free(pstVarname);
               for(i = 0 ; i < iRows * iCols ; i++)
               {
                    free(pdblReal[i]);
               }
               free(pdblReal);
               
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;
          }
          sciErr = createMatrixOfPoly(pvApiCtx, 
                                      nbInputArgument(pvApiCtx) + 1, 
                                      pstVarname, 
                                      iRows, 
                                      iCols, 
                                      piNbCoef, 
                                      (const double * const*) pdblReal);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't return complex poly value for %s.\n", 
                        fname, 
                        pstData,
                        1);

               free(piNbCoef);
               free(pstVarname);
               for(i = 0 ; i < iRows * iCols ; i++)
               {
                    free(pdblReal[i]);
               }
               free(pdblReal);

               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;
          }
          for(i = 0 ; i < iRows * iCols ; i++)
          {
               free(pdblReal[i]);
          }
          free(pdblReal);
     }
     else 
     {
          //First call: retrieve dimmension
          sciErr = getComplexMatrixOfPoly(pvApiCtx, piAddr, &iRows, &iCols, NULL, NULL, NULL);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't get polynomial dimension for %s.\n", 
                        fname, 
                        pstData,
                        1);
               free(pstVarname);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 0;
          }
          
          //alloc array of coefficient
          piNbCoef = (int*)malloc(sizeof(int) * iRows * iCols);
          sciErr = getComplexMatrixOfPoly(pvApiCtx, piAddr, &iRows, &iCols, piNbCoef, NULL, NULL);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't get polynomial coef for %s.\n", 
                        fname, 
                        pstData,
                        1);
               free(piNbCoef);
               free(pstVarname);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 0;
          }
          
          //alloc arrays of data
          pdblReal    = (double**)malloc(sizeof(double*) * iRows * iCols);
          pdblImg     = (double**)malloc(sizeof(double*) * iRows * iCols);
          for(i = 0 ; i < iRows * iCols ; i++)
          {
               pdblReal[i] = (double*)malloc(sizeof(double) * piNbCoef[i]);
               pdblImg[i] = (double*)malloc(sizeof(double) * piNbCoef[i]);
          }
          
          //Third call: retrieve data
          sciErr = getComplexMatrixOfPoly(pvApiCtx, piAddr, &iRows, &iCols, piNbCoef, pdblReal, pdblImg);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't return complex poly value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               
               free(piNbCoef);
               free(pstVarname);
               for(i = 0 ; i < iRows * iCols ; i++)
               {
                    free(pdblReal[i]);
                    free(pdblImg[i]);
               }
               free(pdblReal);
               free(pdblImg);
               
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;
          }
          sciErr = createComplexMatrixOfPoly(pvApiCtx, 
                                             nbInputArgument(pvApiCtx) + 1, 
                                             pstVarname, 
                                             iRows, 
                                             iCols, 
                                             piNbCoef, 
                                             (const double * const*) pdblReal,
                                             (const double * const*) pdblImg);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't return complex poly value for %s.\n", 
                        fname, 
                        pstData,
                        1);

               free(piNbCoef);
               free(pstVarname);
               for(i = 0 ; i < iRows * iCols ; i++)
               {
                    free(pdblReal[i]);
                    free(pdblImg[i]);
               }
               free(pdblReal);
               free(pdblImg);

               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;
          }
          
          for(i = 0 ; i < iRows * iCols ; i++)
          {
               free(pdblImg[i]);
               free(pdblReal[i]);
          }
          free(pdblImg);
          free(pdblReal);
     }

     free(piNbCoef);
     free(pstVarname);
     
     AssignOutputVariable(pvApiCtx, 1) = nbInputArgument(pvApiCtx) + 1;
     ReturnArguments(pvApiCtx);  
}

int get_value_boolean(char *fname, void* pvApiCtx, char* pstData, int* piAddr){
     SciErr sciErr;
     int iRet;
     
     int iRows, iCols;
     
     int bVal;
     int* bValMat;

     sciErr = getMatrixOfBoolean(pvApiCtx, 
                                 piAddr, 
                                 &iRows, 
                                 &iCols, 
                                 &bValMat);
     if(sciErr.iErr)
     {
          Scierror(999, 
                   "%s: can't get boolean value for %s.\n", 
                   fname, 
                   pstData,
                   1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;
     }
     sciErr = createMatrixOfBoolean(pvApiCtx, 
                                    nbInputArgument(pvApiCtx) + 1, 
                                    iRows, 
                                    iCols, 
                                    bValMat);
     if(sciErr.iErr)
     {
          Scierror(999, 
                   "%s: can't return boolean value for %s.\n", 
                   fname, 
                   pstData,
                   1);
          AssignOutputVariable(pvApiCtx, 1) = 0;
          return 1;
     }
     AssignOutputVariable(pvApiCtx, 1) = nbInputArgument(pvApiCtx) + 1;
     ReturnArguments(pvApiCtx);  
}

int get_value_string(char *fname, void* pvApiCtx, char* pstData, int* piAddr)
{
     SciErr sciErr;
     int i,j;
     int iLeng		= 0;
     //variable info
     int iRows		= 0;
     int iCols		= 0;
     int* piLen		= NULL;
     char** strData	= NULL;
     //output variable
     char** pstOut	= NULL;
     
     sciErr = getVarDimension(pvApiCtx, piAddr, &iRows, &iCols);
     if(sciErr.iErr)
     {
          Scierror(999, 
                   "%s: can't get dimensions for %s.\n", 
                   fname, 
                   pstData,
                   1);
          AssignOutputVariable(pvApiCtx, 1) = 0;
          return 1;
     }
     
     piLen = (int*)malloc(sizeof(int) * iRows * iCols);
     
     //second call to retrieve length of each string
     sciErr = getMatrixOfString(pvApiCtx, 
                                piAddr, 
                                &iRows, 
                                &iCols, 
                                piLen, 
                                NULL);
     if(sciErr.iErr)
     {
          Scierror(999, 
                   "%s: can't get strings length for %s.\n", 
                   fname, 
                   pstData,
                   1);
          free(piLen);
          AssignOutputVariable(pvApiCtx, 1) = 0;
          return 1;
     }
     strData = (char**)malloc(sizeof(char*) * iRows * iCols);
     for(i = 0 ; i < iRows * iCols ; i++)
     {
          strData[i] = (char*)malloc(sizeof(char) * (piLen[i] + 1));
     }

     //third call to retrieve data
     sciErr = getMatrixOfString(pvApiCtx, 
                                piAddr, 
                                &iRows, 
                                &iCols, 
                                piLen, 
                                strData);
     if(sciErr.iErr)
     {
          Scierror(999, 
                   "%s: can't get strings value for %s.\n", 
                   fname, 
                   pstData,
                   1);
          free(piLen);
          for(i = 0 ; i < iRows * iCols ; i++)
          {
               free(strData[i]);
          }
          free(strData);
          AssignOutputVariable(pvApiCtx, 1) = 0;
          return 1;
     }
     
     //create new variable
     sciErr = createMatrixOfString(pvApiCtx, 
                                   nbInputArgument(pvApiCtx) + 1, 
                                   iRows, 
                                   iCols, 
                                   (const char * const*) strData);
     if(sciErr.iErr)
     {
          Scierror(999, 
                   "%s: can't get return strings value for %s.\n", 
                   fname, 
                   pstData,
                   1);
          free(piLen);
          for(i = 0 ; i < iRows * iCols ; i++)
          {
               free(strData[i]);
          }
          free(strData);
          AssignOutputVariable(pvApiCtx, 1) = 0;
          return 1;
     }
     
     //free memory
     free(piLen);
     
     for(i = 0 ; i < iRows * iCols ; i++)
     {
          free(strData[i]);
     }
     free(strData);
     AssignOutputVariable(pvApiCtx, 1) = nbInputArgument(pvApiCtx) + 1;
     ReturnArguments(pvApiCtx);
}

int get_value_sparse(char *fname, void* pvApiCtx, char* pstData, int* piAddr)
{
     SciErr sciErr;

     int complex = isVarComplex(pvApiCtx, piAddr);
     int iRows   = 0;
     int iCols   = 0;
     int iNbItem = 0;
     int* piNbItemRow = NULL;
     int* piColPos    = NULL;
     double* pdblReal = NULL;
     double* pdblImg  = NULL;
     
     if (complex)
     {
          sciErr = getComplexSparseMatrix(pvApiCtx, 
                                          piAddr, 
                                          &iRows, 
                                          &iCols, 
                                          &iNbItem, 
                                          &piNbItemRow, 
                                          &piColPos, 
                                          &pdblReal, 
                                          &pdblImg);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't get complex sparse value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1; 
          }
          sciErr = createComplexSparseMatrix(pvApiCtx, 
                                             nbInputArgument(pvApiCtx) + 1,
                                             iRows,
                                             iCols, 
                                             iNbItem, 
                                             piNbItemRow, 
                                             piColPos, 
                                             pdblReal, 
                                             pdblImg);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't create complex sparse value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;   
          }
     }
     else
     {
          sciErr = getSparseMatrix(pvApiCtx, 
                                   piAddr, 
                                   &iRows, 
                                   &iCols, 
                                   &iNbItem, 
                                   &piNbItemRow, 
                                   &piColPos, 
                                   &pdblReal);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't get sparse value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1; 
          }
          sciErr = createSparseMatrix(pvApiCtx, 
                                      nbInputArgument(pvApiCtx) + 1,
                                      iRows,
                                      iCols, 
                                      iNbItem, 
                                      piNbItemRow, 
                                      piColPos, 
                                      pdblReal);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't create sparse value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;   
          }
     }
     AssignOutputVariable(pvApiCtx, 1) = nbInputArgument(pvApiCtx) + 1;
     ReturnArguments(pvApiCtx);
}

int get_value_boolean_sparse(char *fname, void* pvApiCtx, char* pstData, int* piAddr)
{
     SciErr sciErr;

     int complex = isVarComplex(pvApiCtx, piAddr);
     int iRows   = 0;
     int iCols   = 0;
     int iNbItem = 0;
     int* piNbItemRow = NULL;
     int* piColPos    = NULL;
     double* pdblReal = NULL;
     double* pdblImg  = NULL;
     
     sciErr = getBooleanSparseMatrix(pvApiCtx, 
                              piAddr, 
                              &iRows, 
                              &iCols, 
                              &iNbItem, 
                              &piNbItemRow, 
                              &piColPos);
     if(sciErr.iErr)
     {
          Scierror(999, 
                   "%s: can't get sparse value for %s.\n", 
                   pstData,
                   fname, 
                        1);
          AssignOutputVariable(pvApiCtx, 1) = 0;
          return 1; 
     }
     sciErr = createBooleanSparseMatrix(pvApiCtx, 
                                        nbInputArgument(pvApiCtx) + 1,
                                        iRows,
                                        iCols, 
                                        iNbItem, 
                                        piNbItemRow, 
                                        piColPos);
     if(sciErr.iErr)
     {
          Scierror(999, 
                   "%s: can't create sparse value for %s.\n", 
                   pstData,
                   fname, 
                   1);
          AssignOutputVariable(pvApiCtx, 1) = 0;
          return 1;   
     }
     AssignOutputVariable(pvApiCtx, 1) = nbInputArgument(pvApiCtx) + 1;
     ReturnArguments(pvApiCtx);
}

int get_value_int(char *fname, void* pvApiCtx, char* pstData, int* piAddr)
{
     SciErr sciErr;
     int iPrec, iRows, iCols;
     char *pcData;
     short  *psData;
     int *piData;
     unsigned char* pucData;
     unsigned short* pusData;
     unsigned int* puiData;
     
     sciErr = getMatrixOfIntegerPrecision(pvApiCtx, piAddr, &iPrec);
     if(sciErr.iErr)
     {
          Scierror(999, 
                   "%s: can't get integer precision for %s.\n", 
                   fname, 
                   pstData,
                   1);
          AssignOutputVariable(pvApiCtx, 1) = 0;
          return 1;   
     }

     switch(iPrec)
     {
     case SCI_INT8 :
          sciErr = getMatrixOfInteger8(pvApiCtx, 
                                       piAddr, 
                                       &iRows, 
                                       &iCols, 
                                       &pcData);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't get integer8 value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;    
          }
          sciErr = createMatrixOfInteger8(pvApiCtx, 
                                          nbInputArgument(pvApiCtx) + 1, 
                                          iRows, 
                                          iCols, 
                                          pcData);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't create integer8 value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;   
          }
          break;
     case SCI_INT16 :
          sciErr = getMatrixOfInteger16(pvApiCtx, 
                                        piAddr, 
                                        &iRows, 
                                        &iCols, 
                                        &psData);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't get integer8 value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;    
          }
          sciErr = createMatrixOfInteger16(pvApiCtx, 
                                           nbInputArgument(pvApiCtx) + 1, 
                                           iRows, 
                                           iCols, 
                                           psData);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't create integer8 value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;   
          }
          break;
     case SCI_INT32 :
          sciErr = getMatrixOfInteger32(pvApiCtx, 
                                        piAddr, 
                                        &iRows, 
                                        &iCols, 
                                        &piData);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't get integer8 value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;    
          }
          sciErr = createMatrixOfInteger32(pvApiCtx, 
                                           nbInputArgument(pvApiCtx) + 1, 
                                           iRows, 
                                           iCols, 
                                           piData);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't create integer8 value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;   
          }
          break;
     case SCI_INT64 :
          Scierror(999, 
                   "%s: can't read integer64 %s.\n", 
                   fname, 
                   pstData,
                   1);
          AssignOutputVariable(pvApiCtx, 1) = 0;
          return 1;  
     case SCI_UINT8 :
          sciErr = getMatrixOfUnsignedInteger8(pvApiCtx, 
                                               piAddr, 
                                               &iRows, 
                                               &iCols, 
                                               &pucData);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't get uinteger8 value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;    
          }
          sciErr = createMatrixOfUnsignedInteger8(pvApiCtx, 
                                                  nbInputArgument(pvApiCtx) + 1, 
                                                  iRows, 
                                                  iCols, 
                                           pucData);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't create uinteger8 value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;   
          }
          break;  
     case SCI_UINT16 :
          sciErr = getMatrixOfUnsignedInteger16(pvApiCtx, 
                                                piAddr, 
                                                &iRows, 
                                                &iCols, 
                                                &pusData);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't get uinteger16 value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;    
          }
          sciErr = createMatrixOfUnsignedInteger16(pvApiCtx, 
                                                   nbInputArgument(pvApiCtx) + 1, 
                                                   iRows, 
                                                   iCols, 
                                                   pusData);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't create uinteger16 value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;   
          }
          break;
     case SCI_UINT32 :
          sciErr = getMatrixOfUnsignedInteger32(pvApiCtx, 
                                                piAddr, 
                                                &iRows, 
                                                &iCols, 
                                                &puiData);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't get uinteger32 value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;    
          }
          sciErr = createMatrixOfUnsignedInteger32(pvApiCtx, 
                                                   nbInputArgument(pvApiCtx) + 1, 
                                                   iRows, 
                                                   iCols, 
                                                   puiData);
          if(sciErr.iErr)
          {
               Scierror(999, 
                        "%s: can't create uinteger32 value for %s.\n", 
                        fname, 
                        pstData,
                        1);
               AssignOutputVariable(pvApiCtx, 1) = 0;
               return 1;   
          }
          break;
     case SCI_UINT64 :
          Scierror(999, 
                   "%s: can't read uinteger64 %s.\n", 
                   fname, 
                   pstData,
                   1);
          AssignOutputVariable(pvApiCtx, 1) = 0;
          return 1; 
          break;
     default :
          Scierror(999, 
                   "%s: unknown precision for %s.\n", 
                   fname, 
                   pstData,
                   1);
          AssignOutputVariable(pvApiCtx, 1) = 0;
          return 1;  
     }
     AssignOutputVariable(pvApiCtx, 1) = nbInputArgument(pvApiCtx) + 1;
     ReturnArguments(pvApiCtx);
}

int get_value(char *fname, void* pvApiCtx, char* pstData, int* piAddr)
{
     SciErr sciErr;
     int iType;

     sciErr = getVarType(pvApiCtx, piAddr, &iType);
     if(sciErr.iErr)
     {
          return 1;
     }

     switch(iType)
     {
     case sci_matrix :
          get_value_double(fname, pvApiCtx, pstData, piAddr);
          break;
     case sci_poly :
          get_value_poly(fname, pvApiCtx, pstData, piAddr);
          break;
     case sci_boolean :
          get_value_boolean(fname, pvApiCtx, pstData, piAddr);
          break;
     case sci_sparse :
          get_value_sparse(fname, pvApiCtx, pstData, piAddr);
          break;
     case sci_boolean_sparse :
          get_value_boolean_sparse(fname, pvApiCtx, pstData, piAddr);
          break;
     case sci_ints :
          get_value_int(fname, pvApiCtx, pstData, piAddr);
          break;
     case sci_strings :
          get_value_string(fname, pvApiCtx, pstData, piAddr);
          break;
     case sci_list :
          Scierror(999, "%s: can't get value for %s(sci_list).\n", fname, pstData, 1);
          break;
     case sci_tlist :
          Scierror(999, "%s: can't get value for %s(sci_tlist).\n", fname, pstData, 1);
          break;
     case sci_mlist :
          Scierror(999, "%s: can't get value for %s(sci_mlist).\n", fname, pstData, 1);
          break;
     default :
          Scierror(999, "%s: unknown type for %s.\n", fname, pstData, 1);
          return 1;
     }
    
     return 0;
}

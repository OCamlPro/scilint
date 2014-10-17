#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include "api_scilab.h"

/* Common */

CAMLprim value
caml_get_var_address_from_name(value ctx, value var_name)
{
  SciErr sciErr;
  CAMLparam2( ctx, var_name );
  CAMLlocal1( piAddr );
  sciErr = getVarAddressFromName((void *) ctx, String_val(var_name), (int **) &piAddr);
  
  if (sciErr.iErr)
    {
      /* Scierror(999, "variable \'%s\' not found.\n", String_val(str), 1); */
      CAMLreturn( caml_copy_nativeint(-1) );
    }
  
  CAMLreturn( piAddr );
}

CAMLprim value
caml_get_var_address_from_position(value ctx, value pos)
{
  SciErr sciErr;
  CAMLparam2( ctx, pos );
  CAMLlocal1( piAddr );
  sciErr = getVarAddressFromPosition((void *) ctx, Int_val(pos), (int **) &piAddr);
  
  if (sciErr.iErr)
    {
      Scierror(999, "variable #%i not found.\n", "", Int_val(pos), 1);
      CAMLreturn( caml_copy_nativeint(-1) );
    }
  
  CAMLreturn( piAddr );
}

CAMLprim value
caml_get_var_name_from_position(value ctx, value pos)
{
  SciErr sciErr;
  CAMLparam2( ctx, pos );
  char *psData;
  sciErr = getVarNameFromPosition((void *) ctx, Int_val(pos), psData);
  
  if (sciErr.iErr)
    {
      caml_failwith("can't get var name from position");
    }
  
  CAMLreturn( caml_copy_string(psData) );
}

CAMLprim value
caml_get_var_type(value ctx, value piaddr)
{
  SciErr sciErr;
  int iType;
  CAMLparam2( ctx, piaddr );
  sciErr = getVarType((void *) ctx, (int *) piaddr, (int *) &iType);
  
  if (sciErr.iErr)
    {
      Scierror(999, "can't get type.\n", 1);
      CAMLreturn(-1);
    }
  
  CAMLreturn( Val_int(iType) );
}

CAMLprim value
caml_get_named_var_type(value ctx, value var_name)
{
  SciErr sciErr;
  int iType;
  CAMLparam2( ctx, var_name );
  sciErr = getNamedVarType((void *) ctx, String_val(var_name), (int *) &iType);

  if (sciErr.iErr)
    {
      Scierror(999, "can't get type of variable %s.\n", String_val(var_name), 1);
      CAMLreturn(-1);
    }
  
  CAMLreturn( Val_int(iType) );
}

CAMLprim value
caml_get_var_dimension(value ctx, value piaddr)
{
  SciErr sciErr;
  int rows, cols;
  CAMLparam2( ctx, piaddr );
  CAMLlocal1( dim );
  dim = caml_alloc(2, 0);

  sciErr = getVarDimension((void *) ctx, (int *) piaddr, (int *) &rows, (int *) &cols);
  if(sciErr.iErr) {
      Scierror(999, "can't get dimemsion %s.\n", 1);
      Store_field(dim, 0, Val_int(-1));
      Store_field(dim, 1, Val_int(-1));
      CAMLreturn(dim);
  }

  Field(dim, 0) = Val_int(rows);
  Field(dim, 1) = Val_int(cols);
  /* res = Val_int(0); */
  CAMLreturn( dim );
}

CAMLprim value
caml_get_named_var_dimension(value ctx, value var_name)
{
  SciErr sciErr;
  int rows, cols;
  CAMLparam2( ctx, var_name );
  CAMLlocal1( dim );
  dim = caml_alloc(2, 0);

  sciErr = getNamedVarDimension((void *) ctx, String_val(var_name), (int *) &rows, (int *) &cols);

  if(sciErr.iErr)
    {
      Scierror(999, "can't get dimemsion %s.\n", 1);
      Store_field(dim, 0, -1);
      Store_field(dim, 1, -1);
      CAMLreturn(dim);
    }

  Field(dim, 0) = Val_int(rows);
  Field(dim, 1) = Val_int(cols);
  CAMLreturn(dim);
}

CAMLprim value
caml_increase_val_ref(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int ret;
  ret = increaseValRef((void *) ctx, (int *) piaddr);
  CAMLreturn( Val_bool(ret) );
}

CAMLprim value
caml_decrease_val_ref(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int ret;
  ret = decreaseValRef((void *) ctx, (int *) piaddr);
  CAMLreturn( Val_bool(ret) );
}

CAMLprim value
caml_check_input_argument(value ctx, value min, value max)
{
  CAMLparam3( ctx, min, max );
  int bool;
  bool = checkInputArgument((void *) ctx, Int_val(min), Int_val(max));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_check_input_argument_at_least(value ctx, value min)
{
  CAMLparam2( ctx, min );
  int bool;
  bool = checkInputArgumentAtLeast((void *) ctx, Int_val(min));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_check_input_argument_at_most(value ctx, value max)
{
  CAMLparam2( ctx, max );
  int bool;
  bool = checkInputArgumentAtMost((void *) ctx, Int_val(max));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_check_output_argument(value ctx, value min, value max)
{
  CAMLparam3( ctx, min, max );
  int bool;
  bool = checkOutputArgument((void *) ctx, Int_val(min), Int_val(max));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_check_output_argument_at_least(value ctx, value min)
{
  CAMLparam2( ctx, min );
  int bool;
  bool = checkOutputArgumentAtLeast((void *) ctx, Int_val(min));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_check_output_argument_at_most(value ctx, value max)
{
  CAMLparam2( ctx, max );
  int bool;
  bool = checkOutputArgumentAtMost((void *) ctx, Int_val(max));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_get_nb_input_argument(value ctx)
{
  CAMLparam1( ctx );
  int nb;
  nb = getNbInputArgument((void *) ctx);
  CAMLreturn( Val_int(nb) );
}

CAMLprim value
caml_get_nb_output_argument(value ctx)
{
  CAMLparam1( ctx );
  int nb;
  nb = getNbOutputArgument((void *) ctx);
  CAMLreturn( Val_int(nb) );
}

CAMLprim value
caml_return_arguments(value ctx)
{
  CAMLparam1( ctx );
  int nb;
  nb = returnArguments((void *) ctx);
  CAMLreturn( Val_int(nb) );
}

CAMLprim value
caml_call_overload_function(value ctx, value ivar, value fname, value length)
{
  CAMLparam4( ctx, ivar, fname, length );
  int res;
  res = callOverloadFunction((void *) ctx, Int_val(ivar), String_val(fname), Unsigned_int_val(length));
  CAMLreturn( res );
}

CAMLprim value
caml_call_scilab_function(value ctx, value fname, value start, value nbret, value nbargs)
{
  CAMLparam5( ctx, fname, start, nbret, nbargs );
  int res;
  res = callScilabFunction((void *) ctx, String_val(fname), Int_val(start), Int_val(nbret), Int_val(nbargs));
  CAMLreturn( res );
}

CAMLprim value
caml_is_named_var_exist(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int bool;
  bool = isNamedVarExist((void *) ctx, String_val(var_name));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_delete_named_variable(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int del;
  del = deleteNamedVariable((void *) ctx, String_val(var_name));
  CAMLreturn( Val_bool(del) );
}

CAMLprim value
caml_is_var_complex(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int bool;
  bool = isVarComplex((void *) ctx, (int *) piaddr);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_is_named_var_complex(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int bool;
  bool = isNamedVarComplex((void *) ctx, String_val(var_name));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_is_var_matrix_type(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int bool;
  bool = isVarMatrixType((void *) ctx, (int *) piaddr);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_is_named_var_matrix_type(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int bool;
  bool = isNamedVarMatrixType((void *) ctx, String_val(var_name));
  CAMLreturn(Val_bool(bool));
}

CAMLprim value
caml_is_row_vector(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int bool;
  bool = isRowVector((void *) ctx, (int *) piaddr);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_is_named_row_vector(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int bool;
  bool = isNamedRowVector((void *) ctx, String_val(var_name));
  CAMLreturn( Val_bool(bool) );
}


CAMLprim value
caml_is_column_vector(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int bool;
  bool = isColumnVector((void *) ctx, (int *) piaddr);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_is_named_column_vector(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int bool;
  bool = isNamedColumnVector((void *) ctx, String_val(var_name));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_is_vector(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int bool;
  bool = isVector((void *) ctx, (int *) piaddr);
  CAMLreturn(Val_bool(bool));
}

CAMLprim value
caml_is_named_vector(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int bool;
  bool = isNamedVector((void *) ctx, String_val(var_name));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_is_scalar(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int bool;
  bool = isScalar((void *) ctx, (int *) piaddr);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_is_named_scalar(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int bool;
  bool = isNamedScalar((void *) ctx, String_val(var_name));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_is_square_matrix(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int bool;
  bool = isSquareMatrix((void *) ctx, (int *) piaddr);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_is_named_square_matrix(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int bool;
  bool = isNamedSquareMatrix((void *) ctx, String_val(var_name));
  CAMLreturn( Val_bool(bool) );
}


CAMLprim value
caml_is_empty_matrix(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int bool;
  bool = isEmptyMatrix((void *) ctx, (int *) piaddr);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_is_named_empty_matrix(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int bool;
  bool = isNamedEmptyMatrix((void *) ctx, String_val(var_name));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_create_empty_matrix(value ctx, value pos)
{
  CAMLparam2( ctx, pos );
  int bool;
  bool = createEmptyMatrix((void *) ctx, Int_val(pos));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_create_named_empty_matrix(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int bool;
  bool = createNamedEmptyMatrix((void *) ctx, String_val(var_name));
  CAMLreturn( Val_bool(bool) );
}

/* String */

CAMLprim value
caml_is_string_type(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int bool;
  bool = isStringType((void *) ctx, (int *) piaddr);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_is_named_string_type(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int bool;
  bool = isNamedStringType((void *) ctx, String_val(var_name));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_get_allocated_single_string(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int res;
  char *psData;
  res = getAllocatedSingleString((void *) ctx, (int *) piaddr, &psData);
  CAMLreturn( caml_copy_string(psData) );
}

CAMLprim value
caml_get_allocated_named_single_string(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int res;
  char *psData;
  res = getAllocatedNamedSingleString((void *) ctx, String_val(var_name), &psData);
  CAMLreturn( caml_copy_string(psData) );
}

CAMLprim value
caml_create_single_string(value ctx, value pos, value str)
{
  CAMLparam3( ctx, pos, str);
  int res;
  res = createSingleString((void *) ctx, Int_val(pos), String_val(str));
  CAMLreturn( res );
}

CAMLprim value
caml_create_named_single_string(value ctx, value var_name, value str)
{
  CAMLparam3( ctx, var_name, str);
  int res;
  res = createNamedSingleString((void *) ctx, String_val(var_name), String_val(str));
  CAMLreturn( res );
}

CAMLprim value
caml_get_matrix_of_string(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  CAMLlocal1( str_array );
  SciErr sciErr;
  int i,j;
  //variable info
  int iRows		= 0;
  int iCols		= 0;
  int* piLen		= NULL;
  char** strData	= NULL;
  sciErr = getVarDimension((void *) ctx, (int *) piaddr, &iRows, &iCols);
  if(sciErr.iErr)
    {
      Scierror(999, "can't get dimensions.\n", 1);
      return 1;
    }
  
  piLen = (int*)malloc(sizeof(int) * iRows * iCols);

  //second call to retrieve length of each string
  sciErr = getMatrixOfString((void *) ctx, 
			     (int *) piaddr, 
			     &iRows, 
			     &iCols, 
			     piLen, 
			     NULL);
  if(sciErr.iErr)
    {
          Scierror(999, "can't get strings length.\n", 1);
          free(piLen);
          return 1;
    }

  strData = (char**)malloc(sizeof(char*) * iRows * iCols + 1);
  for(i = 0 ; i < iRows * iCols ; i++)
    {
      strData[i] = (char*)malloc(sizeof(char) * (piLen[i] + 1));
    }
  strData[iRows * iCols] = (char*)malloc(sizeof(char));

  //third call to retrieve data
  sciErr = getMatrixOfString((void *) ctx, 
			     (int *) piaddr, 
			     &iRows, 
			     &iCols, 
			     piLen, 
			     strData);
  strData[iRows * iCols] = NULL;
  
  if(sciErr.iErr)
    {
      Scierror(999, "can't get strings value.\n", 1);
      free(piLen);
      for(i = 0 ; i < iRows * iCols ; i++)
	{
	  free(strData[i]);
	}
      free(strData);
      return 1;
    }

  str_array = caml_copy_string_array((const char **) strData);

  //free memory
  free(piLen);
  
  for(i = 0 ; i < iRows * iCols ; i++)
    {
      free(strData[i]);
    }
  free(strData);
  CAMLreturn( str_array );
    
}

CAMLprim value
caml_get_named_matrix_of_string(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  CAMLlocal1( str_array );
  SciErr sciErr;
  int i,j;
  //variable info
  int iRows		= 0;
  int iCols		= 0;
  int* piLen		= NULL;
  char** strData	= NULL;
  char* varName = String_val(var_name);
  
  sciErr = getNamedVarDimension((void *) ctx, varName, &iRows, &iCols);
  if(sciErr.iErr)
    {
      Scierror(999, "can't get dimensions.\n", 1);
      return 1;
    }
  piLen = (int*)malloc(sizeof(int) * iRows * iCols + 1);
  
  //second call to retrieve length of each string
  sciErr = readNamedMatrixOfString((void *) ctx, 
			     varName, 
			     &iRows, 
			     &iCols, 
			     piLen, 
			     NULL);
  if(sciErr.iErr)
    {
      Scierror(999, "can't get strings length.\n", 1);
      free(piLen);
      return 1;
    }
  strData = (char**)malloc(sizeof(char*) * iRows * iCols + 1);
  for(i = 0 ; i < iRows * iCols ; i++)
    {
      strData[i] = (char*)malloc(sizeof(char) * (piLen[i] + 1));
    }
  strData[iRows * iCols] = (char*)malloc(sizeof(char));
  
  //third call to retrieve data
  sciErr = readNamedMatrixOfString((void *) ctx, 
				   varName, 
				   &iRows, 
				   &iCols, 
				   piLen, 
				   strData);
  strData[iRows * iCols] = NULL;
  
  if(sciErr.iErr)
    {
      Scierror(999, "can't get strings value.\n", 1);
      free(piLen);
      for(i = 0 ; i < iRows * iCols ; i++)
	{
	  free(strData[i]);
	}
      free(strData);
      return 1;
    }

  str_array = caml_copy_string_array((const char **) strData);
  
  //free memory
  free(piLen);
  
  for(i = 0 ; i < iRows * iCols ; i++)
    {
      free(strData[i]);
    }
  free(strData);
  CAMLreturn( str_array );
    
}

CAMLprim value
caml_create_matrix_of_string(value ctx, value pos, value rows, value cols, value str_arr)
{
  CAMLparam5( ctx, pos, rows, cols, str_arr );
  SciErr sciErr;
  int iRows = Int_val(rows);
  int iCols = Int_val(cols);
  int len = iRows * iCols;
  int strlen, i;
  char ** strData = (char**)malloc(sizeof(char*) * len);
  for (i=0; i < len; i++)
    {
      strlen = caml_string_length(Field(str_arr, i));
      strData[i] = (char*)malloc(sizeof(char) * strlen);
      strData[i] = String_val(Field(str_arr, i));
    }
  sciErr = createMatrixOfString((void *) ctx, 
				Int_val(pos), 
				iRows, 
				iCols, 	      
				(const char * const*) strData);
  
  if(sciErr.iErr)
    {
      Scierror(999, "can't create matrix of string.\n", 1);
      CAMLreturn(Val_int(1));
    }
  CAMLreturn(Val_int(0));
}

CAMLprim value
caml_create_named_matrix_of_string(value ctx, value var_name, value rows, value cols, value str_arr)
{
  CAMLparam5( ctx, var_name, rows, cols, str_arr );
  SciErr sciErr;
  int iRows = Int_val(rows);
  int iCols = Int_val(cols);
  int len = iRows * iCols;
  int strlen, i;
  char ** strData = (char**)malloc(sizeof(char*) * len);

  for (i=0; i < len; i++)
    {
      strlen = caml_string_length(Field(str_arr, i));
      strData[i] = (char*)malloc(sizeof(char) * strlen);
      strData[i] = String_val(Field(str_arr, i));
    }

  sciErr = createNamedMatrixOfString((void *) ctx, 
				     String_val(var_name), 
				     iRows, 
				     iCols, 	      
				     (const char * const*) strData);
  if(sciErr.iErr)
    {
      Scierror(999, "can't create matrix of string.\n", 1);
      CAMLreturn(Val_int(1));
    }
  CAMLreturn(Val_int(0));
}

/* Double */

CAMLprim value
caml_is_double_type(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int bool;
  bool = isDoubleType((void *) ctx, (int *) piaddr);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_is_named_double_type(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int bool;
  bool = isNamedDoubleType((void *) ctx, String_val(var_name));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_get_scalar_double(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  double dbl;
  int res;
  res = getScalarDouble((void *) ctx, (int *) piaddr, &dbl);
  if (res) CAMLreturn( caml_copy_double(-1) );
  CAMLreturn( caml_copy_double(dbl) );
}

CAMLprim value
caml_get_named_scalar_double(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  double dbl;
  int res;
  res = getNamedScalarDouble((void *) ctx, String_val(var_name), &dbl);
  if (res) CAMLreturn( caml_copy_double(-1) );
  CAMLreturn( caml_copy_double(dbl) );
}

CAMLprim value
caml_get_scalar_complex_double(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  CAMLlocal1( val_ret );
  val_ret = caml_alloc(2, 0);
  double real, complex;
  int res;
  res = getScalarComplexDouble((void *) ctx, (int *) piaddr, &real, &complex);
  if (res) {
    Store_field(val_ret, 0, caml_copy_double(-1));
    Store_field(val_ret, 1, caml_copy_double(-1));
    CAMLreturn( val_ret );
  }
  Store_field(val_ret, 0, caml_copy_double(real));
  Store_field(val_ret, 1, caml_copy_double(complex));
  CAMLreturn( val_ret );
}

CAMLprim value
caml_get_named_scalar_complex_double(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  CAMLlocal1( val_ret );
  val_ret = caml_alloc(2, 0);
  double real, complex;
  int res;
  res = getNamedScalarComplexDouble((void *) ctx, String_val(var_name), &real, &complex);
  if (res) {
    Store_field(val_ret, 0, caml_copy_double(-1));
    Store_field(val_ret, 1, caml_copy_double(-1));
    CAMLreturn( val_ret );
  }
  Store_field(val_ret, 0, caml_copy_double(real));
  Store_field(val_ret, 1, caml_copy_double(complex));
  CAMLreturn( val_ret );
}

CAMLprim value
caml_create_scalar_double(value ctx, value pos, value dbl)
{
  CAMLparam3( ctx, pos, dbl );
  int res;
  res = createScalarDouble((void *) ctx, Int_val(pos), Double_val(dbl));
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_create_named_scalar_double(value ctx, value var_name, value dbl)
{
  CAMLparam3( ctx, var_name, dbl );
  int res;
  res = createNamedScalarDouble((void *) ctx, String_val(var_name), Double_val(dbl));
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_create_scalar_complex_double(value ctx, value pos, value real, value complex)
{
  CAMLparam4( ctx, pos, real, complex );
  int res;
  res = createScalarComplexDouble((void *) ctx, Int_val(pos), Double_val(real), Double_val(complex));
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_create_named_scalar_complex_double(value ctx, value var_name, value real, value complex)
{
  CAMLparam4( ctx, var_name, real, complex );
  int res;
  res = createNamedScalarComplexDouble((void *) ctx, String_val(var_name), Double_val(real), Double_val(complex));
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_get_matrix_of_double(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  CAMLlocal1( float_arr );
  SciErr sciErr;
  
  int i, iRows, iCols;
  double* dblValMat;
  
  sciErr = getVarDimension((void *) ctx, (int *) piaddr, &iRows, &iCols);
  
  if(sciErr.iErr)
    {
      Scierror(999, "can't get dimensions.\n", 1);
      return 1;
    }
  
  float_arr = caml_alloc((iRows * iCols) * Double_wosize, Double_array_tag);
  
  sciErr = getMatrixOfDouble((void *) ctx, 
			     (int *) piaddr, 
			     &iRows, 
			     &iCols, 
			     dblValMat);

  if(sciErr.iErr)
    {
      Scierror(999, "can't get matrix of double.\n", 1);
      return 1;
    }

  for(i = 0; i < iRows * iCols; i++){
    Store_double_field(float_arr, i, dblValMat[i]);
  }

  CAMLreturn( float_arr );
}

CAMLprim value
caml_get_named_matrix_of_double(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  CAMLlocal1( float_arr );
  SciErr sciErr;
  
  int i, iRows, iCols;
  double* dblValMat;
  sciErr = getNamedVarDimension((void *) ctx, String_val(var_name), &iRows, &iCols);
  
  if(sciErr.iErr)
    {
      Scierror(999, "can't get dimensions.\n", 1);
      return 1;
    }
  float_arr = caml_alloc((iRows * iCols) * Double_wosize, Double_array_tag);
  /* dblValMat = (double *)malloc(sizeof(double) * iRows * iCols); */
  sciErr = readNamedMatrixOfDouble((void *) ctx, 
				   String_val(var_name), 
				   &iRows, 
				   &iCols, 
				   dblValMat);
  if(sciErr.iErr)
    {
      Scierror(999, "can't get matrix of double.\n", 1);
      return 1;
    }

  for(i = 0; i < iRows * iCols; i++){
    Store_double_field(float_arr, i, dblValMat[i]);
  }
  CAMLreturn( float_arr );
}

CAMLprim value
caml_get_complex_matrix_of_double(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  CAMLlocal3( real_arr, complex_arr, res_arr );
  SciErr sciErr;
  
  int i, iRows, iCols;
  double* realValMat;
  double* complValMat;
  
  sciErr = getVarDimension((void *) ctx, (int *) piaddr, &iRows, &iCols);
  
  if(sciErr.iErr)
    {
      Scierror(999, "can't get dimensions.\n", 1);
      return 1;
    }
  
  real_arr = caml_alloc((iRows * iCols) * Double_wosize, Double_array_tag);
  complex_arr = caml_alloc((iRows * iCols) * Double_wosize, Double_array_tag);
  res_arr = caml_alloc(2, 0);
  realValMat = (double *)malloc(sizeof(double) * (iRows * iCols));
  complValMat = (double *)malloc(sizeof(double) * (iRows * iCols));

  sciErr = getComplexMatrixOfDouble((void *) ctx, 
				    (int *) piaddr, 
				    &iRows, 
				    &iCols, 
				    realValMat,
				    complValMat);

  if(sciErr.iErr)
    {
      Scierror(999, "can't get matrix of double.\n", 1);
      return 1;
    }

  for(i = 0; i < iRows * iCols; i++){
    Store_double_field(real_arr, i, realValMat[i]);
    Store_double_field(complex_arr, i, complValMat[i]);
  }
  Store_field(res_arr, 0, real_arr);
  Store_field(res_arr, 1, complex_arr);
  
  CAMLreturn( res_arr );
}

CAMLprim value
caml_get_named_complex_matrix_of_double(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  CAMLlocal3( real_arr, complex_arr, res_arr );
  SciErr sciErr;
  
  int i, iRows, iCols;
  double* realValMat;
  double* complValMat;

  sciErr = getNamedVarDimension((void *) ctx, String_val(var_name), &iRows, &iCols);
  
  if(sciErr.iErr)
    {
      Scierror(999, "can't get dimensions.\n", 1);
      return 1;
    }

  real_arr = caml_alloc((iRows * iCols) * Double_wosize, Double_array_tag);
  complex_arr = caml_alloc((iRows * iCols) * Double_wosize, Double_array_tag);
  res_arr = caml_alloc(2, 0);
  realValMat = (double *)malloc(sizeof(double) * (iRows * iCols));
  complValMat = (double *)malloc(sizeof(double) * (iRows * iCols));

  sciErr = readNamedComplexMatrixOfDouble((void *) ctx, 
					  String_val(var_name),
					  &iRows, 
					  &iCols, 
					  realValMat,
					  complValMat);

  if(sciErr.iErr)
    {
      Scierror(999, "can't get matrix of double.\n", 1);
      return 1;
    }
  
  for(i = 0; i < iRows * iCols; i++){
    Store_double_field(real_arr, i, realValMat[i]);
    Store_double_field(complex_arr, i, complValMat[i]);
  }
  Store_field(res_arr, 0, real_arr);
  Store_field(res_arr, 1, complex_arr);

  CAMLreturn( res_arr );
}

CAMLprim value
caml_create_matrix_of_double(value ctx, value pos, value rows, value cols, value dbl_arr)
{
  CAMLparam5( ctx, pos, rows, cols, dbl_arr );
  SciErr sciErr;
  int iRows = Int_val(rows);
  int iCols = Int_val(cols);
  int len = iRows * iCols;
  int i;
  double* dblValMat = (double *)malloc(sizeof(double) * len);

  for (i=0; i < len; i++) dblValMat[i] = Double_field(dbl_arr, i);

  sciErr = createMatrixOfDouble((void *) ctx, 
				Int_val(pos), 
				iRows, 
				iCols, 	      
				dblValMat);
  free(dblValMat);
  
  if(sciErr.iErr)
    {
      Scierror(999, "can't create matrix of string.\n", 1);
      CAMLreturn(Val_int(1));
    }
    
  CAMLreturn(Val_int(0));
}

CAMLprim value
caml_create_named_matrix_of_double(value ctx, value var_name, value rows, value cols, value dbl_arr)
{
  CAMLparam5( ctx, var_name, rows, cols, dbl_arr );
  SciErr sciErr;
  int iRows = Int_val(rows);
  int iCols = Int_val(cols);
  int len = iRows * iCols;
  int i;
  double* dblValMat = (double *)malloc(sizeof(double) * len);

  for (i=0; i < len; i++) dblValMat[i] = Double_field(dbl_arr, i);

  sciErr = createNamedMatrixOfDouble((void *) ctx, 
				     String_val(var_name), 
				     iRows, 
				     iCols, 	      
				     dblValMat);

  if(sciErr.iErr)
    {
      Scierror(999, "can't create matrix of string.\n", 1);
      CAMLreturn(Val_int(1));
    }
    
  CAMLreturn(Val_int(0));
}

CAMLprim value
caml_create_complex_matrix_of_double(value ctx, value pos, value dim, value real_arr, value compl_arr)
{
  CAMLparam5( ctx, pos, dim, real_arr, compl_arr );
  SciErr sciErr;
  int iRows = Int_val(Field(dim, 0));
  int iCols = Int_val(Field(dim, 1));
  int len = iRows * iCols;
  int i;
  double* realValMat = (double *)malloc(sizeof(double) * len);
  double* complValMat = (double *)malloc(sizeof(double) * len);
  for (i=0; i < len; i++) {
    realValMat[i] = Double_field(real_arr, i);
    complValMat[i] = Double_field(compl_arr, i);
  }
  
  sciErr = createComplexMatrixOfDouble((void *) ctx, 
				       Int_val(pos), 
				       iRows, 
				       iCols, 	      
				       realValMat,
				       complValMat);
  free(realValMat);
  free(complValMat);
  
  if(sciErr.iErr)
    {
      Scierror(999, "can't create matrix of string.\n", 1);
      CAMLreturn(Val_int(1));
    }
    
  CAMLreturn(Val_int(0));
}

CAMLprim value
caml_create_named_complex_matrix_of_double(value ctx, value var_name, value dim, value real_arr, value compl_arr)
{
  CAMLparam5( ctx, var_name, dim, real_arr, compl_arr);
  SciErr sciErr;
  int iRows = Int_val(Field(dim, 0));
  int iCols = Int_val(Field(dim, 1));
  int len = iRows * iCols;
  int i;
  double* realValMat = (double *)malloc(sizeof(double) * len);
  double* complValMat = (double *)malloc(sizeof(double) * len);
  
  for (i=0; i < len; i++) {
    realValMat[i] = Double_field(real_arr, i);
    complValMat[i] = Double_field(compl_arr, i);
  }
  
  sciErr = createNamedComplexMatrixOfDouble((void *) ctx, 
					    String_val(var_name), 
					    iRows, 
					    iCols, 	      
					    realValMat,
					    complValMat);
  free(realValMat);
  free(complValMat);
  if(sciErr.iErr)
    {
      Scierror(999, "can't create matrix of string.\n", 1);
      CAMLreturn(Val_int(1));
    }
    
  CAMLreturn(Val_int(0));
}


/* Boolean */

CAMLprim value
caml_is_boolean_type(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int bool;
  bool = isBooleanType((void *) ctx, (int *) piaddr);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_is_named_boolean_type(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int bool;
  bool = isNamedBooleanType((void *) ctx, String_val(var_name));
  CAMLreturn( Val_bool(bool) );
}


CAMLprim value
caml_get_scalar_boolean(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  int bool;
  int res;
  res = getScalarDouble((void *) ctx, (int *) piaddr, &bool);
  if (res) CAMLreturn( caml_copy_double(-1) );
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_get_named_scalar_boolean(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  int bool;
  int res;
  res = getNamedScalarDouble((void *) ctx, String_val(var_name), &bool);
  if (res) CAMLreturn( caml_copy_double(-1) );
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_create_scalar_boolean(value ctx, value pos, value bool)
{
  CAMLparam3( ctx, pos, bool );
  int res;
  res = createScalarBoolean((void *) ctx, Int_val(pos), Bool_val(bool));
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_create_named_scalar_boolean(value ctx, value var_name, value bool)
{
  CAMLparam3( ctx, var_name, bool );
  int res;
  res = createNamedScalarBoolean((void *) ctx, String_val(var_name), Bool_val(bool));
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_get_matrix_of_boolean(value ctx, value piaddr)
{
  CAMLparam2( ctx, piaddr );
  CAMLlocal1( bool_arr );
  SciErr sciErr;
  
  int i, iRows, iCols;
  int* boolValMat;
  
  sciErr = getVarDimension((void *) ctx, (int *) piaddr, &iRows, &iCols);
  
  if(sciErr.iErr)
    {
      Scierror(999, "can't get dimensions.\n", 1);
      return 1;
    }
  
  bool_arr = caml_alloc((iRows * iCols), 0);
  
  sciErr = getMatrixOfBoolean((void *) ctx, 
			      (int *) piaddr, 
			      &iRows, 
			      &iCols, 
			      &boolValMat);

  if(sciErr.iErr)
    {
      Scierror(999, "can't get matrix of double.\n", 1);
      return 1;
    }

  for(i = 0; i < iRows * iCols; i++){
    Store_double_field(bool_arr, i, boolValMat[i]);
  }

  CAMLreturn( bool_arr );
}

CAMLprim value
caml_get_named_matrix_of_boolean(value ctx, value var_name)
{
  CAMLparam2( ctx, var_name );
  CAMLlocal1( bool_arr );
  SciErr sciErr;
  
  int i, iRows, iCols;
  int* boolValMat;
  sciErr = getNamedVarDimension((void *) ctx, String_val(var_name), &iRows, &iCols);
  
  if(sciErr.iErr)
    {
      Scierror(999, "can't get dimensions.\n", 1);
      return 1;
    }
  bool_arr = caml_alloc((iRows * iCols), 0);

  sciErr = readNamedMatrixOfBoolean((void *) ctx, 
				    String_val(var_name), 
				    &iRows, 
				    &iCols, 
				    &boolValMat);
  if(sciErr.iErr)
    {
      Scierror(999, "can't get matrix of double.\n", 1);
      return 1;
    }

  for(i = 0; i < iRows * iCols; i++){
    Store_double_field(bool_arr, i, boolValMat[i]);
  }
  CAMLreturn( bool_arr );
}

CAMLprim value
caml_create_matrix_of_boolean(value ctx, value pos, value rows, value cols, value bool_arr)
{
  CAMLparam5( ctx, pos, rows, cols, bool_arr );
  SciErr sciErr;
  int iRows = Int_val(rows);
  int iCols = Int_val(cols);
  int len = iRows * iCols;
  int i;
  int* boolValMat = (int *)malloc(sizeof(int) * len);

  for (i=0; i < len; i++) boolValMat[i] = Int_val(Field(bool_arr, i));

  sciErr = createMatrixOfBoolean((void *) ctx, 
				 Int_val(pos), 
				 iRows, 
				 iCols, 	      
				 boolValMat);
  free(boolValMat);
  
  if(sciErr.iErr)
    {
      Scierror(999, "can't create matrix of string.\n", 1);
      CAMLreturn(Val_int(1));
    }
    
  CAMLreturn(Val_int(0));
}

CAMLprim value
caml_create_named_matrix_of_boolean(value ctx, value var_name, value rows, value cols, value bool_arr)
{
  CAMLparam5( ctx, var_name, rows, cols, bool_arr );
  SciErr sciErr;
  int iRows = Int_val(rows);
  int iCols = Int_val(cols);
  int len = iRows * iCols;
  int i;
  int* boolValMat = (int *)malloc(sizeof(int) * len);

  for (i=0; i < len; i++) boolValMat[i] = Int_val(Field(bool_arr, i));

  sciErr = createNamedMatrixOfDouble((void *) ctx, 
				     String_val(var_name), 
				     iRows, 
				     iCols, 	      
				     boolValMat);

  free(boolValMat);

  if(sciErr.iErr)
    {
      Scierror(999, "can't create matrix of string.\n", 1);
      CAMLreturn(Val_int(1));
    }
    
  CAMLreturn(Val_int(0));
}

/* (U)INT8 (U)INT16, INT OCAML */
/* (U)INT32, INT32 OCAML */

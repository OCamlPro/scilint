#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include "api_scilab.h"

value Val_abstract(const void* ptr)
{
  value ptr_v = caml_alloc_small(1, Abstract_tag);
  Field(ptr_v, 0) = ptr;
  return ptr_v;
}

void* Abstract_val(value ptr_v)
{      return (void*)Field(ptr_v, 0);   }

#define Type_piaddr_val Abstract_val
#define Type_var_name_val String_val

typedef char var_name_type;
typedef void piaddr_type;

#define BEGIN_SCILAB_STUB(name, ctx, var)	    \
   CAMLprim value name(value ctx##_v, value var##_v){ \
   CAMLparam2(ctx##_v, var##_v); \
   CAMLlocal1(ret_v); \
   void *ctx = Abstract_val(ctx##_v); \
   var##_type *var = Type_##var##_val(var##_v)

#define END_SCILAB_STUB CAMLreturn(ret_v); }

#define CHECK_IF_VAR_EXISTS(res) if (res != 0) caml_raise_not_found()

#define CHECK_IF_SUCCESS(name, res) if (res != 0) caml_raise_invalid_argument(name) 
  

/* Common */

CAMLprim value
caml_scilab_get_var_address_from_name(value ctx_v, value var_name_v)
{
  CAMLparam2(ctx_v, var_name_v);
  CAMLlocal1(ret_v);
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  SciErr sciErr;
  int* piaddr = NULL;
  sciErr = getVarAddressFromName(ctx, var_name, &piaddr);
  CHECK_IF_VAR_EXISTS(sciErr.err);
  CAMLreturn( Val_abstract(piaddr) );
}

CAMLprim value
caml_scilab_get_var_address_from_position(value ctx_v, value position_v)
{
  CAMLparam2( ctx_v, position_v );
  void *ctx = Abstract_val(ctx_v);
  int position = Int_val(position_v);
  SciErr sciErr;
  int* piaddr = NULL;
  sciErr = getVarAddressFromPosition(ctx, position, &piaddr);
  CHECK_IF_VAR_EXISTS(sciErr.err);
  CAMLreturn( Val_abstract(piaddr) );
}

CAMLprim value
caml_scilab_get_var_name_from_position(value ctx_v, value position_v)
{
  CAMLparam2( ctx_v, position_v );
  void *ctx = Abstract_val(ctx_v);
  int position = Int_val(position_v);
  SciErr sciErr;
  char *psData;
  sciErr = getVarNameFromPosition(ctx, position, psData);
  CHECK_IF_VAR_EXISTS(sciErr.err);
  CAMLreturn( caml_copy_string(psData) );
}

CAMLprim value
caml_scilab_get_var_type(value ctx_v, value piaddr_v)
{
  CAMLparam2( ctx_v, piaddr_v );
  void *ctx = Abstract_val(ctx_v);
  void *piaddr = Abstract_val(piaddr_v);
  SciErr sciErr;
  int typ;
  sciErr = getVarType(ctx, (int *) piaddr, &typ);
  CHECK_IF_VAR_EXISTS(sciErr.err);
  CAMLreturn( Val_int(typ) );
}

CAMLprim value
caml_scilab_get_named_var_type(value ctx_v, value var_name_v)
{
  CAMLparam2( ctx_v, var_name_v );
  void *ctx = Abstract_val(ctx_v);
  char *var_name = String_val(var_name_v);
  SciErr sciErr;
  int typ;
  sciErr = getNamedVarType(ctx, var_name, &typ);
  CHECK_IF_VAR_EXISTS(sciErr.err);
  CAMLreturn( Val_int(typ) );
}

CAMLprim value
caml_scilab_get_var_dimension(value ctx_v, value piaddr_v)
{
  CAMLparam2( ctx_v, piaddr_v );
  CAMLlocal1( dim_v );
  void *ctx = Abstract_val(ctx_v);
  void *piaddr = Abstract_val(piaddr_v);
  SciErr sciErr;
  int rows, cols;
  sciErr = getVarDimension(ctx, (int *) piaddr, (int *) &rows, (int *) &cols);
  CHECK_IF_VAR_EXISTS(sciErr.err);
  dim_v = caml_alloc(2, 0);
  Field(dim_v, 0) = Val_int(rows);
  Field(dim_v, 1) = Val_int(cols);
  CAMLreturn( dim_v );
}

CAMLprim value
caml_scilab_get_named_var_dimension(value ctx_v, value var_name_v)
{
  CAMLparam2( ctx_v, var_name_v );
  CAMLlocal1( dim_v );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  SciErr sciErr;
  int rows, cols;
  sciErr = getNamedVarDimension(ctx, var_name, (int *) &rows, (int *) &cols);
  CHECK_IF_VAR_EXISTS(sciErr.err);
  dim_v = caml_alloc(2, 0);
  Field(dim_v, 0) = Val_int(rows);
  Field(dim_v, 1) = Val_int(cols);
  CAMLreturn( dim_v );
}

BEGIN_SCILAB_STUB(caml_scilab_increase_val_ref, ctx, piaddr);
int bool = increaseValRef( ctx, (int *) piaddr );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_decrease_val_ref, ctx, piaddr);
int bool = decreaseValRef( ctx, (int *) piaddr );
ret_v = Val_bool(bool);
END_SCILAB_STUB;


CAMLprim value
caml_scilab_check_input_argument(value ctx_v, value min_v, value max_v)
{
  CAMLparam3( ctx_v, min_v, max_v );
  void *ctx = Abstract_val(ctx_v);
  int min = Int_val(min_v);
  int max = Int_val(max_v);
  int bool = checkInputArgument(ctx, min, max);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_scilab_check_input_argument_at_least(value ctx_v, value min_v)
{
  CAMLparam2( ctx_v, min_v );
  void *ctx = Abstract_val(ctx_v);
  int min = Int_val(min_v);
  int bool = checkInputArgumentAtLeast(ctx, min);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_scilab_check_input_argument_at_most(value ctx_v, value max_v)
{
  CAMLparam2( ctx_v, max_v );
  void *ctx = Abstract_val(ctx_v);
  int max = Int_val(max_v);
  int bool = checkInputArgumentAtMost(ctx, max);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_scilab_check_output_argument(value ctx_v, value min_v, value max_v)
{
  CAMLparam3( ctx_v, min_v, max_v );
  void *ctx = Abstract_val(ctx_v);
  int min = Int_val(min_v);
  int max = Int_val(max_v);
  int bool = checkOutputArgument(ctx, min, max);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_scilab_check_output_argument_at_least(value ctx_v, value min_v)
{
  CAMLparam2( ctx_v, min_v );
  void *ctx = Abstract_val(ctx_v);
  int min = Int_val(min_v);
  int bool = checkOutputArgumentAtLeast(ctx, min);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_scilab_check_output_argument_at_most(value ctx_v, value max_v)
{
  CAMLparam2( ctx_v, max_v );
  void *ctx = Abstract_val(ctx_v);
  int max = Int_val(max_v);
  int bool = checkOutputArgumentAtMost(ctx, max);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_scilab_get_nb_input_argument(value ctx_v)
{
  CAMLparam1( ctx_v );
  void *ctx = Abstract_val(ctx_v);
  int nb = getNbInputArgument(ctx);
  CAMLreturn( Val_int(nb) );
}

CAMLprim value
caml_scilab_get_nb_output_argument(value ctx_v)
{
  CAMLparam1( ctx_v );
  void *ctx = Abstract_val(ctx_v);
  int nb = getNbOutputArgument(ctx);
  CAMLreturn( Val_int(nb) );
}

CAMLprim value
caml_scilab_return_arguments(value ctx_v)
{
  CAMLparam1( ctx_v );
  void *ctx = Abstract_val(ctx_v);
  int nb = returnArguments((void *) ctx);
  CAMLreturn( Val_int(nb) );
}

CAMLprim value
caml_scilab_call_overload_function(value ctx_v, value ivar_v, value fun_name_v, value length_v)
{
  CAMLparam4( ctx_v, ivar_v, fun_name_v, length_v );
  void *ctx = Abstract_val(ctx_v);
  int ivar = Int_val(ivar_v);
  char* fun_name = String_val(fun_name_v);
  int res = callOverloadFunction(ctx, ivar, fun_name, Unsigned_int_val(length_v));
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_scilab_call_scilab_function(value ctx_v, value fun_name_v, value start_v, value nb_ret_v, value nb_args_v)
{
  CAMLparam5( ctx_v, fun_name_v, start_v, nb_ret_v, nb_args_v );
  void *ctx = Abstract_val(ctx_v);
  char* fun_name = String_val(fun_name_v);
  int start = Int_val(start_v);
  int nb_ret = Int_val(nbret_v);
  int nb_args = Int_val(nbargs_v);
  int res = callScilabFunction(ctx, fun_name, start, nb_ret, nb_args);
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_scilab_named_var_exist(value ctx_v, value var_name_v)
{
  CAMLparam2( ctx_v, var_name_v );
  int bool;
  bool = isNamedVarExist((void *) ctx_v, String_val(var_name_v));
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_scilab_delete_named_var(value ctx_v, value var_name_v)
{
  CAMLparam2( ctx_v, var_name_v );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  int del = deleteNamedVariable(ctx, var_name);
  CAMLreturn( Val_bool(del) );
}

BEGIN_SCILAB_STUB(caml_scilab_is_var_complex, ctx, piaddr);
int bool = isVarComplex( ctx, (int *) piaddr );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_named_var_complex, ctx, var_name);
int bool = isNamedVarComplex( ctx, var_name );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_var_matrix_type, ctx, piaddr);
int bool = isVarMatrixType( ctx, (int *) piaddr );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_named_var_matrix_type, ctx, var_name);
int bool = isNamedVarMatrixType( ctx, var_name );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_row_vector, ctx, piaddr);
int bool = isRowVector( ctx, (int *) piaddr );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_named_row_vector, ctx, var_name);
int bool = isNamedRowVector( ctx, var_name );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_column_vector, ctx, piaddr);
int bool = isColumnVector( ctx, (int *) piaddr );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_named_column_vector, ctx, var_name);
int bool = isNamedColumnVector( ctx, var_name );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_vector, ctx, piaddr);
int bool = isVector( ctx, (int *) piaddr );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_named_vector, ctx, var_name);
int bool = isNamedVector( ctx, var_name );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_scalar, ctx, piaddr);
int bool = isScalar( ctx, (int *) piaddr );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_named_scalar, ctx, var_name);
int bool = isNamedScalar( ctx, var_name );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_square_matrix, ctx, piaddr);
int bool = isSquareMatrix( ctx, (int *) piaddr );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_named_square_matrix, ctx, var_name);
int bool = isNamedSquareMatrix( ctx, var_name );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_empty_matrix, ctx, piaddr);
int bool = isEmptyMatrix( ctx, (int *) piaddr );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_named_empty_matrix, ctx, var_name);
int bool = isNamedEmptyMatrix( ctx, var_name );
ret_v = Val_bool(bool);
END_SCILAB_STUB;


CAMLprim value
caml_scilab_create_empty_matrix(value ctx_v, value position_v)
{
  CAMLparam2( ctx_v, position_v );
  void *ctx = Abstract_val(ctx_v);
  int position = Int_val(position_v);
  int bool = createEmptyMatrix(ctx, position);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_scilab_create_named_empty_matrix(value ctx_v, value var_name_v)
{
  CAMLparam2( ctx_v, var_name_v );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  int bool = createNamedEmptyMatrix(ctx, var_name);
  CAMLreturn( Val_bool(bool) );
}

/* String */

BEGIN_SCILAB_STUB(caml_scilab_is_string_type, ctx, piaddr);
int bool = isStringType( ctx, (int *) piaddr );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_named_string_type, ctx, var_name);
int bool = isNamedStringType( ctx, var_name );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

CAMLprim value
caml_scilab_get_allocated_single_string(value ctx_v, value piaddr_v)
{
  CAMLparam2( ctx_v, piaddr_v );
  void *ctx = Abstract_val(ctx_v);
  void *piaddr = Abstract_val(piaddr_v);
  char *psData;
  int res = getAllocatedSingleString(ctx, (int *) piaddr, &psData);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  CAMLreturn( caml_copy_string(psData) );
}

CAMLprim value
caml_scilab_get_allocated_named_single_string(value ctx_v, value var_name_v)
{
  CAMLparam2( ctx_v, var_name_v );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  char *psData;
  int res = getAllocatedNamedSingleString(ctx, var_name, &psData);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  CAMLreturn( caml_copy_string(psData) );
}

CAMLprim value
caml_scilab_create_single_string(value ctx_v, value position_v, value str_v)
{
  CAMLparam3( ctx_v, position_v, str_v);
  void *ctx = Abstract_val(ctx_v);
  int position = Int_val(position_v);
  char* str = String_val(str_v);
  int res = createSingleString(ctx, position, str_v);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_scilab_create_named_single_string(value ctx_v, value var_name_v, value str_v)
{
  CAMLparam3( ctx_v, var_name_v, str_v);
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  char* str = String_val(str_v);
  int res = createNamedSingleString(ctx, var_name, str);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_scilab_get_matrix_of_string(value ctx_v, value piaddr_v)
{
  CAMLparam2( ctx_v, piaddr_v );
  CAMLlocal1( str_array_v );
  void *ctx = Abstract_val(ctx_v);
  void *piaddr = Abstract_val(piaddr_v);
  SciErr sciErr;
  int i,j;
  //variable info
  int rows		= 0;
  int cols		= 0;
  int* piLen		= NULL;
  char** strData	= NULL;
  sciErr = getVarDimension(ctx, (int *) piaddr, &rows, &cols);
  CHECK_IF_VAR_EXISTS(sciErr.err);
  piLen = (int*)malloc(sizeof(int) * rows * cols);
  //second call to retrieve length of each string
  sciErr = getMatrixOfString(ctx, (int *) piaddr, &rows, &cols, piLen, NULL);
  CHECK_IF_VAR_EXISTS(sciErr.err);
  strData = (char**)malloc(sizeof(char*) * rows * cols + 1);
  for(i = 0 ; i < rows * cols ; i++) {
    strData[i] = (char*)malloc(sizeof(char) * (piLen[i] + 1));
  }
  strData[rows * cols] = (char*)malloc(sizeof(char));
  //third call to retrieve data
  sciErr = getMatrixOfString(ctx, (int *) piaddr, &rows, &cols, piLen, strData);
  strData[rows * cols] = NULL;
  if(sciErr.iErr)
    {
      free(piLen);
      for(i = 0 ; i < rows * cols ; i++)
	{
	  free(strData[i]);
	}
      free(strData);
      caml_raise_not_found();
    }
  str_array_v = caml_copy_string_array((const char **) strData);
  //free memory
  free(piLen);
  for(i = 0 ; i < rows * cols ; i++) {
    free(strData[i]);
  }
  free(strData);
  CAMLreturn( str_array_v ); 
}

CAMLprim value
caml_scilab_get_named_matrix_of_string(value ctx_v, value var_name_v)
{
  CAMLparam2( ctx_v, var_name_v );
  CAMLlocal1( str_array_v );
  void *ctx = Abstract_val(ctx_v);
  char *var_name = String_val(var_name_v);
  SciErr sciErr;
  int i,j;
  //variable info
  int rows		= 0;
  int cols		= 0;
  int* piLen		= NULL;
  char** strData	= NULL;
  sciErr = getNamedVarDimension(ctx, var_name, &rows, &cols);
  CHECK_IF_VAR_EXISTS(sciErr.err);
  piLen = (int*)malloc(sizeof(int) * rows * cols);
  //second call to retrieve length of each string
  sciErr = getNamedMatrixOfString(ctx, var_name, &rows, &cols, piLen, NULL);
  CHECK_IF_VAR_EXISTS(sciErr.err);
  strData = (char**)malloc(sizeof(char*) * rows * cols + 1);
  for(i = 0 ; i < rows * cols ; i++) {
    strData[i] = (char*)malloc(sizeof(char) * (piLen[i] + 1));
  }
  strData[rows * cols] = (char*)malloc(sizeof(char));
  //third call to retrieve data
  sciErr = getNamedMatrixOfString(ctx, var_name, &rows, &cols, piLen, strData);
  strData[rows * cols] = NULL;
  if(sciErr.iErr)
    {
      free(piLen);
      for(i = 0 ; i < rows * cols ; i++)
	{
	  free(strData[i]);
	}
      free(strData);
      caml_raise_not_found();
    }
  str_array_v = caml_copy_string_array((const char **) strData);
  //free memory
  free(piLen);
  for(i = 0 ; i < rows * cols ; i++) {
    free(strData[i]);
  }
  free(strData);
  CAMLreturn( str_array_v ); 
}

CAMLprim value
caml_scilab_create_matrix_of_string(value ctx_v, value position_v, value rows_v, value cols_v, value str_arr_v)
{
  CAMLparam5( ctx_v, position_v, rows_v, cols_v, str_arr_v );
  void *ctx = Abstract_val(ctx_v);
  int position = Int_val(position_v);
  int rows = Int_val(rows);
  int cols = Int_val(cols);
  SciErr sciErr;
  int len = rows * cols;
  int strlen, i;
  char ** strData = (char**)malloc(sizeof(char*) * len);
  for (i=0; i < len; i++)
    {
      strlen = caml_string_length(Field(str_arr_v, i));
      strData[i] = (char*)malloc(sizeof(char) * strlen);
      strData[i] = String_val(Field(str_arr_v, i));
    }
  sciErr = createMatrixOfString(ctx, position, rows, cols,(const char * const*) strData);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  CAMLreturn( Val_int(0) );
}

CAMLprim value
caml_scilab_create_named_matrix_of_string(value ctx_v, value var_name_v, value rows_v, value cols_v, value str_arr_v)
{
  CAMLparam5( ctx_v, var_name_v, rows_v, cols_v, str_arr_v );
  void *ctx = Abstract_val(ctx_v);
  char *var_name = String_val(var_name_v);
  int rows = Int_val(rows);
  int cols = Int_val(cols);
  SciErr sciErr;
  int len = rows * cols;
  int strlen, i;
  char ** strData = (char**)malloc(sizeof(char*) * len);
  for (i=0; i < len; i++)
    {
      strlen = caml_string_length(Field(str_arr_v, i));
      strData[i] = (char*)malloc(sizeof(char) * strlen);
      strData[i] = String_val(Field(str_arr_v, i));
    }
  sciErr = createNamedMatrixOfString(ctx, var_name, rows, cols,(const char * const*) strData);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  CAMLreturn( Val_int(0) );
}

/* Double */

BEGIN_SCILAB_STUB(caml_scilab_is_double_type, ctx, piaddr);
int bool = isDoubleType( ctx, (int *) piaddr );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_named_double_type, ctx, var_name);
int bool = isNamedDoubleType( ctx, var_name );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

CAMLprim value
caml_scilab_get_scalar_double(value ctx_v, value piaddr_v)
{
  CAMLparam2( ctx_v, piaddr_v );
  void *ctx = Abstract_val(ctx_v);
  void *piaddr = Abstract_val(piaddr_v);
  double dbl;
  int res = getScalarDouble(ctx, (int *) piaddr_v, &dbl);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  CAMLreturn( caml_copy_double(dbl) );
}

CAMLprim value
caml_scilab_get_named_scalar_double(value ctx_v, value var_name_v)
{
  CAMLparam2( ctx_v, var_name_v );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  double dbl;
  int res = getNamedScalarDouble(ctx, var_name, &dbl);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  CAMLreturn( caml_copy_double(dbl) );
}

CAMLprim value
caml_scilab_get_scalar_complex_double(value ctx_v, value piaddr_v)
{
  CAMLparam2( ctx_v, piaddr_v );
  CAMLlocal1( val_ret );
  void *ctx = Abstract_val(ctx_v);
  void *piaddr = Abstract_val(piaddr_v);
  double real, complex;
  int res = getScalarComplexDouble(ctx, (int *) piaddr, &real, &complex);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  val_ret = caml_alloc(2, 0);
  Store_field(val_ret, 0, caml_copy_double(real));
  Store_field(val_ret, 1, caml_copy_double(complex));
  CAMLreturn( val_ret );
}

CAMLprim value
caml_scilab_get_named_scalar_complex_double(value ctx_v, value var_name_v)
{
  CAMLparam2( ctx_v, var_name_v );
  CAMLlocal1( val_ret );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  double real, complex;
  int res = getNamedScalarComplexDouble(ctx, var_name, &real, &complex);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  val_ret = caml_alloc(2, 0);
  Store_field(val_ret, 0, caml_copy_double(real));
  Store_field(val_ret, 1, caml_copy_double(complex));
  CAMLreturn( val_ret );
}

CAMLprim value
caml_scilab_create_scalar_double(value ctx_v, value position_v, value dbl_v)
{
  CAMLparam3( ctx_v, position_v, dbl_v );
  void *ctx = Abstract_val(ctx_v);
  int position = Int_val(position_v);
  double dbl = Double_val(dbl_v);
  int res = createScalarDouble(ctx, position, dbl);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_scilab_create_named_scalar_double(value ctx_v, value var_name_v, value dbl_v)
{
  CAMLparam3( ctx_v, var_name_v, dbl );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  double dbl = Double_val(dbl_v);
  int res = createNamedScalarDouble(ctx, var_name, dbl);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_scilab_create_scalar_complex_double(value ctx_v, value position_v, value real_v, value complex_v)
{
  CAMLparam4( ctx_v, position_v, real_v, complex_v );
  void *ctx = Abstract_val(ctx_v);
  int position = Int_val(position_v);
  double real = Double_val(real_v);
  double complex = Double_val(complex_v);
  int res = createScalarComplexDouble(ctx, position, real, complex);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_scilab_create_named_scalar_complex_double(value ctx_v, value var_name_v, value real_v, value complex_v)
{
  CAMLparam4( ctx_v, var_name_v, real_v, complex_v );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  double real = Double_val(real_v);
  double complex = Double_val(complex_v);
  int res = createNamedScalarComplexDouble(ctx, var_name, real, complex);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_scilab_get_matrix_of_double(value ctx_v, value piaddr_v)
{
  CAMLparam2( ctx_v, piaddr_v );
  CAMLlocal1( float_arr_v );
  void *ctx = Abstract_val(ctx_v);
  void *piaddr = Abstract_val(piaddr_v);
  SciErr sciErr;
  int i, rows, cols;
  double* dblValMat;
  sciErr = getVarDimension(ctx, (int *) piaddr, &rows, &cols);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  float_arr_v = caml_alloc((rows * cols) * Double_wosize, Double_array_tag);  
  sciErr = getMatrixOfDouble(ctx, (int *) piaddr, &rows, &cols, dblValMat);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  for(i = 0; i < rows * cols; i++){
    Store_double_field(float_arr_v, i, dblValMat[i]);
  }
  CAMLreturn( float_arr_v );
}

CAMLprim value
caml_scilab_get_named_matrix_of_double(value ctx_v, value var_name_v)
{
  CAMLparam2( ctx_v, var_name_v );
  CAMLlocal1( float_arr_v );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  SciErr sciErr;
  int i, rows, cols;
  double* dblValMat;
  sciErr = getNamedVarDimension(ctx, var_name, &rows, &cols);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  float_arr_v = caml_alloc((rows * cols) * Double_wosize, Double_array_tag);  
  sciErr = getNamedMatrixOfDouble(ctx, var_name, &rows, &cols, dblValMat);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  for(i = 0; i < rows * cols; i++){
    Store_double_field(float_arr_v, i, dblValMat[i]);
  }
  CAMLreturn( float_arr_v );
}

CAMLprim value
caml_scilab_get_complex_matrix_of_double(value ctx_v, value piaddr_v)
{
  CAMLparam2( ctx_v, piaddr_v );
  CAMLlocal3( real_arr_v, complex_arr_v, res_arr_v );
  void *ctx = Abstract_val(ctx_v);
  void *piaddr = Abstract_val(piaddr_v);
  SciErr sciErr;
  int i, rows, cols;
  double* realValMat;
  double* complValMat;
  sciErr = getVarDimension(ctx, (int *) piaddr, &rows, &cols);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  real_arr_v = caml_alloc((rows * cols) * Double_wosize, Double_array_tag);
  complex_arr_v = caml_alloc((rows * cols) * Double_wosize, Double_array_tag);
  res_arr_v = caml_alloc(2, 0);
  realValMat = (double *)malloc(sizeof(double) * (rows * cols));
  complValMat = (double *)malloc(sizeof(double) * (rows * cols));
  sciErr = getComplexMatrixOfDouble(ctx, (int *) piaddr, &rows, &cols, realValMat, complValMat);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  for(i = 0; i < rows * cols; i++){
    Store_double_field(real_arr_v, i, realValMat[i]);
    Store_double_field(complex_arr_v, i, complValMat[i]);
  }
  Store_field(res_arr_v, 0, real_arr_v);
  Store_field(res_arr_v, 1, complex_arr_v);
  CAMLreturn( res_arr_v );
}

CAMLprim value
caml_scilab_get_named_complex_matrix_of_double(value ctx_v, value var_name_v)
{
  CAMLparam2( ctx_v, var_name_v );
  CAMLlocal3( real_arr_v, complex_arr_v, res_arr_v );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  SciErr sciErr;
  int i, rows, cols;
  double* realValMat;
  double* complValMat;
  sciErr = getNamedVarDimension(ctx, var_name, &rows, &cols);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  real_arr_v = caml_alloc((rows * cols) * Double_wosize, Double_array_tag);
  complex_arr_v = caml_alloc((rows * cols) * Double_wosize, Double_array_tag);
  res_arr_v = caml_alloc(2, 0);
  realValMat = (double *)malloc(sizeof(double) * (rows * cols));
  complValMat = (double *)malloc(sizeof(double) * (rows * cols));
  sciErr = getNamedComplexMatrixOfDouble(ctx, var_name, &rows, &cols, realValMat, complValMat);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  for(i = 0; i < rows * cols; i++){
    Store_double_field(real_arr_v, i, realValMat[i]);
    Store_double_field(complex_arr_v, i, complValMat[i]);
  }
  Store_field(res_arr_v, 0, real_arr_v);
  Store_field(res_arr_v, 1, complex_arr_v);
  CAMLreturn( res_arr_v );
}

CAMLprim value
caml_scilab_create_matrix_of_double(value ctx_v, value position_v, value rows_v, value cols_v, value dbl_arr_v)
{
  CAMLparam5( ctx_v, position_v, rows_v, cols_v, dbl_arr_v );
  void *ctx = Abstract_val(ctx_v);
  int position = Int_val(position_v);
  SciErr sciErr;
  int rows = Int_val(rows_v);
  int cols = Int_val(cols_V);
  int len = rows * cols;
  int i;
  double* dblValMat = (double *)malloc(sizeof(double) * len);
  for (i=0; i < len; i++) dblValMat[i] = Double_field(dbl_arr_v, i);
  sciErr = createMatrixOfDouble(ctx, position, rows, cols, dblValMat);
  free(dblValMat);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  CAMLreturn(Val_int(0));
}

CAMLprim value
caml_scilab_create_named_matrix_of_double(value ctx_v, value var_name_v, value rows_v, value cols_v, value dbl_arr_v)
{
  CAMLparam5( ctx_v, var_name_v, rows_v, cols_v, dbl_arr_v );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  SciErr sciErr;
  int rows = Int_val(rows);
  int cols = Int_val(cols);
  int len = rows * cols;
  int i;
  double* dblValMat = (double *)malloc(sizeof(double) * len);
  for (i=0; i < len; i++) dblValMat[i] = Double_field(dbl_arr_v, i);
  sciErr = createNamedMatrixOfDouble(ctx, var_name, rows, cols, dblValMat);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  CAMLreturn(Val_int(0));
}

CAMLprim value
caml_scilab_create_complex_matrix_of_double(value ctx_v, value position_v, value dim_v, value real_arr_v, value compl_arr_v)
{
  CAMLparam5( ctx_v, position_v, dim_v, real_arr_v, compl_arr_v );
  void *ctx = Abstract_val(ctx_v);
  int position = Int_val(position_v);
  SciErr sciErr;
  int rows = Int_val(Field(dim_v, 0));
  int cols = Int_val(Field(dim_v, 1));
  int len = rows * cols;
  int i;
  double* realValMat = (double *)malloc(sizeof(double) * len);
  double* complValMat = (double *)malloc(sizeof(double) * len);
  for (i=0; i < len; i++) {
    realValMat[i] = Double_field(real_arr_v, i);
    complValMat[i] = Double_field(compl_arr_v, i);
  }
  sciErr = createComplexMatrixOfDouble(ctx, position, rows,cols, realValMat,complValMat);
  free(realValMat);
  free(complValMat);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  CAMLreturn(Val_int(0));
}

CAMLprim value
caml_scilab_create_named_complex_matrix_of_double(value ctx_v, value var_name_v, value dim_v, value real_arr_v, value compl_arr_v)
{
  CAMLparam5( ctx_v, var_name_v, dim_v, real_arr_v, compl_arr_v);
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  SciErr sciErr;
  int rows = Int_val(Field(dim_v, 0));
  int cols = Int_val(Field(dim_v, 1));
  int len = rows * cols;
  int i;
  double* realValMat = (double *)malloc(sizeof(double) * len);
  double* complValMat = (double *)malloc(sizeof(double) * len);
  for (i=0; i < len; i++) {
    realValMat[i] = Double_field(real_arr_v, i);
    complValMat[i] = Double_field(compl_arr_v, i);
  }
  sciErr = createNamedComplexMatrixOfDouble(ctx, 
					    var_name, 
					    rows, 
					    cols, 	      
					    realValMat,
					    complValMat);
  free(realValMat);
  free(complValMat);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  CAMLreturn(Val_int(0));
}


/* Boolean */


BEGIN_SCILAB_STUB(caml_scilab_is_boolean_type, ctx, piaddr);
int bool = isBooleanType( ctx, (int *) piaddr );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

BEGIN_SCILAB_STUB(caml_scilab_is_named_boolean_type);
int bool = isNamedBooleanType( ctx, var_name );
ret_v = Val_bool(bool);
END_SCILAB_STUB;

CAMLprim value
caml_scilab_get_scalar_boolean(value ctx_v, value piaddr_v)
{
  CAMLparam2( ctx_v, piaddr_v );
  void *ctx = Abstract_val(ctx_v);
  void *piaddr = Abstract_val(piaddr_v);
  int bool;
  int res = getScalarBoolean(ctx, (int *) piaddr_v, &bool);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_scilab_get_named_scalar_boolean(value ctx_v, value var_name_v)
{
  CAMLparam2( ctx_v, var_name_v );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  int bool;
  int res = getNamedScalarBoolean(ctx, var_name, &bool);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  CAMLreturn( Val_bool(bool) );
}

CAMLprim value
caml_scilab_create_scalar_boolean(value ctx_v, value position_v, value bool_v)
{
  CAMLparam3( ctx_v, position_v, bool_v );
  void *ctx = Abstract_val(ctx_v);
  int position = Int_val(position_v);
  int bool = Int_val(bool_v);
  int res = createScalarBoolean(ctx, position, bool);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_scilab_create_named_scalar_boolean(value ctx_v, value var_name_v, value bool_v)
{
  CAMLparam3( ctx_v, var_name_v, bool_v );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  int bool = Int_val(bool_v);
  int res = createNamedScalarBoolean(ctx, var_name, bool);
  CHECK_IF_SUCCESS(__FUNCTION__, res);
  CAMLreturn( Val_int(res) );
}

CAMLprim value
caml_scilab_get_matrix_of_boolean(value ctx_v, value piaddr_v)
{
  CAMLparam2( ctx_v, piaddr_v );
  CAMLlocal1( bool_arr_v );
  void *ctx = Abstract_val(ctx_v);
  void *piaddr = Abstract_val(piaddr_v);
  SciErr sciErr;  
  int i, rows, cols;
  int* boolValMat;
  sciErr = getVarDimension(ctx, (int *) piaddr, &rows, &cols);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  sciErr = getMatrixOfBoolean(ctx, (int *) piaddr, &rows, &cols, &boolValMat);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  bool_arr_v = caml_alloc((rows * cols), 0);
  for(i = 0; i < rows * cols; i++){
    Store_double_field(bool_arr_v, i, boolValMat[i]);
  }
  CAMLreturn( bool_arr_v );
}

CAMLprim value
caml_scilab_get_named_matrix_of_boolean(value ctx_v, value var_name_v)
{
  CAMLparam2( ctx_v, var_name_v );
  CAMLlocal1( bool_arr_v );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  SciErr sciErr;  
  int i, rows, cols;
  int* boolValMat;
  sciErr = getNamedVarDimension(ctx, var_name, &rows, &cols);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  sciErr = readNamedMatrixOfBoolean(ctx, var_name, &rows, &cols, &boolValMat);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);
  bool_arr_v = caml_alloc((rows * cols), 0);
  for(i = 0; i < rows * cols; i++){
    Store_double_field(bool_arr_v, i, boolValMat[i]);
  }
  CAMLreturn( bool_arr_v );
}

CAMLprim value
caml_scilab_create_matrix_of_boolean(value ctx_v, value position_v, value rows_v, value cols_v, value bool_arr_v)
{
  CAMLparam5( ctx_v, position_v, rows_v, cols_v, bool_arr_v );
  void *ctx = Abstract_val(ctx_v);
  int position = Int_val(position_v);
  SciErr sciErr;  
  int rows = Int_val(rows_v);
  int cols = Int_val(cols_v);
  int len = rows * cols;
  int i;
  int* boolValMat = (int *)malloc(sizeof(int) * len);
  for (i=0; i < len; i++) boolValMat[i] = Int_val(Field(bool_arr_v, i));
  sciErr = createMatrixOfBoolean(ctx, position, rows, cols, boolValMat);
  free(boolValMat);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);    
  CAMLreturn(Val_int(0));
}

CAMLprim value
caml_scilab_create_named_matrix_of_boolean(value ctx_v, value var_name_v, value rows_v, value cols_v, value bool_arr_v)
{
  CAMLparam5( ctx_v, var_name_v, rows_v, cols_v, bool_arr_v );
  void *ctx = Abstract_val(ctx_v);
  char* var_name = String_val(var_name_v);
  SciErr sciErr;
  int rows = Int_val(rows_v);
  int cols = Int_val(cols_v);
  int len = rows * cols;
  int i;
  int* boolValMat = (int *)malloc(sizeof(int) * len);
  for (i=0; i < len; i++) boolValMat[i] = Int_val(Field(bool_arr_v, i));
  sciErr = createNamedMatrixOfDouble(ctx, var_name, rows, cols, boolValMat);
  free(boolValMat);
  CHECK_IF_SUCCESS(__FUNCTION__, sciErr.err);    
  CAMLreturn(Val_int(0));
}

/* (U)INT8 (U)INT16, INT OCAML */
/* (U)INT32, INT32 OCAML */

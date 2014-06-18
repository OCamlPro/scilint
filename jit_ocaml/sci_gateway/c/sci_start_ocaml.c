#include "api_scilab.h"

/* ==================================================================== */
extern int start_ocaml();
/* ==================================================================== */
int sci_jitstart(char *fname, void *pvApiCtx)
{
  CheckInputArgument(pvApiCtx, 0, 0);
  CheckOutputArgument(pvApiCtx, 1, 1);
  start_ocaml();
}

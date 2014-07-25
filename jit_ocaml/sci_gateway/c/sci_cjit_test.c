/* ==================================================================== */
extern int cjit_test(void* pivApiCtx);
/* ==================================================================== */

int sci_cjit_test(char *fname, void *pvApiCtx)
{
  return cjit_test(pvApiCtx);
}
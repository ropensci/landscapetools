#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
    Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void rcpp_get_jenkbreaks(void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"rcpp_get_jenkbreaks", (DL_FUNC) &rcpp_get_jenkbreaks, 4},
    {NULL, NULL, 0}
};

void R_init_landscapetools(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

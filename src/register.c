#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
*    Check these declarations against the C/Fortran source code.
*    */

/* .Call calls */

extern SEXP readstata13_stata_pre13_save(SEXP, SEXP);
extern SEXP readstata13_stata_read(SEXP, SEXP);
extern SEXP readstata13_stata_save(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"readstata13_stata_pre13_save", (DL_FUNC) &readstata13_stata_pre13_save, 2},
    {"readstata13_stata_read",       (DL_FUNC) &readstata13_stata_read,       3},
    {"readstata13_stata_save",       (DL_FUNC) &readstata13_stata_save,       2},
    {NULL, NULL, 0}
};

void R_init_readstata13(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}


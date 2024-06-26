#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP c_paramset_ids(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_paramset_ids", (DL_FUNC) &c_paramset_ids, 5},
    {NULL, NULL, 0}
};

void R_init_mypackage(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

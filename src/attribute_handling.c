#include <R.h>
#include <Rinternals.h>

SEXP CcopyMostAttrib(SEXP to, SEXP from) {
   Rf_copyMostAttrib(from, to);
   return R_NilValue; // solves an issue: converting NULL pointer to R NULL
}

SEXP CDUPLICATE_ATTRIB(SEXP to, SEXP from) {
   DUPLICATE_ATTRIB(to, from);
   return R_NilValue; // solves an issue: converting NULL pointer to R NULL
}

static const R_CallMethodDef CallEntries[] = {
  {"C_copyMostAttrib", (DL_FUNC) &CcopyMostAttrib, 2},
  {"C_DUPLICATE_ATTRIB", (DL_FUNC) &CDUPLICATE_ATTRIB, 2},
  {NULL, NULL, 0}
};

void R_init_fastverse(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}




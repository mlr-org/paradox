#include <R.h>
#include <Rinternals.h>



// typedef for checkmate::qassert signature
typedef void (*fun_t)(SEXP x, const char *rule, const char *name);

SEXP c_paramset_ids(SEXP s_paramtbl, SEXP s_tagtbl, SEXP s_classes, SEXP s_tags, SEXP s_anytags) {
    fun_t qassert = (fun_t) R_GetCCallable("checkmate", "qassert");

    if (!isNull(s_classes)) qassert(s_classes, "S", "class");
    if (!isNull(s_tags)) qassert(s_tags, "S", "tags");
    if (!isNull(s_anytags)) qassert(s_anytags, "S", "any_tags");

    int s_classes_n = LENGTH(s_classes);
    int s_tags_n = LENGTH(s_tags);
    int s_anytags_n = LENGTH(s_anytags);
    /* Rprintf("s_classes_n=%i, s_tags=%i, s_anytags_n=%i\n", s_classes_n, s_tags_n, s_anytags_n); */

    int paramtbl_nrows = LENGTH(VECTOR_ELT(s_paramtbl, 0));
    int tagtbl_nrows = LENGTH(VECTOR_ELT(s_tagtbl, 0));
    // FIXME: i am not sure if we want to index cols by nr here...
    SEXP s_paramtbl_ids = VECTOR_ELT(s_paramtbl, 0);

    SEXP s_paramtbl_classes = VECTOR_ELT(s_paramtbl, 1);
    SEXP s_tagtbl_ids = VECTOR_ELT(s_tagtbl, 0);
    SEXP s_tagtbl_tags = VECTOR_ELT(s_tagtbl, 1);

    // result; potentially too large. has as many els as we have params
    SEXP s_ids = PROTECT(allocVector(STRSXP, paramtbl_nrows));
    int s_ids_count = 0;

    // iter thru all rows in paramtbl and check that for each param all conditions hold
    for (int i = 0; i < paramtbl_nrows; i++) {
        /* Rprintf("i=%i, id=%s, class=%s\n", i, id, class); */
        // check that params's class is in "s_classes"
        // if s_classes is NULL or empty, we dont need to check
        if (s_classes_n > 0) {
            SEXP class = STRING_ELT(s_paramtbl_classes, i);
            int ok_classes = 0;
            for (int j = 0; j < s_classes_n; j++) {
                if (class == STRING_ELT(s_classes, j)) {
                    /* Rprintf("class ok, j = %i\n", j); */
                    ok_classes = 1;
                }
            }
            if (!ok_classes) continue;
        }
        SEXP id = STRING_ELT(s_paramtbl_ids, i);

        // check that param has all tags that are in "s_tags"
        // if s_tags is NULL or empty, we dont need to check
        if (s_tags_n > 0) {
            int ok_tags = 0;
            for (int j = 0; j < s_tags_n; j++) {
                SEXP tag = STRING_ELT(s_tags, j);
                // FIXME: this search is super slow, we iterate the tbl again and again
                for (int k = 0; k < tagtbl_nrows; k++) {
                    SEXP tagtbl_id = STRING_ELT(s_tagtbl_ids, k);
                    SEXP tagtbl_tag = STRING_ELT(s_tagtbl_tags, k);
                    if (id == tagtbl_id && tag == tagtbl_tag)
                        ok_tags++;
                }
            }
            /* Rprintf("ok_tags=%i\n", ok_tags); */
            // we didnt find all tags, so we skip current param
            if (ok_tags < s_tags_n) continue;
        }

        // check that param has at least one tag from "s_anytags"
        // if s_anytags is NULL or empty, we dont need to check
        if (s_anytags_n > 0) {
            int ok_anytags = 0;
            for (int j = 0; j < s_anytags_n; j++) {
                SEXP anytag = STRING_ELT(s_anytags, j);
                for (int k = 0; k < tagtbl_nrows; k++) {
                    SEXP tagtbl_id = STRING_ELT(s_tagtbl_ids, k);
                    SEXP tagtbl_tag = STRING_ELT(s_tagtbl_tags, k);
                    if (id == tagtbl_id && anytag == tagtbl_tag)
                        ok_anytags = 1;
                }
            }
            if (!ok_anytags) continue;
        }

        // if we ended up here, we add param to result
        SET_STRING_ELT(s_ids, s_ids_count++, mkChar(CHAR(id)));
    }

    // copy result to shorter charvec of correct size
    SEXP s_ids_2 = PROTECT(allocVector(STRSXP, s_ids_count));
    for (int i = 0; i < s_ids_count; i++)
        SET_STRING_ELT(s_ids_2, i, STRING_ELT(s_ids, i));
    UNPROTECT(2);
    return s_ids_2;
}


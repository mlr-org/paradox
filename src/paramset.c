#include <R.h>
#include <Rinternals.h>

#define RETURN_STRING(str)          \
    SEXP result = PROTECT(allocVector(STRSXP, 1)); \
    SET_STRING_ELT(result, 0, mkChar(str));        \
    UNPROTECT(1);                                  \
    return result;

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


// FIXME: is this really correct? why does this work? s_name is a charvec?
int charvec_find_index(SEXP s_charvec, SEXP s_string) {
    int n = LENGTH(s_charvec);
    for (int i = 0; i < n; i++) {
        /* Rprintf("i = %i, s=%s\n", i, CHAR(s_string)); */
        if (STRING_ELT(s_charvec, i) == s_string)
            return i;
    }
    return -1;
}


/* set_values = function(..., .values = list(), .insert = TRUE) { */
SEXP c_paramset_set_values(SEXP s_paramset, SEXP s_dotvals, SEXP s_values, SEXP s_insert) {
    /* Rprintf("foo1111ooo\n"); */
    // FIXME: add arg checks
    /* fun_t qassert = (fun_t) R_GetCCallable("checkmate", "qassert"); */
    /* assert_list(dots, names = "unique") */
    /* assert_list(.values, names = "unique") */
    /* assert_disjunct(names(dots), names(.values)) */
    /* if (!isNull(s_classes)) qassert(s_classes, "S", "class"); */
    /* if (!isNull(s_tags)) qassert(s_tags, "S", "tags"); */
    /* if (!isNull(s_anytags)) qassert(s_anytags, "S", "any_tags"); */

    int insert = LOGICAL(s_insert)[0];
    int s_dotvals_n = LENGTH(s_dotvals);
    int s_values_n = LENGTH(s_values);
    SEXP s_result, s_result_ns;
    /* Rprintf("s_dotvales_n=%i, s_values_n=%i\n", s_dotvals_n, s_values_n); */

    SEXP s_dotvals_ns = getAttrib(s_dotvals, R_NamesSymbol);
    SEXP s_values_ns = getAttrib(s_values, R_NamesSymbol);
    SEXP slot_symbol = install("values");


    if (!insert) {
        // FIXME: we need to check that names of dots and values are distinct.
        // we can probably do this here...

        // we now copy s_dotvals and s_values into a new object
        // (and we also copy names), under the assumption thats both lists are named and disjoint
        s_result = PROTECT(allocVector(VECSXP, s_dotvals_n + s_values_n));
        SEXP s_result_ns = PROTECT(allocVector(STRSXP, s_dotvals_n + s_values_n));
        for (int i = 0; i < s_dotvals_n; i++) {
            SET_VECTOR_ELT(s_result, i, VECTOR_ELT(s_dotvals, i));
            SET_STRING_ELT(s_result_ns, i, STRING_ELT(s_dotvals_ns, i));
        }
        for (int i = 0; i < s_values_n; i++) {
            SET_VECTOR_ELT(s_result, s_dotvals_n + i, VECTOR_ELT(s_values, i));
            SET_STRING_ELT(s_result_ns, s_dotvals_n + i, STRING_ELT(s_values_ns, i));
        }
    } else {
        SEXP s_member = findVarInFrame(s_paramset, slot_symbol);
        int s_member_n = LENGTH(s_member);
        SEXP s_member_ns = getAttrib(s_member, R_NamesSymbol);
        int n_add = 0;
        int s_dotvals_idx[s_dotvals_n];
        int s_values_idx[s_values_n];
        // go thru names of dotvals, find them in target_ns
        // store index in array (with -1)
        // go thru names of values, find them in target_ns
        // store index in array (with -1)
        // FIXME: linear search is somewhat slow. but maybe ok for small lists?
        for (int i = 0; i < s_dotvals_n; i++) {
            int j = charvec_find_index(s_member_ns, STRING_ELT(s_dotvals_ns, i));
            s_dotvals_idx[i] = j;
            // FIXME: we can already set the target res index here
            if (j == -1) n_add++;
            /* Rprintf("check dotvals i=%i, j=%i\n", i, j); */
        }
        for (int i = 0; i < s_values_n; i++) {
            int j = charvec_find_index(s_member_ns, STRING_ELT(s_values_ns, i));
            s_values_idx[i] = j;
            if (j == -1) n_add++;
            /* Rprintf("check values i=%i, j=%i\n", i, j); */
        }
        // FIXME: special case when nothing is added? we dont need to copy...
        /* Rprintf("alloc s_res len=%i + %i\n", s_member_n, n_add); */
        s_result = PROTECT(allocVector(VECSXP, s_member_n + n_add));
        s_result_ns = PROTECT(allocVector(STRSXP, s_member_n + n_add));

        // copy current member list
        for (int i = 0; i < s_member_n; i++) {
            /* Rprintf("copy members i=%i, j=%i\n", i, i); */
            SET_VECTOR_ELT(s_result, i, VECTOR_ELT(s_member, i));
            SET_STRING_ELT(s_result_ns, i, STRING_ELT(s_member_ns, i));
        }
        int s_result_count = s_member_n-1;
        // copy dotvals into result
        for (int i = 0; i < s_dotvals_n; i++) {
            int j = s_dotvals_idx[i];
            if (j == -1) j = ++s_result_count;
            /* Rprintf("copy dotvals i=%i, j=%i\n", i, j); */
            SET_VECTOR_ELT(s_result, j, VECTOR_ELT(s_dotvals, i));
            SET_STRING_ELT(s_result_ns, j, STRING_ELT(s_dotvals_ns, i));
        }
        // copy values into result
        for (int i = 0; i < s_values_n; i++) {
            int j = s_values_idx[i];
            if (j == -1) j = ++s_result_count;
            /* Rprintf("copy values i=%i, j=%i\n", i, j); */
            SET_VECTOR_ELT(s_result, j, VECTOR_ELT(s_values, i));
            SET_STRING_ELT(s_result_ns, j, STRING_ELT(s_values_ns, i));
        }
    }
    setAttrib(s_result, R_NamesSymbol, s_result_ns);
    // assign s_result into "values" of the R6 ParamSet object
    // FIXME: we have to write into private slot here?
    UNPROTECT(2);
    /* SEXP private_env = findVarInFrame(r6_object, install(".__enclos_env__")); */
    /* private_env = findVarInFrame(private_env, install("private")); */
    /* defineVar(field_symbol, new_value, private_env); */
    /* defineVar(slot_symbol, s_result, s_paramset); */
    return s_result;
}

// partially copied over from checkmate, but boiled down to essentials
// FIXME: do we really need to check for factors, dates?
int check_integerish(SEXP s_x, int *res) {
    if (length(s_x) != 1) return 0;
    switch(TYPEOF(s_x)) {
        case INTSXP:
            if (inherits(s_x, "factor")) return 0;
            *res = asInteger(s_x);
            return 1;
        case REALSXP:
            if (inherits(s_x, "Date") || inherits(s_x, "POSIXt"))
                return 0;
            // FIXME: i dont think we need to do so much tol checking here,
            // if we do some mild sanitizing anayway
            double x = asReal(s_x);
            if (!ISNAN(x)) {
                if (x <= INT_MIN || x > INT_MAX)
                    return 0;
                // FIXME: magic nr for tol
                if (fabs(x - nearbyint(x)) < 1e-8) {
                    *res = (int) x;
                    return 1;
                }
            }
        default: return 0;
    }
}

//FIXME: we need to handle special vals
SEXP c_paramset_domain_check(SEXP s_paramset, SEXP s_values) {
    // FIXME: do we have bad checka as for untyped params etc...? just get rid of this?
    SEXP private_env = findVarInFrame(s_paramset, install(".__enclos_env__"));
    private_env = findVarInFrame(private_env, install("private"));
    SEXP s_paramtbl = findVarInFrame(private_env, install(".params"));
    // FIXME: again we use numbers to index cols
    SEXP s_ids = VECTOR_ELT(s_paramtbl, 0);
    SEXP s_classes = VECTOR_ELT(s_paramtbl, 1);
    SEXP s_lows = VECTOR_ELT(s_paramtbl, 4);
    SEXP s_upps = VECTOR_ELT(s_paramtbl, 5);
    SEXP s_levs = VECTOR_ELT(s_paramtbl, 7);
    // FIXME: how to handle deps? and NAs?
    int s_values_n = LENGTH(s_values);
    // FIXME: better macros for this stuff?
    SEXP s_values_ns = getAttrib(s_values, R_NamesSymbol);
    // FIXME: we need to be sure that we always have enough space...
    char err_msg[200];

    // iterate over all values. check their basic storage type, and whether they are in-bounds
    for (int i = 0; i < s_values_n; i++) {
        SEXP s_x = VECTOR_ELT(s_values, i);
        /* Rprintf("i=%i\n", i); */

        // if s_x is a TuneToken, we skip it, and do no checks
        SEXP class_attr = getAttrib(s_x, R_ClassSymbol);
        /* if (class_attr != R_NilValue) */
            /* Rprintf("class = %s\n", CHAR(STRING_ELT(class_attr, 0))); */
        if (class_attr != R_NilValue && strcmp(CHAR(STRING_ELT(class_attr, length(class_attr)-1)), "TuneToken") == 0)
            continue;

        SEXP s_x_name = STRING_ELT(s_values_ns, i);
        int j = charvec_find_index(s_ids, s_x_name);
        SEXP p_class = STRING_ELT(s_classes, j);
        /* Rprintf("i=%i, j=%i, name=%s, class=%s\n", i, j, CHAR(s_x_name), CHAR(p_class)); */
        // FIXME: use strcmp? or the R hashing?
        if (strcmp(CHAR(p_class), "ParamDbl") == 0) {
            if (TYPEOF(s_x) != REALSXP || length(s_x) != 1) {
                sprintf(err_msg, "%s: Value is not of type 'scalar double'!\n", CHAR(s_x_name));
                RETURN_STRING(err_msg);
            }
            double low = REAL(s_lows)[j];
            double upp = REAL(s_upps)[j];
            double x = asReal(s_x);
            // FIXME: use some tol for check?
            // FIXME: maybe we can make the message more cm-like?
            // also works for +-Inf (either in val or bound)
            // for NA=NAN these comparisons will always return "false"
            if (!(x >= low && x <= upp) ) {
                sprintf(err_msg, "%s: Value %g is not in [%g, %g]!\n", CHAR(s_x_name), x, low, upp);
                RETURN_STRING(err_msg);
            }
            // FIXME: what about NA and NAN?
            // NAN seems to pass, for NA we have this type problem
            // NA_real_ also seems to pass, really check that both cases are handled in tests
        } else if (strcmp(CHAR(p_class), "ParamFct") == 0) {
            if (TYPEOF(s_x) != STRSXP || length(s_x) != 1) {
                sprintf(err_msg, "%s: Value is not of type 'string'!\n", CHAR(s_x_name));
                RETURN_STRING(err_msg);
            }
            s_x = STRING_ELT(s_x, 0);
            SEXP s_p_levels = VECTOR_ELT(s_levs, j);
            int level_ok = charvec_find_index(s_p_levels, s_x);
            /* Rprintf("level_ok=%i\n", level_ok); */
            if (level_ok == -1) {
                sprintf(err_msg, "%s: Value '%s' is not in feasible levels!\n", CHAR(s_x_name), CHAR(STRING_ELT(s_x, 0)));
                RETURN_STRING(err_msg);
            }
        } else if (strcmp(CHAR(p_class), "ParamInt") == 0) {
            int x;
            if (!check_integerish(s_x, &x)) {
                sprintf(err_msg, "%s: Value is not of type 'scalar int(egerish)'!\n", CHAR(s_x_name));
                RETURN_STRING(err_msg);
            }
            double low = REAL(s_lows)[j];
            double upp = REAL(s_upps)[j];
            if (x < low || x > upp) {
                sprintf(err_msg, "%s: Value %i is not in [%g, %g]!\n", CHAR(s_x_name), x, low, upp);
                RETURN_STRING(err_msg);
            }
        } else if (strcmp(CHAR(p_class), "ParamLgl") == 0) {
            if (TYPEOF(s_x) != LGLSXP || length(s_x) != 1) {
                sprintf(err_msg, "%s: Value for not of type 'scalar logical'!\n", CHAR(s_x_name));
                RETURN_STRING(err_msg);
            }
        }
    }
    return R_NilValue;
}


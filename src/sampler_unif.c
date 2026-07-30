#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <string.h>

#include "paradox.h"
#include <R_ext/Random.h>

#include "core_state.h"
#include "paramset_collection_readers.h"
#include "paramset_domain_common.h"
#include "r_api_compat.h"
#include "r_utils.h"

/*
 * Uniform sampling is intentionally one closed capsule operation.  The
 * package-owned Sampler1D objects exposed by SamplerUnif are compatibility
 * metadata; they are neither consulted nor executed here.  Fixed values
 * and dependencies are applied exactly once by Design$new() after this
 * routine returns its independently owned data.table facade.
 */

typedef enum {
  SAMPLER_DBL = 1,
  SAMPLER_INT,
  SAMPLER_FCT,
  SAMPLER_LGL
} sampler_kind_t;

typedef struct {
  sampler_kind_t kind;
  SEXP id;
  double lower;
  double upper;
  SEXP levels;
} sampler_spec_t;

static unsigned char sampler_unif_handoff_identity;

static SEXP sampler_unif_handoff_tag(void) {
  return Rf_install("paradox.sampler.unif.subspace.v1");
}

static R_xlen_t parse_row_count(SEXP input) {
  SEXP n = PROTECT(paradox_snapshot_semantic_vector(input));
  const SEXPTYPE type = (SEXPTYPE) TYPEOF(n);
  /* Names/classes on a scalar count never carried sampling semantics and
   * checkmate historically admitted them. The owned snapshot deliberately
   * ignores those attributes after materialization. A factor is the one
   * classed integer checkmate rejects, and reading its level code as a row
   * count is the sole way this boundary could answer a nonsensical request
   * with a plausible design instead of the established diagnostic. */
  if (Rf_isFactor(input)) {
    UNPROTECT(1);
    Rf_error("`n` must be one non-negative integer, not a factor");
  }
  if ((type != INTSXP && type != REALSXP) || XLENGTH(n) != 1) {
    UNPROTECT(1);
    Rf_error("`n` must be one non-negative integer");
  }

  double value;
  if (type == INTSXP) {
    const int observed = INTEGER_ELT(n, 0);
    value = observed == NA_INTEGER ? NA_REAL : (double) observed;
  } else {
    value = REAL_ELT(n, 0);
  }
  if (!R_FINITE(value) || value < 0.0 || value > (double) INT_MAX ||
      floor(value) != value) {
    UNPROTECT(1);
    Rf_error(
      "`n` must be one non-negative integer not greater than %d",
      INT_MAX
    );
  }
  UNPROTECT(1);
  return (R_xlen_t) value;
}

static sampler_kind_t row_kind(SEXP classes, R_xlen_t row) {
  SEXP cls = STRING_ELT(classes, row);
  if (paradox_domain_string_is(cls, "ParamDbl")) {
    return SAMPLER_DBL;
  }
  if (paradox_domain_string_is(cls, "ParamInt")) {
    return SAMPLER_INT;
  }
  if (paradox_domain_string_is(cls, "ParamFct")) {
    return SAMPLER_FCT;
  }
  if (paradox_domain_string_is(cls, "ParamLgl")) {
    return SAMPLER_LGL;
  }
  Rf_error("ParamSet contains untyped params");
  return SAMPLER_DBL;
}

static void capture_specs(const paradox_domain_params_t *params,
    sampler_spec_t *specs, R_xlen_t requested_rows,
    R_xlen_t *work_since_interrupt) {
  SEXP lower = VECTOR_ELT(params->table, PARADOX_DOMAIN_LOWER);
  SEXP upper = VECTOR_ELT(params->table, PARADOX_DOMAIN_UPPER);
  SEXP levels = VECTOR_ELT(params->table, PARADOX_DOMAIN_LEVELS);

  for (R_xlen_t row = 0; row < params->row_count; ++row) {
    paradox_account_work(work_since_interrupt);
    sampler_spec_t *spec = &specs[row];
    spec->kind = row_kind(params->classes, row);
    spec->id = STRING_ELT(params->ids, row);
    spec->lower = paradox_numeric_elt(lower, row);
    spec->upper = paradox_numeric_elt(upper, row);
    spec->levels = VECTOR_ELT(levels, row);

    switch (spec->kind) {
    case SAMPLER_DBL:
      if (!R_FINITE(spec->lower) || !R_FINITE(spec->upper) ||
          spec->lower > spec->upper) {
        Rf_error("ParamSet contains unbounded params");
      }
      break;
    case SAMPLER_INT:
      if (!R_FINITE(spec->lower) || !R_FINITE(spec->upper) ||
          spec->lower > spec->upper ||
          spec->lower < -(double) INT_MAX ||
          spec->upper > (double) INT_MAX ||
          spec->lower != (double) ((int) spec->lower) ||
          spec->upper != (double) ((int) spec->upper)) {
        Rf_error("ParamSet contains unbounded or invalid integer params");
      }
      break;
    case SAMPLER_FCT:
      if (TYPEOF(spec->levels) != STRSXP || ALTREP(spec->levels) ||
          Rf_isObject(spec->levels)) {
        Rf_error("Corrupt ParamSet sampling state: invalid factor levels");
      }
      if (requested_rows != 0 && XLENGTH(spec->levels) == 0) {
        Rf_error("Cannot sample a factor parameter with no levels");
      }
      break;
    case SAMPLER_LGL:
      /* The shared parameter validator already proves c(TRUE, FALSE). */
      break;
    }
  }
}

static SEXPTYPE output_type(sampler_kind_t kind) {
  switch (kind) {
  case SAMPLER_DBL:
    return REALSXP;
  case SAMPLER_INT:
    return INTSXP;
  case SAMPLER_FCT:
    return STRSXP;
  case SAMPLER_LGL:
    return LGLSXP;
  }
  Rf_error("Internal error: unknown uniform sampler kind");
  return NILSXP;
}

static SEXP allocate_table(const sampler_spec_t *specs,
    R_xlen_t parameter_count, R_xlen_t rows) {
  SEXP table = PROTECT(Rf_allocVector(VECSXP, parameter_count));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, parameter_count));
  for (R_xlen_t column = 0; column < parameter_count; ++column) {
    SEXP output = PROTECT(Rf_allocVector(output_type(specs[column].kind), rows));
    SET_VECTOR_ELT(table, column, output);
    SET_STRING_ELT(names, column, specs[column].id);
    UNPROTECT(1);
  }

  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(classes, 0, Rf_mkChar("data.table"));
  SET_STRING_ELT(classes, 1, Rf_mkChar("data.frame"));
  SEXP row_names = PROTECT(Rf_allocVector(INTSXP, rows == 0 ? 0 : 2));
  if (rows != 0) {
    SET_INTEGER_ELT(row_names, 0, NA_INTEGER);
    SET_INTEGER_ELT(row_names, 1, -(int) rows);
  }
  Rf_setAttrib(table, R_NamesSymbol, names);
  Rf_setAttrib(table, R_ClassSymbol, classes);
  Rf_setAttrib(table, R_RowNamesSymbol, row_names);

  SEXP result = PROTECT(paradox_prepare_data_table(table, TRUE));
  UNPROTECT(5);
  return result;
}

static void fill_table(SEXP table, const sampler_spec_t *specs,
    R_xlen_t parameter_count, R_xlen_t rows) {
  if (parameter_count == 0 || rows == 0) {
    return;
  }

  /* Everything that can allocate or error is complete before RNG entry.  The
   * loop deliberately has no interrupt poll: a longjmp between GetRNGstate()
   * and PutRNGstate() would lose the already-consumed stream. */
  GetRNGstate();
  for (R_xlen_t column = 0; column < parameter_count; ++column) {
    SEXP output = VECTOR_ELT(table, column);
    const sampler_spec_t *spec = &specs[column];
    switch (spec->kind) {
    case SAMPLER_DBL: {
      double *target = REAL(output);
      for (R_xlen_t row = 0; row < rows; ++row) {
        target[row] = paradox_qunif_double_value(
          unif_rand(),
          spec->lower,
          spec->upper
        );
      }
      break;
    }
    case SAMPLER_INT: {
      int *target = INTEGER(output);
      for (R_xlen_t row = 0; row < rows; ++row) {
        int mapped = NA_INTEGER;
        (void) paradox_qunif_integer_value(
          unif_rand(),
          spec->lower,
          spec->upper,
          &mapped
        );
        target[row] = mapped;
      }
      break;
    }
    case SAMPLER_FCT: {
      const R_xlen_t level_count = XLENGTH(spec->levels);
      for (R_xlen_t row = 0; row < rows; ++row) {
        const R_xlen_t selected = paradox_qunif_level_index(
          unif_rand(),
          level_count
        );
        SET_STRING_ELT(output, row, STRING_ELT(spec->levels, selected));
      }
      break;
    }
    case SAMPLER_LGL: {
      int *target = LOGICAL(output);
      for (R_xlen_t row = 0; row < rows; ++row) {
        target[row] = unif_rand() < 0.5;
      }
      break;
    }
    }
  }
  PutRNGstate();
}

SEXP paradox_sampler_unif_subspace_handoffs(SEXP param_set) {
  /*
   * Issue every subset bundle and sampler-specific carrier in one native
   * operation over the caller's already private owned graph. The protected
   * subset tokens have never been exposed through a public ParamSet shell, so
   * ownership can safely move into the child sampler. A non-NULL process-local
   * address makes the carrier impossible to fabricate or restore through
   * serialization from R.
   */
  SEXP private_environment = PROTECT(
    paradox_domain_private_environment(param_set)
  );
  if (private_environment == R_UnboundValue) {
    UNPROTECT(1);
    Rf_error("`param_set` must be a current ParamSet object");
  }
  SEXP bundles = PROTECT(paradox_param_set_all_subspace_states(
    private_environment,
    param_set
  ));
  if (TYPEOF(bundles) != VECSXP || ALTREP(bundles) ||
      Rf_isS4(bundles)) {
    UNPROTECT(2);
    Rf_error("Internal error: invalid SamplerUnif subspace bundle list");
  }
  SEXP names = PROTECT(Rf_getAttrib(bundles, R_NamesSymbol));
  if (TYPEOF(names) != STRSXP || ALTREP(names) || Rf_isS4(names) ||
      XLENGTH(names) != XLENGTH(bundles) ||
      !paradox_api_has_no_attributes(names)) {
    UNPROTECT(3);
    Rf_error("Internal error: invalid SamplerUnif subspace bundle names");
  }

  const R_xlen_t count = XLENGTH(bundles);
  SEXP handoffs = PROTECT(Rf_allocVector(VECSXP, count));
  for (R_xlen_t index = 0; index < count; ++index) {
    SEXP bundle = VECTOR_ELT(bundles, index);
    if (TYPEOF(bundle) != VECSXP || ALTREP(bundle) ||
        Rf_isS4(bundle) || XLENGTH(bundle) != 4) {
      UNPROTECT(4);
      Rf_error("Internal error: invalid SamplerUnif subspace bundle");
    }
    SEXP handoff = PROTECT(R_MakeExternalPtr(
      (void *) &sampler_unif_handoff_identity,
      sampler_unif_handoff_tag(),
      bundle
    ));
    SET_VECTOR_ELT(handoffs, index, handoff);
    UNPROTECT(1);
  }
  Rf_setAttrib(handoffs, R_NamesSymbol, names);
  UNPROTECT(4);
  return handoffs;
}

SEXP paradox_sampler_unif_take_subspace(SEXP handoff) {
  if (TYPEOF(handoff) != EXTPTRSXP ||
      R_ExternalPtrTag(handoff) != sampler_unif_handoff_tag()) {
    return R_NilValue;
  }
  if (Rf_isS4(handoff) || !paradox_api_has_no_attributes(handoff) ||
      R_ExternalPtrAddr(handoff) !=
        (void *) &sampler_unif_handoff_identity) {
    Rf_error(
      "Invalid, malformed, or already consumed internal "
      "SamplerUnif subspace handoff"
    );
  }

  SEXP bundle = PROTECT(R_ExternalPtrProtected(handoff));
  if (TYPEOF(bundle) != VECSXP || ALTREP(bundle) ||
      Rf_isS4(bundle) || XLENGTH(bundle) != 4) {
    UNPROTECT(1);
    Rf_error("Malformed internal SamplerUnif subspace handoff");
  }

  /*
   * Keep the tag after clearing the address so reuse and serialized carriers
   * fail with the specific handoff diagnostic. The protected subset bundle is
   * returned exactly once and its token is itself consumed by ParamSet$new().
   */
  R_SetExternalPtrProtected(handoff, R_NilValue);
  R_ClearExternalPtr(handoff);
  UNPROTECT(1);
  return bundle;
}

SEXP paradox_sampler_unif_sample_builtin(SEXP param_set, SEXP n) {
  PROTECT(param_set);

  /* Materialize the only semantic public vector before choosing the capsule
   * snapshot.  An ALTREP Elt callback may legitimately mutate the ParamSet;
   * that mutation is therefore included in the operation snapshot below. */
  const R_xlen_t requested_rows = parse_row_count(n);

  SEXP private_environment = PROTECT(
    paradox_domain_private_environment(param_set)
  );
  if (private_environment == R_UnboundValue) {
    UNPROTECT(2);
    Rf_error("`param_set` must be a current ParamSet object");
  }
  PROTECT_INDEX core_index;
  SEXP core;
  PROTECT_WITH_INDEX(
    core = paradox_core_from_private(private_environment),
    &core_index
  );
  if (core == R_UnboundValue) {
    UNPROTECT(3);
    Rf_error("Corrupt ParamSet sampling state: missing core capsule");
  }

  R_xlen_t work_since_interrupt = 0;
  paradox_domain_params_t params;
  R_xlen_t unused_row = 0;
  PROTECT_INDEX roots_index;
  SEXP roots;
  PROTECT_WITH_INDEX(roots = R_NilValue, &roots_index);
  const paradox_core_kind_t kind = paradox_core_kind(core);
  if (kind == PARADOX_CORE_COLLECTION) {
    paradox_collection_graph_t graph;
    paradox_collection_graph_build(
      private_environment,
      param_set,
      &graph,
      &roots,
      roots_index,
      &work_since_interrupt
    );
    params = graph.nodes[0].params;
  } else {
    if (!paradox_core_is_verified(core)) {
      REPROTECT(
        core = paradox_core_refresh(param_set, private_environment),
        core_index
      );
    }
    const paradox_core_kind_t sampled_kind = paradox_core_kind(core);
    if (sampled_kind != PARADOX_CORE_BASE &&
        sampled_kind != PARADOX_CORE_SHADOW) {
      UNPROTECT(4);
      Rf_error("Corrupt ParamSet sampling state: unknown core kind");
    }
    SEXP state = PROTECT(paradox_core_payload(core));
    if (!paradox_core_state_exact_schema(state) ||
      !paradox_domain_validate_params(
        VECTOR_ELT(state, PARADOX_CORE_PARAMS),
        R_NilValue,
        TRUE,
        &params,
        &unused_row,
        &work_since_interrupt
      )) {
      UNPROTECT(5);
      Rf_error("Corrupt ParamSet sampling state: invalid parameter schema");
    }
    UNPROTECT(1);
  }

  const R_xlen_t parameter_count = params.row_count;
  if (parameter_count > INT_MAX) {
    UNPROTECT(4);
    Rf_error("ParamSet is too large to sample into a data.frame");
  }
  sampler_spec_t *specs = paradox_temporary_alloc(
    parameter_count,
    sizeof(*specs)
  );
  capture_specs(&params, specs, requested_rows, &work_since_interrupt);

  /* A zero-dimensional Design has zero rows for compatibility, independent
   * of the requested count, and never creates or advances .Random.seed. */
  const R_xlen_t output_rows = parameter_count == 0 ? 0 : requested_rows;
  SEXP result = PROTECT(allocate_table(specs, parameter_count, output_rows));
  fill_table(result, specs, parameter_count, output_rows);
  UNPROTECT(5);
  return result;
}

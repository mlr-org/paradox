#include <limits.h>
#include <stdint.h>
#include <string.h>

#include "paradox.h"

#include "parameter_suggestion.h"
#include "r_utils.h"

/*
 * This is the intended Paradox 1 suggestion policy, implemented without an R
 * callback: case-insensitive partial Levenshtein distance, a 20% query-length
 * threshold, and at most three stable suggestions. Partial matching makes
 * collection-generated affixes cheap (an exact local ID inside a prefixed
 * public ID has distance zero).
 *
 * The helper is entered only after an exact ID lookup has failed. Pathological
 * diagnostic strings get the useful base error without spending unbounded
 * memory or CPU on an optional hint.
 */

enum {
  SUGGESTION_COUNT = 3,
  SUGGESTION_QUERY_CODEPOINT_LIMIT = 4096,
  SUGGESTION_QUERY_BYTE_LIMIT = 16384,
  SUGGESTION_CANDIDATE_BYTE_LIMIT = 1048576,
  SUGGESTION_WORK_LIMIT = 8388608
};

typedef struct {
  int distance;
  R_xlen_t row;
} suggestion_t;

static int utf8_continuation(unsigned char byte) {
  return byte >= 0x80U && byte <= 0xbfU;
}

static int next_utf8_codepoint(const unsigned char **cursor,
    const unsigned char *end, uint32_t *codepoint) {
  const unsigned char *text = *cursor;
  if (text == end) return FALSE;

  const unsigned char first = *text++;
  if (first <= 0x7fU) {
    *codepoint = first;
    *cursor = text;
    return TRUE;
  }

  if (first >= 0xc2U && first <= 0xdfU &&
      end - text >= 1 && utf8_continuation(text[0])) {
    *codepoint = ((uint32_t) (first & 0x1fU) << 6U) |
      (uint32_t) (text[0] & 0x3fU);
    *cursor = text + 1;
    return TRUE;
  }

  if (first >= 0xe0U && first <= 0xefU && end - text >= 2 &&
      utf8_continuation(text[1])) {
    const unsigned char second = text[0];
    if ((first == 0xe0U && second >= 0xa0U && second <= 0xbfU) ||
        (first == 0xedU && second >= 0x80U && second <= 0x9fU) ||
        (first != 0xe0U && first != 0xedU &&
          utf8_continuation(second))) {
      *codepoint = ((uint32_t) (first & 0x0fU) << 12U) |
        ((uint32_t) (second & 0x3fU) << 6U) |
        (uint32_t) (text[1] & 0x3fU);
      *cursor = text + 2;
      return TRUE;
    }
  }

  if (first >= 0xf0U && first <= 0xf4U && end - text >= 3 &&
      utf8_continuation(text[1]) && utf8_continuation(text[2])) {
    const unsigned char second = text[0];
    if ((first == 0xf0U && second >= 0x90U && second <= 0xbfU) ||
        (first == 0xf4U && second >= 0x80U && second <= 0x8fU) ||
        (first != 0xf0U && first != 0xf4U &&
          utf8_continuation(second))) {
      *codepoint = ((uint32_t) (first & 0x07U) << 18U) |
        ((uint32_t) (second & 0x3fU) << 12U) |
        ((uint32_t) (text[1] & 0x3fU) << 6U) |
        (uint32_t) (text[2] & 0x3fU);
      *cursor = text + 3;
      return TRUE;
    }
  }

  Rf_error("Internal error: invalid UTF-8 parameter identifier");
  return FALSE;
}

static uint32_t fold_identifier_codepoint(uint32_t codepoint) {
  /* Parameter IDs are overwhelmingly ASCII. Keep folding deterministic and
   * locale-independent while comparing every non-ASCII Unicode scalar
   * exactly. */
  return codepoint >= (uint32_t) 'A' && codepoint <= (uint32_t) 'Z'
    ? codepoint + ((uint32_t) 'a' - (uint32_t) 'A')
    : codepoint;
}

static int decode_query(SEXP id, uint32_t **query, int *query_length) {
  if (LENGTH(id) > SUGGESTION_QUERY_BYTE_LIMIT) return FALSE;

  const void *translation_watermark = vmaxget();
  const char *translated = Rf_translateCharUTF8(id);
  const size_t byte_count = strlen(translated);
  if (byte_count > SUGGESTION_QUERY_BYTE_LIMIT) {
    vmaxset(translation_watermark);
    return FALSE;
  }

  const unsigned char *cursor = (const unsigned char *) translated;
  const unsigned char *end = cursor + byte_count;
  int count = 0;
  uint32_t ignored = 0;
  while (cursor != end) {
    (void) next_utf8_codepoint(&cursor, end, &ignored);
    if (count == SUGGESTION_QUERY_CODEPOINT_LIMIT) {
      vmaxset(translation_watermark);
      return FALSE;
    }
    ++count;
  }
  vmaxset(translation_watermark);

  uint32_t *decoded = paradox_temporary_alloc(
    count == 0 ? 1 : (R_xlen_t) count,
    sizeof(*decoded)
  );
  translation_watermark = vmaxget();
  translated = Rf_translateCharUTF8(id);
  cursor = (const unsigned char *) translated;
  end = cursor + strlen(translated);
  for (int index = 0; index < count; ++index) {
    uint32_t codepoint = 0;
    if (!next_utf8_codepoint(&cursor, end, &codepoint)) {
      Rf_error("Internal error: truncated UTF-8 parameter identifier");
    }
    decoded[index] = fold_identifier_codepoint(codepoint);
  }
  if (cursor != end) {
    Rf_error("Internal error: unstable UTF-8 parameter identifier");
  }
  vmaxset(translation_watermark);

  *query = decoded;
  *query_length = count;
  return TRUE;
}

static int capped_increment(int value, int limit) {
  return value < limit ? value + 1 : limit;
}

static int partial_edit_distance(const uint32_t *query, int query_length,
    SEXP candidate, int threshold, int *costs, uint64_t *work) {
  if (LENGTH(candidate) > SUGGESTION_CANDIDATE_BYTE_LIMIT) {
    return threshold + 1;
  }
  const char *translated = Rf_translateCharUTF8(candidate);
  const size_t byte_count = strlen(translated);
  if (byte_count > SUGGESTION_CANDIDATE_BYTE_LIMIT) {
    return threshold + 1;
  }

  const int limit = threshold + 1;
  for (int index = 0; index <= query_length; ++index) {
    costs[index] = index < limit ? index : limit;
  }
  int best = costs[query_length];
  if (best == 0) return 0;

  const unsigned char *cursor = (const unsigned char *) translated;
  const unsigned char *end = cursor + byte_count;
  while (cursor != end) {
    if (*work > (uint64_t) SUGGESTION_WORK_LIMIT -
        (uint64_t) query_length) {
      *work = (uint64_t) SUGGESTION_WORK_LIMIT;
      return limit;
    }
    *work += (uint64_t) query_length;

    uint32_t candidate_codepoint = 0;
    (void) next_utf8_codepoint(&cursor, end, &candidate_codepoint);
    candidate_codepoint = fold_identifier_codepoint(candidate_codepoint);

    int diagonal = costs[0];
    costs[0] = 0; /* A candidate prefix is outside the partial match. */
    for (int query_index = 1; query_index <= query_length; ++query_index) {
      const int previous_column = costs[query_index];
      const int deletion = capped_increment(
        costs[query_index - 1],
        limit
      );
      const int insertion = capped_increment(previous_column, limit);
      int substitution = diagonal;
      if (query[query_index - 1] != candidate_codepoint) {
        substitution = capped_increment(substitution, limit);
      }
      int distance = deletion < insertion ? deletion : insertion;
      if (substitution < distance) distance = substitution;
      costs[query_index] = distance;
      diagonal = previous_column;
    }
    if (costs[query_length] < best) {
      best = costs[query_length];
      if (best == 0) return 0;
    }
  }
  return best;
}

static void insert_suggestion(suggestion_t *suggestions, int *count,
    int distance, R_xlen_t row) {
  int position = 0;
  while (position < *count &&
      suggestions[position].distance <= distance) {
    ++position;
  }
  if (position >= SUGGESTION_COUNT) return;

  const int new_count = *count < SUGGESTION_COUNT
    ? *count + 1
    : *count;
  for (int index = new_count - 1; index > position; --index) {
    suggestions[index] = suggestions[index - 1];
  }
  suggestions[position].distance = distance;
  suggestions[position].row = row;
  *count = new_count;
}

static int find_suggestions(SEXP id, SEXP candidate_ids,
    suggestion_t *suggestions) {
  if (Rf_getCharCE(id) == CE_BYTES) return 0;

  SEXP safe_id = PROTECT(paradox_diagnostic_charsxp(id));
  uint32_t *query = NULL;
  int query_length = 0;
  if (!decode_query(safe_id, &query, &query_length)) {
    UNPROTECT(1);
    return 0;
  }

  const int threshold = query_length / 5 +
    (query_length % 5 != 0);
  int *costs = paradox_temporary_alloc(
    (R_xlen_t) query_length + 1,
    sizeof(*costs)
  );
  int suggestion_count = 0;
  uint64_t work = 0;
  const R_xlen_t candidate_count = XLENGTH(candidate_ids);
  for (R_xlen_t row = 0; row < candidate_count; ++row) {
    if (work >= (uint64_t) SUGGESTION_WORK_LIMIT) break;
    ++work; /* Bound candidate visits even when no DP cells are evaluated. */
    if (row != 0 &&
        row % PARADOX_INTERRUPT_CHECK_INTERVAL == 0) {
      R_CheckUserInterrupt();
    }
    SEXP candidate = STRING_ELT(candidate_ids, row);
    if (candidate == NA_STRING || Rf_getCharCE(candidate) == CE_BYTES) {
      continue;
    }
    /* Take the watermark first: a candidate whose UTF-8 translation is
     * invalid makes `paradox_diagnostic_charsxp()` build an escaped copy in
     * R_alloc memory, and that copy has to be released with the distance
     * scratch rather than accumulate for the whole call. */
    const void *translation_watermark = vmaxget();
    SEXP safe_candidate = PROTECT(paradox_diagnostic_charsxp(candidate));
    const int distance = partial_edit_distance(
      query,
      query_length,
      safe_candidate,
      threshold,
      costs,
      &work
    );
    vmaxset(translation_watermark);
    UNPROTECT(1);
    if (distance <= threshold) {
      insert_suggestion(
        suggestions,
        &suggestion_count,
        distance,
        row
      );
      if (suggestion_count == SUGGESTION_COUNT &&
          suggestions[SUGGESTION_COUNT - 1].distance == 0) {
        break;
      }
    }
    if (work >= (uint64_t) SUGGESTION_WORK_LIMIT) break;
  }
  UNPROTECT(1);
  return suggestion_count;
}

static SEXP format_unavailable(SEXP id, SEXP candidate_ids,
    const suggestion_t *suggestions, int suggestion_count,
    const char *location) {
  SEXP roots = PROTECT(Rf_allocVector(
    STRSXP,
    (R_xlen_t) suggestion_count + 1
  ));
  SEXP safe = PROTECT(paradox_diagnostic_charsxp(id));
  SET_STRING_ELT(roots, 0, safe);
  UNPROTECT(1);
  for (int index = 0; index < suggestion_count; ++index) {
    safe = PROTECT(paradox_diagnostic_charsxp(STRING_ELT(
      candidate_ids,
      suggestions[index].row
    )));
    SET_STRING_ELT(roots, (R_xlen_t) index + 1, safe);
    UNPROTECT(1);
  }

  paradox_utf8_piece_t pieces[6 + 2 * SUGGESTION_COUNT];
  R_xlen_t piece_count = 0;
  pieces[piece_count++] = paradox_utf8_ascii_piece("Parameter '");
  pieces[piece_count++] = paradox_utf8_charsxp_piece(
    STRING_ELT(roots, 0)
  );
  pieces[piece_count++] = paradox_utf8_ascii_piece("' not available");
  pieces[piece_count++] = paradox_utf8_ascii_piece(location);
  /* The one period this diagnostic owns separates its two sentences.  A
   * terminating period is the assertion wrapper's to add, and adding one here
   * as well is what produced `not available..`; see
   * `paradox_charsxp_ends_sentence()`. */
  if (suggestion_count != 0) {
    pieces[piece_count++] = paradox_utf8_ascii_piece(". Did you mean '");
    pieces[piece_count++] = paradox_utf8_charsxp_piece(
      STRING_ELT(roots, 1)
    );
    for (int index = 1; index < suggestion_count; ++index) {
      pieces[piece_count++] = paradox_utf8_ascii_piece("' / '");
      pieces[piece_count++] = paradox_utf8_charsxp_piece(
        STRING_ELT(roots, (R_xlen_t) index + 1)
      );
    }
    pieces[piece_count++] = paradox_utf8_ascii_piece("'?");
  }
  if (piece_count > (R_xlen_t) (sizeof(pieces) / sizeof(*pieces))) {
    UNPROTECT(1);
    Rf_error("Internal error: too many parameter suggestions");
  }

  SEXP result = PROTECT(paradox_utf8_message(pieces, piece_count));
  UNPROTECT(2);
  return result;
}

SEXP paradox_parameter_unavailable_diagnostic(SEXP id, SEXP candidate_ids,
    const char *location) {
  PROTECT(id);
  PROTECT(candidate_ids);
  if (TYPEOF(id) != CHARSXP || id == NA_STRING ||
      TYPEOF(candidate_ids) != STRSXP || ALTREP(candidate_ids) ||
      location == NULL) {
    UNPROTECT(2);
    Rf_error("Internal error: invalid parameter suggestion input");
  }

  suggestion_t suggestions[SUGGESTION_COUNT];
  const int suggestion_count = find_suggestions(
    id,
    candidate_ids,
    suggestions
  );
  SEXP result = PROTECT(format_unavailable(
    id,
    candidate_ids,
    suggestions,
    suggestion_count,
    location
  ));
  UNPROTECT(3);
  return result;
}

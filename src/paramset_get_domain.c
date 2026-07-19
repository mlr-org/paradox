#include "paradox.h"

/* `$domains` and `$get_domain()` deliberately share one capsule snapshot
 * engine.  Keeping this registered entry point as a tiny forwarding unit
 * preserves the public/native symbol while avoiding a second implementation. */
SEXP paradox_param_set_get_domain(SEXP private_environment, SEXP self,
    SEXP id) {
  return paradox_param_set_domains_select(private_environment, self, id);
}

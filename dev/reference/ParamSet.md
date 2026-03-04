# ParamSet

An object representing the space of possible parametrizations of a
function or another object. `ParamSet`s are used on the side of objects
being parameterized, where they function as a configuration space
determining the set of possible configurations accepted by these
objects. They can also be used to specify search spaces for
optimization, indicating the set of legal configurations to try out. It
is often convenient to generate search spaces from configuration spaces,
which can be done using the `$search_space()` method in combination with
[`to_tune()`](https://paradox.mlr-org.com/dev/reference/to_tune.md) /
[`TuneToken`](https://paradox.mlr-org.com/dev/reference/to_tune.md)
objects.

Individual dimensions of a `ParamSet` are specified by
[`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md) objects,
created as
[`p_dbl()`](https://paradox.mlr-org.com/dev/reference/Domain.md),
[`p_lgl()`](https://paradox.mlr-org.com/dev/reference/Domain.md) etc.
The field `$values` can be used to store an active configuration or to
partially fix some parameters to constant values – the precise effect
can be determined by the object being parameterized.

Constructing a `ParamSet` can be done using `ParamSet$new()` in
combination with a named list of
[`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md) objects.
This route is recommended when the set of dimensions (i.e. the members
of this named list) is dynamically created, such as when the number of
parameters is variable. `ParamSet`s can also be created using the
[`ps()`](https://paradox.mlr-org.com/dev/reference/ps.md) shorthand,
which is the recommended way when the set of parameters is fixed. In
practice, the majority of cases where a `ParamSet` is created, the
[`ps()`](https://paradox.mlr-org.com/dev/reference/ps.md) should be
used.

## S3 methods and type converters

- [`as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html)  
  `ParamSet` -\>
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)  
  Compact representation as datatable. Col types are:  

  - id: character

  - class: character

  - lower, upper: numeric

  - levels: list col, with NULL elements

  - nlevels: integer valued numeric

  - is_bounded: logical

  - special_vals: list col of list

  - default: list col

  - storage_type: character

  - tags: list col of character vectors

## Public fields

- `assert_values`:

  (`logical(1)`)  
  Should values be checked for validity during assigment to active
  binding `$values`? Default is `TRUE`, only switch this off if you know
  what you are doing.

## Active bindings

- `data`:

  (`data.table`) `data.table` representation of the `ParamSet`.

- `values`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Currently set / fixed parameter values. Settable, and feasibility of
  values will be checked when you set them. You do not have to set
  values for all parameters, but only for a subset. When you set values,
  all previously set values will be unset / removed.

- `tags`:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [`character()`](https://rdrr.io/r/base/character.html))  
  Can be used to group and subset parameters. Named with parameter IDs.

- `params`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  `data.table` representing the combined
  [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md)
  objects used to construct the `ParamSet`. Used for internal purpuses.
  Its use by external code is deprecated.

- `domains`:

  (named `list` of
  [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md)) List
  of [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md)
  objects that could be used to initialize this `ParamSet`.

- `extra_trafo`:

  (`function(x, param_set)`)  
  Transformation function. Settable. User has to pass a `function(x)`,
  of the form  
  (named [`list()`](https://rdrr.io/r/base/list.html), ParamSet) -\>
  named [`list()`](https://rdrr.io/r/base/list.html).  
  The function is responsible to transform a feasible configuration into
  another encoding, before potentially evaluating the configuration with
  the target algorithm. For the output, not many things have to hold. It
  needs to have unique names, and the target algorithm has to accept the
  configuration. For convenience, the self-paramset is also passed in,
  if you need some info from it (e.g. tags). Is NULL by default, and you
  can set it to NULL to switch the transformation off.

- `constraint`:

  (`function(x)`)  
  Constraint function. Settable. This function must evaluate a named
  [`list()`](https://rdrr.io/r/base/list.html) of values and determine
  whether it satisfies constraints, returning a scalar `logical(1)`
  value.

- `deps`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Table has cols `id` (`character(1)`) and `on` (`character(1)`) and
  `cond`
  ([Condition](https://paradox.mlr-org.com/dev/reference/Condition.md)).
  Lists all (direct) dependency parents of a param, through parameter
  IDs. Internally created by a call to `add_dep`. Settable, if you want
  to remove dependencies or perform other changes.

- `length`:

  (`integer(1)`)  
  Number of contained parameters.

- `is_empty`:

  (`logical(1)`)  
  Is the `ParamSet` empty? Named with parameter IDs.

- `has_trafo`:

  (`logical(1)`)  
  Whether a `trafo` function is present, in parameters or in
  `extra_trafo`.

- `has_extra_trafo`:

  (`logical(1)`)  
  Whether `extra_trafo` is set.

- `has_deps`:

  (`logical(1)`)  
  Whether the parameter dependencies are present

- `has_constraint`:

  (`logical(1)`)  
  Whether parameter constraint is set.

- `all_numeric`:

  (`logical(1)`)  
  Is `TRUE` if all parameters are
  [`p_dbl()`](https://paradox.mlr-org.com/dev/reference/Domain.md) or
  [`p_int()`](https://paradox.mlr-org.com/dev/reference/Domain.md).

- `all_categorical`:

  (`logical(1)`)  
  Is `TRUE` if all parameters are
  [`p_fct()`](https://paradox.mlr-org.com/dev/reference/Domain.md) and
  [`p_lgl()`](https://paradox.mlr-org.com/dev/reference/Domain.md).

- `all_bounded`:

  (`logical(1)`)  
  Is `TRUE` if all parameters are bounded.

- `class`:

  (named [`character()`](https://rdrr.io/r/base/character.html))  
  Classes of contained parameters. Named with parameter IDs.

- `lower`:

  (named [`double()`](https://rdrr.io/r/base/double.html))  
  Lower bounds of numeric parameters (`NA` for non-numerics). Named with
  parameter IDs.

- `upper`:

  (named [`double()`](https://rdrr.io/r/base/double.html))  
  Upper bounds of numeric parameters (`NA` for non-numerics). Named with
  parameter IDs.

- `levels`:

  (named [`list()`](https://rdrr.io/r/base/list.html) of `character`)  
  Allowed levels of categorical parameters (`NULL` for
  non-categoricals). Named with parameter IDs.

- `storage_type`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Data types of parameters when stored in tables. Named with parameter
  IDs.

- `special_vals`:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [`list()`](https://rdrr.io/r/base/list.html))  
  Special values for all parameters. Named with parameter IDs.

- `default`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Default values of all parameters. If no default exists, element is not
  present. Named with parameter IDs.

- `has_trafo_param`:

  ([`logical()`](https://rdrr.io/r/base/logical.html))  
  Whether `trafo` is set for any parameter.

- `is_logscale`:

  ([`logical()`](https://rdrr.io/r/base/logical.html))  
  Whether `trafo` was set to `logscale` during construction.  
  Note that this only refers to the `logscale` flag set during
  construction, e.g. `p_dbl(logscale = TRUE)`. If the parameter was set
  to logscale manually, e.g. through `p_dbl(trafo = exp)`, this
  `is_logscale` will be `FALSE`.

- `nlevels`:

  (named [`integer()`](https://rdrr.io/r/base/integer.html))  
  Number of distinct levels of parameters. `Inf` for double parameters
  or unbounded integer parameters. Named with param IDs.

- `is_number`:

  (named [`logical()`](https://rdrr.io/r/base/logical.html))  
  Whether parameter is
  [`p_dbl()`](https://paradox.mlr-org.com/dev/reference/Domain.md) or
  [`p_int()`](https://paradox.mlr-org.com/dev/reference/Domain.md).
  Named with parameter IDs.

- `is_categ`:

  (named [`logical()`](https://rdrr.io/r/base/logical.html))  
  Whether parameter is
  [`p_fct()`](https://paradox.mlr-org.com/dev/reference/Domain.md) or
  [`p_lgl()`](https://paradox.mlr-org.com/dev/reference/Domain.md).
  Named with parameter IDs.

- `is_bounded`:

  (named [`logical()`](https://rdrr.io/r/base/logical.html))  
  Whether parameters have finite bounds. Named with parameter IDs.

## Methods

### Public methods

- [`ParamSet$new()`](#method-ParamSet-new)

- [`ParamSet$ids()`](#method-ParamSet-ids)

- [`ParamSet$get_values()`](#method-ParamSet-get_values)

- [`ParamSet$set_values()`](#method-ParamSet-set_values)

- [`ParamSet$trafo()`](#method-ParamSet-trafo)

- [`ParamSet$aggr_internal_tuned_values()`](#method-ParamSet-aggr_internal_tuned_values)

- [`ParamSet$disable_internal_tuning()`](#method-ParamSet-disable_internal_tuning)

- [`ParamSet$convert_internal_search_space()`](#method-ParamSet-convert_internal_search_space)

- [`ParamSet$test_constraint()`](#method-ParamSet-test_constraint)

- [`ParamSet$test_constraint_dt()`](#method-ParamSet-test_constraint_dt)

- [`ParamSet$check()`](#method-ParamSet-check)

- [`ParamSet$check_dependencies()`](#method-ParamSet-check_dependencies)

- [`ParamSet$test()`](#method-ParamSet-test)

- [`ParamSet$assert()`](#method-ParamSet-assert)

- [`ParamSet$check_dt()`](#method-ParamSet-check_dt)

- [`ParamSet$test_dt()`](#method-ParamSet-test_dt)

- [`ParamSet$assert_dt()`](#method-ParamSet-assert_dt)

- [`ParamSet$qunif()`](#method-ParamSet-qunif)

- [`ParamSet$get_domain()`](#method-ParamSet-get_domain)

- [`ParamSet$subset()`](#method-ParamSet-subset)

- [`ParamSet$subspaces()`](#method-ParamSet-subspaces)

- [`ParamSet$flatten()`](#method-ParamSet-flatten)

- [`ParamSet$search_space()`](#method-ParamSet-search_space)

- [`ParamSet$add_dep()`](#method-ParamSet-add_dep)

- [`ParamSet$format()`](#method-ParamSet-format)

- [`ParamSet$print()`](#method-ParamSet-print)

- [`ParamSet$clone()`](#method-ParamSet-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ParamSet$new(params = named_list(), allow_dangling_dependencies = FALSE)

#### Arguments

- `params`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  List of
  [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md), named
  with their respective ID.

- `allow_dangling_dependencies`:

  (`character(1)`)  
  Whether dependencies depending on parameters that are not present
  should be allowed. A parameter `x` having `depends = y == 0` if `y` is
  not present would usually throw an error, but if dangling dependencies
  are allowed, the dependency is added regardless. This is mainly for
  internal use.

------------------------------------------------------------------------

### Method `ids()`

Retrieves IDs of contained parameters based on some filter criteria
selections, `NULL` means no restriction. Only returns IDs of parameters
that satisfy all conditions.

#### Usage

    ParamSet$ids(class = NULL, tags = NULL, any_tags = NULL)

#### Arguments

- `class`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Typically a subset of `"ParamDbl"`, `"ParamInt"`, `"ParamFct"`,
  `"ParamLgl"`, `"ParamUty"`. Other classes are possible if implemented
  by 3rd party packages. Return only IDs of dimensions with the given
  class.

- `tags`:

  ([`character()`](https://rdrr.io/r/base/character.html)). Return only
  IDs of dimensions that have *all* tags given in this argument.

- `any_tags`:

  ([`character()`](https://rdrr.io/r/base/character.html)). Return only
  IDs of dimensions that have at least one of the tags given in this
  argument.

#### Returns

[`character()`](https://rdrr.io/r/base/character.html).

------------------------------------------------------------------------

### Method `get_values()`

Retrieves parameter values based on some selections, `NULL` means no
restriction and is equivalent to `$values`. Only returns values of
parameters that satisfy all conditions.

#### Usage

    ParamSet$get_values(
      class = NULL,
      tags = NULL,
      any_tags = NULL,
      type = "with_token",
      check_required = TRUE,
      remove_dependencies = TRUE
    )

#### Arguments

- `class`:

  ([`character()`](https://rdrr.io/r/base/character.html)). See
  `$ids()`.

- `tags`:

  ([`character()`](https://rdrr.io/r/base/character.html)). See
  `$ids()`.

- `any_tags`:

  ([`character()`](https://rdrr.io/r/base/character.html)). See
  `$ids()`.

- `type`:

  (`character(1)`)  
  Return values `"with_token"` (i.e. all values),

- `check_required`:

  (`logical(1)`)  
  Check if all required parameters are set?

- `remove_dependencies`:

  (`logical(1)`)  
  If `TRUE`, set values with dependencies that are not fulfilled to
  `NULL`.

#### Returns

Named [`list()`](https://rdrr.io/r/base/list.html).

------------------------------------------------------------------------

### Method `set_values()`

Allows to to modify (and overwrite) or replace the parameter values. Per
default already set values are being kept unless new values are being
provided.

#### Usage

    ParamSet$set_values(..., .values = list(), .insert = TRUE)

#### Arguments

- `...`:

  (any)  
  Named parameter values.

- `.values`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Named list with parameter values. Names must not already appear in
  `...`.

- `.insert`:

  (`logical(1)`)  
  Whether to insert the values (old values are being kept, if not
  overwritten), or to replace all values. Default is TRUE.

------------------------------------------------------------------------

### Method `trafo()`

Perform transformation specified by the `trafo` of
[`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md) objects,
as well as the `$extra_trafo` field.

#### Usage

    ParamSet$trafo(x, param_set = self)

#### Arguments

- `x`:

  (named [`list()`](https://rdrr.io/r/base/list.html) \| `data.frame`)  
  The value(s) to be transformed.

- `param_set`:

  (`ParamSet`)  
  Passed to `extra_trafo()`. Note that the `extra_trafo` of `self` is
  used, not the `extra_trafo` of the `ParamSet` given in the `param_set`
  argument. In almost all cases, the default `param_set = self` should
  be used.

------------------------------------------------------------------------

### Method `aggr_internal_tuned_values()`

Aggregate parameter values according to their aggregation rules.

#### Usage

    ParamSet$aggr_internal_tuned_values(x)

#### Arguments

- `x`:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [`list()`](https://rdrr.io/r/base/list.html)s)  
  The value(s) to be aggregated. Names are parameter values. The
  aggregation function is selected based on the parameter.

#### Returns

(named [`list()`](https://rdrr.io/r/base/list.html))

------------------------------------------------------------------------

### Method `disable_internal_tuning()`

Set the parameter values so that internal tuning for the selected
parameters is disabled.

#### Usage

    ParamSet$disable_internal_tuning(ids)

#### Arguments

- `ids`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  The ids of the parameters for which to disable internal tuning.

#### Returns

`Self`

------------------------------------------------------------------------

### Method `convert_internal_search_space()`

Convert all parameters from the search space to parameter values using
the transformation given by `in_tune_fn`.

#### Usage

    ParamSet$convert_internal_search_space(search_space)

#### Arguments

- `search_space`:

  (`ParamSet`)  
  The internal search space.

#### Returns

(named [`list()`](https://rdrr.io/r/base/list.html))

------------------------------------------------------------------------

### Method `test_constraint()`

checkmate-like test-function. Takes a named list. Return `FALSE` if the
given `$constraint` is not satisfied, `TRUE` otherwise. Note this is
different from satisfying the bounds or types given by the `ParamSet`
itself: If `x` does not satisfy these, an error will be thrown, given
that `assert_value` is `TRUE`.

#### Usage

    ParamSet$test_constraint(x, assert_value = TRUE)

#### Arguments

- `x`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  The value to test.

- `assert_value`:

  (`logical(1)`)  
  Whether to verify that `x` satisfies the bounds and types given by
  this `ParamSet`. Should be `TRUE` unless this was already checked
  before.

#### Returns

`logical(1)`: Whether `x` satisfies the `$constraint`.

------------------------------------------------------------------------

### Method `test_constraint_dt()`

checkmate-like test-function. Takes a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html). For
each row, return `FALSE` if the given `$constraint` is not satisfied,
`TRUE` otherwise. Note this is different from satisfying the bounds or
types given by the `ParamSet` itself: If `x` does not satisfy these, an
error will be thrown, given that `assert_value` is `TRUE`.

#### Usage

    ParamSet$test_constraint_dt(x, assert_value = TRUE)

#### Arguments

- `x`:

  (`data.table`)  
  The values to test.

- `assert_value`:

  (`logical(1)`)  
  Whether to verify that `x` satisfies the bounds and types given by
  this `ParamSet`. Should be `TRUE` unless this was already checked
  before.

#### Returns

`logical`: For each row in `x`, whether it satisfies the `$constraint`.

------------------------------------------------------------------------

### Method `check()`

checkmate-like check-function. Takes a setting of parameters as a named
list. A point x is feasible, if it configures a subset of params in a
valid manner, i.e., the param type is correct, and param bounds,
constraints and dependencies are satisfied. Params for which
dependencies are not satisfied should not be part of `x` (and not set to
`NA`). Param dependencies and `$constraint` are not checked when
`check_strict` is `FALSE` (but data type and bounds are checked). This
is sometimes useful when you only want to check the validity of
individual params in intermediate objects. Use `presence = "all"` to
check that all parameters are present in `xs`, except for parameters
with unsatisfied dependencies. 'presence = "none"' is often useful when
you want to check the validity of settings you want to assign when
defaults are already present, 'presence = "all"' is often useful when
configurations are created but some algorithm from a search space param
set in optimization.

#### Usage

    ParamSet$check(
      xs,
      check_strict = TRUE,
      sanitize = FALSE,
      presence = "none",
      allow_token = TRUE
    )

#### Arguments

- `xs`:

  (named [`list()`](https://rdrr.io/r/base/list.html)).

- `check_strict`:

  (`logical(1)`)  
  Whether to check that constraints and dependencies are satisfied.

- `sanitize`:

  (`logical(1)`)  
  Whether to move values that are slightly outside bounds to valid
  values. These values are accepted independent of `sanitize` (depending
  on the `tolerance` arguments of
  [`p_dbl()`](https://paradox.mlr-org.com/dev/reference/Domain.md) and
  [`p_int()`](https://paradox.mlr-org.com/dev/reference/Domain.md)) . If
  `sanitize` is `TRUE`, the additional effect is that, should checks
  pass, the sanitized values of `xs` are added to the result as
  attribute `"sanitized"`.

- `presence`:

  (`character(1)`)  
  If `"none"` (default), no check is performed for the presence of
  parameters. If `"all"`, all parameters must be present in `xs`, except
  for parameters with unsatisfied dependencies. If `"required"`,
  parameters with the `"required"` tag must be present in `xs`, except
  for parameters with unsatisfied dependencies. For `"all"` and
  `"required"`, `TuneToken`s are not allowed to be present in `xs`.

- `allow_token`:

  (`logical(1)`)  
  Whether to allow `TuneToken`s to be present in `xs`. Default is
  `TRUE`.

#### Returns

If successful `TRUE`, if not a string with an error message.

------------------------------------------------------------------------

### Method `check_dependencies()`

checkmate-like check-function. Takes a named list. Checks that all
individual param dependencies are satisfied.

#### Usage

    ParamSet$check_dependencies(xs)

#### Arguments

- `xs`:

  (named [`list()`](https://rdrr.io/r/base/list.html)).

#### Returns

If successful `TRUE`, if not a string with an error message.

------------------------------------------------------------------------

### Method `test()`

checkmate-like test-function (s. `$check()`).

#### Usage

    ParamSet$test(xs, check_strict = TRUE, presence = "none", allow_token = TRUE)

#### Arguments

- `xs`:

  (named [`list()`](https://rdrr.io/r/base/list.html)).

- `check_strict`:

  (`logical(1)`)  
  Whether to check that constraints and dependencies are satisfied.

- `presence`:

  (`character(1)`)  
  If `"none"` (default), no check is performed for the presence of
  parameters. If `"all"`, all parameters must be present in `xs`, except
  for parameters with unsatisfied dependencies. If `"required"`,
  required parameters must be present in `xs`, except for parameters
  with unsatisfied dependencies. For `"all"` and `"required"`,
  `TuneToken`s are not allowed to be present in `xs`.

- `allow_token`:

  (`logical(1)`)  
  Whether to allow `TuneToken`s to be present in `xs`. Default is
  `TRUE`.

#### Returns

If successful `TRUE`, if not `FALSE`.

------------------------------------------------------------------------

### Method `assert()`

checkmate-like assert-function (s. `$check()`).

#### Usage

    ParamSet$assert(
      xs,
      check_strict = TRUE,
      presence = "none",
      .var.name = vname(xs),
      sanitize = FALSE,
      allow_token = TRUE
    )

#### Arguments

- `xs`:

  (named [`list()`](https://rdrr.io/r/base/list.html)).

- `check_strict`:

  (`logical(1)`)  
  Whether to check that constraints and dependencies are satisfied.

- `presence`:

  (`character(1)`)  
  If `"none"` (default), no check is performed for the presence of
  parameters. If `"all"`, all parameters must be present in `xs`, except
  for parameters with unsatisfied dependencies. If `"required"`,
  required parameters must be present in `xs`, except for parameters
  with unsatisfied dependencies. For `"all"` and `"required"`,
  `TuneToken`s are not allowed to be present in `xs`.

- `.var.name`:

  (`character(1)`)  
  Name of the checked object to print in error messages.  
  Defaults to the heuristic implemented in
  [vname](https://mllg.github.io/checkmate/reference/vname.html).

- `sanitize`:

  (`logical(1)`)  
  Whether to move values that are slightly outside bounds to valid
  values. These values are accepted independent of `sanitize` (depending
  on the `tolerance` arguments of
  [`p_dbl()`](https://paradox.mlr-org.com/dev/reference/Domain.md) and
  [`p_int()`](https://paradox.mlr-org.com/dev/reference/Domain.md)) . If
  `sanitize` is `TRUE`, the additional effect is that `xs` is converted
  to within bounds.

- `allow_token`:

  (`logical(1)`)  
  Whether to allow `TuneToken`s to be present in `xs`. Default is
  `TRUE`.

------------------------------------------------------------------------

### Method `check_dt()`

checkmate-like check-function. Takes a
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
where rows are points and columns are parameters. Checks in a similar
manner as `$check(xs)`. A point x is feasible, if it configures a subset
of params, all individual param constraints are satisfied and all
dependencies are satisfied. Params for which dependencies are not
satisfied should be set to `NA` in `xdt`. Dependencies and `$constraint`
are not checked when `check_strict` is `FALSE`. Note that checking only
subsets implies that `xdt` is therefore allowed to have fewer columns as
there are params in the set.

#### Usage

    ParamSet$check_dt(
      xdt,
      check_strict = TRUE,
      presence = "none",
      allow_token = TRUE
    )

#### Arguments

- `xdt`:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
  \| [`data.frame()`](https://rdrr.io/r/base/data.frame.html)).

- `check_strict`:

  (`logical(1)`)  
  Whether to check that constraints and dependencies are satisfied.

- `presence`:

  (`character(1)`)  
  If `"none"` (default), no check is performed for the presence of
  parameters. If `"all"`, all parameters must be present in `xdt` and
  not 'NA' if their dependencies are satisfied. If `"required"`,
  required parameters must be present in `xdt` and not 'NA' if their
  dependencies are satisfied. For `"all"` and `"required"`, `TuneToken`s
  are not allowed to be present in `xdt`.

- `allow_token`:

  (`logical(1)`)  
  Whether to allow `TuneToken`s to be present in `xdt`. Default is
  `TRUE`.

#### Returns

If successful `TRUE`, if not a string with the error message.

------------------------------------------------------------------------

### Method `test_dt()`

checkmate-like test-function (s. `$check_dt()`).

#### Usage

    ParamSet$test_dt(
      xdt,
      check_strict = TRUE,
      presence = "none",
      allow_token = TRUE
    )

#### Arguments

- `xdt`:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)).

- `check_strict`:

  (`logical(1)`)  
  Whether to check that constraints and dependencies are satisfied.

- `presence`:

  (`character(1)`)  
  If `"none"` (default), no check is performed for the presence of
  parameters. If `"all"`, all parameters must be present in `xdt` and
  not 'NA' if their dependencies are satisfied. If `"required"`,
  required parameters must be present in `xdt` and not 'NA' if their
  dependencies are satisfied. For `"all"` and `"required"`, `TuneToken`s
  are not allowed to be present in `xdt`.

- `allow_token`:

  (`logical(1)`)  
  Whether to allow `TuneToken`s to be present in `xdt`. Default is
  `TRUE`.

#### Returns

If successful `TRUE`, if not `FALSE`.

------------------------------------------------------------------------

### Method `assert_dt()`

checkmate-like assert-function (s. `$check_dt()`).

#### Usage

    ParamSet$assert_dt(
      xdt,
      check_strict = TRUE,
      presence = "none",
      .var.name = vname(xdt),
      allow_token = TRUE
    )

#### Arguments

- `xdt`:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)).

- `check_strict`:

  (`logical(1)`)  
  Whether to check that constraints and dependencies are satisfied.

- `presence`:

  (`character(1)`)  
  If `"none"` (default), no check is performed for the presence of
  parameters. If `"all"`, all parameters must be present in `xdt` and
  not 'NA' if their dependencies are satisfied. If `"required"`,
  required parameters must be present in `xdt` and not 'NA' if their
  dependencies are satisfied. For `"all"` and `"required"`, `TuneToken`s
  are not allowed to be present in `xdt`.

- `.var.name`:

  (`character(1)`)  
  Name of the checked object to print in error messages.  
  Defaults to the heuristic implemented in
  [vname](https://mllg.github.io/checkmate/reference/vname.html).

- `allow_token`:

  (`logical(1)`)  
  Whether to allow `TuneToken`s to be present in `xdt`. Default is
  `TRUE`.

#### Returns

If successful `xs` invisibly, if not, an error is generated.

------------------------------------------------------------------------

### Method [`qunif()`](https://rdrr.io/r/stats/Uniform.html)

Map a `matrix` or `data.frame` of values between 0 and 1 to proportional
values inside the feasible intervals of individual parameters.

#### Usage

    ParamSet$qunif(x)

#### Arguments

- `x`:

  (`matrix` \| `data.frame`)  
  Values to map. Column names must be a subset of the names of
  parameters.

#### Returns

`data.table`.

------------------------------------------------------------------------

### Method `get_domain()`

get the [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md)
object that could be used to create a given parameter.

#### Usage

    ParamSet$get_domain(id)

#### Arguments

- `id`:

  (`character(1)`).

#### Returns

[`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md).

------------------------------------------------------------------------

### Method [`subset()`](https://rdrr.io/r/base/subset.html)

Create a new `ParamSet` restricted to the passed IDs.

#### Usage

    ParamSet$subset(
      ids,
      allow_dangling_dependencies = FALSE,
      keep_constraint = TRUE
    )

#### Arguments

- `ids`:

  ([`character()`](https://rdrr.io/r/base/character.html)).

- `allow_dangling_dependencies`:

  (`logical(1)`)  
  Whether to allow subsets that cut across parameter dependencies.
  Dependencies that point to dropped parameters are kept (but will be
  "dangling", i.e. their `"on"` will not be present).

- `keep_constraint`:

  (`logical(1)`)  
  Whether to keep the `$constraint` function.

#### Returns

`ParamSet`.

------------------------------------------------------------------------

### Method `subspaces()`

Create new one-dimensional `ParamSet`s for each dimension.

#### Usage

    ParamSet$subspaces(ids = private$.params$id)

#### Arguments

- `ids`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  IDs for which to create `ParamSet`s. Defaults to all IDs.

#### Returns

named [`list()`](https://rdrr.io/r/base/list.html) of `ParamSet`.

------------------------------------------------------------------------

### Method `flatten()`

Create a `ParamSet` from this object, even if this object itself is not
a `ParamSet` but e.g. a
[`ParamSetCollection`](https://paradox.mlr-org.com/dev/reference/ParamSetCollection.md).

#### Usage

    ParamSet$flatten()

------------------------------------------------------------------------

### Method `search_space()`

Construct a `ParamSet` to tune over. Constructed from
[`TuneToken`](https://paradox.mlr-org.com/dev/reference/to_tune.md) in
`$values`, see
[`to_tune()`](https://paradox.mlr-org.com/dev/reference/to_tune.md).

#### Usage

    ParamSet$search_space(values = self$values)

#### Arguments

- `values`:

  (`named list`): optional named list of
  [`TuneToken`](https://paradox.mlr-org.com/dev/reference/to_tune.md)
  objects to convert, in place of `$values`.

------------------------------------------------------------------------

### Method `add_dep()`

Adds a dependency to this set, so that param `id` now depends on param
`on`.

#### Usage

    ParamSet$add_dep(id, on, cond, allow_dangling_dependencies = FALSE)

#### Arguments

- `id`:

  (`character(1)`).

- `on`:

  (`character(1)`).

- `cond`:

  ([Condition](https://paradox.mlr-org.com/dev/reference/Condition.md)).

- `allow_dangling_dependencies`:

  (`logical(1)`): Whether to allow dependencies on parameters that are
  not present.

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    ParamSet$format()

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    ParamSet$print(
      ...,
      hide_cols = c("levels", "is_bounded", "special_vals", "tags", "storage_type")
    )

#### Arguments

- `...`:

  (ignored).

- `hide_cols`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Which fields should not be printed? Default is `"levels"`,
  `"is_bounded"`, `"special_vals"`, `"tags"`, and `"storage_type"`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ParamSet$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
pset = ParamSet$new(
  params = list(
    d = p_dbl(lower = -5, upper = 5, default = 0, trafo = function(x) 2^x),
    f = p_fct(levels = letters[1:3])
  )
)

# alternative, recommended way of construction in this case since the
# parameter list is not dynamic:
pset = ps(
  d = p_dbl(lower = -5, upper = 5, default = 0, trafo = function(x) 2^x),
  f = p_fct(levels = letters[1:3])
)

pset$check(list(d = 2.1, f = "a"))
#> [1] TRUE

pset$check(list(d = 2.1, f = "d"))
#> [1] "f: Must be element of set {'a','b','c'}, but is 'd'"
```

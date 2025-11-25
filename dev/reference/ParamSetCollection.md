# ParamSetCollection

A collection of multiple
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)
objects.

- The collection is basically a light-weight wrapper / container around
  references to multiple sets.

- In order to ensure unique param names, every param in the collection
  is referred to with "\<set_id\>.\<param_id\>", where `<set_id>` is the
  name of the entry a given
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md) in
  the named list given during construction. Parameters from
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)
  with empty (i.e. `""`) `set_id` are referenced directly. Multiple
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)s
  with `set_id` `""` can be combined, but their parameter names may not
  overlap to avoid name clashes.

- When you either ask for 'values' or set them, the operation is
  delegated to the individual, contained
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)
  references. The collection itself does not maintain a `values` state.
  This also implies that if you directly change `values` in one of the
  referenced sets, this change is reflected in the collection.

- Dependencies: It is possible to currently handle dependencies

  - regarding parameters inside of the same set - in this case simply
    add the dependency to the set, best before adding the set to the
    collection

  - across sets, where a param from one set depends on the state of a
    param from another set - in this case add call `add_dep` on the
    collection.

  If you call `deps` on the collection, you are returned a complete
  table of dependencies, from sets and across sets.

## Super class

[`paradox::ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)
-\> `ParamSetCollection`

## Active bindings

- `deps`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Table has cols `id` (`character(1)`) and `on` (`character(1)`) and
  `cond`
  ([Condition](https://paradox.mlr-org.com/dev/reference/Condition.md)).
  Lists all (direct) dependency parents of a param, through parameter
  IDs. Internally created by a call to `add_dep`. Settable, if you want
  to remove dependencies or perform other changes.

- `extra_trafo`:

  (`function(x, param_set)`)  
  Transformation function. Settable. User has to pass a `function(x)`,
  of the form  
  (named [`list()`](https://rdrr.io/r/base/list.html),
  [ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md)) -\>
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

- `sets`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Read-only `list` of of
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)s
  contained in this `ParamSetCollection`. This field provides direct
  references to the
  [`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md)
  objects.

## Methods

### Public methods

- [`ParamSetCollection$new()`](#method-ParamSetCollection-new)

- [`ParamSetCollection$add()`](#method-ParamSetCollection-add)

- [`ParamSetCollection$subset()`](#method-ParamSetCollection-subset)

- [`ParamSetCollection$disable_internal_tuning()`](#method-ParamSetCollection-disable_internal_tuning)

- [`ParamSetCollection$convert_internal_search_space()`](#method-ParamSetCollection-convert_internal_search_space)

- [`ParamSetCollection$flatten()`](#method-ParamSetCollection-flatten)

- [`ParamSetCollection$clone()`](#method-ParamSetCollection-clone)

Inherited methods

- [`paradox::ParamSet$add_dep()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-add_dep)
- [`paradox::ParamSet$aggr_internal_tuned_values()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-aggr_internal_tuned_values)
- [`paradox::ParamSet$assert()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-assert)
- [`paradox::ParamSet$assert_dt()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-assert_dt)
- [`paradox::ParamSet$check()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-check)
- [`paradox::ParamSet$check_dependencies()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-check_dependencies)
- [`paradox::ParamSet$check_dt()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-check_dt)
- [`paradox::ParamSet$format()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-format)
- [`paradox::ParamSet$get_domain()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-get_domain)
- [`paradox::ParamSet$get_values()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-get_values)
- [`paradox::ParamSet$ids()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-ids)
- [`paradox::ParamSet$print()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-print)
- [`paradox::ParamSet$qunif()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-qunif)
- [`paradox::ParamSet$search_space()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-search_space)
- [`paradox::ParamSet$set_values()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-set_values)
- [`paradox::ParamSet$subspaces()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-subspaces)
- [`paradox::ParamSet$test()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-test)
- [`paradox::ParamSet$test_constraint()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-test_constraint)
- [`paradox::ParamSet$test_constraint_dt()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-test_constraint_dt)
- [`paradox::ParamSet$test_dt()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-test_dt)
- [`paradox::ParamSet$trafo()`](https://paradox.mlr-org.com/dev/reference/ParamSet.html#method-trafo)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ParamSetCollection$new(
      sets,
      tag_sets = FALSE,
      tag_params = FALSE,
      postfix_names = FALSE
    )

#### Arguments

- `sets`:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md))  
  ParamSet objects are not cloned. Names are used as "set_id" for the
  naming scheme of delegated parameters.

- `tag_sets`:

  (`logical(1)`)  
  Whether to add tags of the form `"set_<set_id>"` to each parameter
  originating from a given `ParamSet` given with name `<set_id>`.

- `tag_params`:

  (`logical(1)`)  
  Whether to add tags of the form `"param_<param_id>"` to each parameter
  with original ID `<param_id>`.

- `postfix_names`:

  (`logical(1)`)  
  Whether to use the names inside `sets` as postfixes, rather than
  prefixes.

------------------------------------------------------------------------

### Method `add()`

Adds a
[`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md) to
this collection.

#### Usage

    ParamSetCollection$add(p, n = "", tag_sets = FALSE, tag_params = FALSE)

#### Arguments

- `p`:

  ([ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md)).

- `n`:

  (`character(1)`)  
  Name to use. Default `""`.

- `tag_sets`:

  (`logical(1)`)  
  Whether to add tags of the form `"set_<n>"` to the newly added
  parameters.

- `tag_params`:

  (`logical(1)`)  
  Whether to add tags of the form `"param_<param_id>"` to each parameter
  with original ID `<param_id>`.

------------------------------------------------------------------------

### Method [`subset()`](https://rdrr.io/r/base/subset.html)

Create a new `ParamSet` restricted to the passed IDs.

#### Usage

    ParamSetCollection$subset(
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

### Method `disable_internal_tuning()`

Set the parameter values so that internal tuning for the selected
parameters is disabled.

#### Usage

    ParamSetCollection$disable_internal_tuning(ids)

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

    ParamSetCollection$convert_internal_search_space(search_space)

#### Arguments

- `search_space`:

  ([`ParamSet`](https://paradox.mlr-org.com/dev/reference/ParamSet.md))  
  The internal search space.

#### Returns

(named [`list()`](https://rdrr.io/r/base/list.html))

------------------------------------------------------------------------

### Method `flatten()`

Create a `ParamSet` from this `ParamSetCollection`.

#### Usage

    ParamSetCollection$flatten()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ParamSetCollection$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

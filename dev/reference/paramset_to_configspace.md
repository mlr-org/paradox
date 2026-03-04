# Convert a paradox ParamSet to a ConfigSpace ConfigurationSpace

Translates a
[ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md) into a
Python `ConfigSpace.ConfigurationSpace` via
[reticulate](https://CRAN.R-project.org/package=reticulate). This
function performs strict validation to ensure the
[ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md) can be
represented in ConfigSpace:

- Defaults are optional. If a parameter has no default, ConfigSpace will
  auto-assign one (e.g. midpoint for numeric parameters, first level for
  categoricals).

Supported parameter mappings:

- [`p_dbl()`](https://paradox.mlr-org.com/dev/reference/Domain.md):
  `Float` / `UniformFloatHyperparameter`

- [`p_int()`](https://paradox.mlr-org.com/dev/reference/Domain.md):
  `Integer` / `UniformIntegerHyperparameter`

- [`p_lgl()`](https://paradox.mlr-org.com/dev/reference/Domain.md):
  `Categorical` (TRUE/FALSE)

- [`p_fct()`](https://paradox.mlr-org.com/dev/reference/Domain.md):
  `Categorical`

Dependency conditions (`CondEqual`, `CondAnyOf`) are preserved. Multiple
conditions on the same child are combined using
`ConfigSpace.AndConjunction`.

The function auto-detects old ConfigSpace API (ConfigSpace \< 0.6.0) vs.
new ConfigSpace API (ConfigSpace \>= 0.6.0).

## Usage

``` r
paramset_to_configspace(param_set, name = NULL)
```

## Arguments

- param_set:

  [ParamSet](https://paradox.mlr-org.com/dev/reference/ParamSet.md)  
  The parameter set to convert. Numeric parameters must define both
  `lower` and `upper` bounds.

- name:

  `character(1)`  
  Optional name for the resulting ConfigurationSpace.

## Value

A Python `ConfigSpace.ConfigurationSpace` object representing the given
parameter set.

## Examples

``` r
if (FALSE) { # \dontrun{
  param_set = ps(
    lr        = p_dbl(lower = 1e-5, upper = 1,   default = 0.01, tags = "train"),
    ntree     = p_int(lower = 10,   upper = 500, default = 100,  tags = c("train","tuning")),
    bootstrap = p_lgl(default = TRUE, tags = "train"),
    criterion = p_fct(levels = c("gini", "entropy", "other"), default = "gini", tags = "train"),
    extras    = p_fct(tags = "predict", default = "alpha",
                      levels = c("alpha","beta","gamma","delta","kappa","nu")),
    depending = p_lgl(tags = "train", default = TRUE,
                      depends = quote(criterion == "entropy" && extras %in% c("alpha","beta")))
  )
  cs = paramset_to_configspace(param_set, name = "demo")
} # }
```

# paradox 0.7.0

* `ParamSet$get_values()` checks whether all required parameter values are set.
  Required parameter are not checked anymore when new values are added to the
  parameter set.
* `ParamSet$check_dt()` accepts `data.frame`s.
* Rename `is_numeric` and `is_categorical` to `all_numeric` and
  `all_categorical`.
* Rename `requires` to `depends`.

# paradox 0.6.0

* `ps()` shortcuts for `ParamSet` construction, with new `Domain` construct and
  constructors `p_dbl`, `p_int`, `p_lgl`, `p_fct`, and `p_uty`.
* `ParamSet$search_space()` method that constructs tunable `ParamSet` from
  `TuneToken` objects, which are constructed with `to_tune()`.

# paradox 0.5.0

* Compact in-memory representation of R6 objects to save space when
  saving objects via saveRDS(), serialize() etc.
* Improved performance for `ParamSetCollection`.

# paradox 0.4.0

* New public methods `is_numeric()` and `is_categorical()` for parameter sets.
* Fixed a test for upcoming release of `data.table()`.
* Added a helper function to format parameter sets in Rd files.

# paradox 0.3.0

* New function `transpose()` converts `data.table` of parameter values to a list
  of lists.
* New methods `ParamSet$check_dt()`, `$assert_dt()` and `test_dt()` can check a
  `data.table` for valid parameter values.
* Documentation updated.
* Unified style for object printers.

# paradox 0.2.0

* Fixed warnings about partial argument matching.
* Enforce integer bounds in ParamInt (#258).
* Reexport `data.table::as.data.table()`.
* Deep cloning of `ParamSet$values` (#273).

# paradox 0.1.0

* Initial release.

# paradox 0.2.0.9000

* New function `transpose()` converts `data.table` of parameter values to a list of lists.
* New methods `ParamSet$check_dt()`, `$assert_dt()` and `test_dt()` can check a `data.table` for valid parameter values.
* Documentation updated.
* Unified style for object printers.

# paradox 0.2.0

* Fixed warnings about partial argument matching.
* Enforce integer bounds in ParamInt (#258).
* Reexport `data.table::as.data.table()`.
* Deep cloning of `ParamSet$values` (#273).

# paradox 0.1.0

* Initial release.

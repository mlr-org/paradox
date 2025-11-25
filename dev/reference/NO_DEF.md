# Extra data type for "no default value"

Special new data type for no-default. Not often needed by the end-user,
mainly internal.

- `NO_DEF`: Singleton object for type, used in
  [`Domain`](https://paradox.mlr-org.com/dev/reference/Domain.md) when
  no default is given.

- `is_nodefault()`: Is an object the 'no default' object?

native_get_domain_call = function(param_set, id) {
  .Call(
    paradox:::C_param_set_get_domain,
    param_set$.__enclos_env__$private,
    param_set,
    id
  )
}

test_that("get_domain native entry is registered and forced", {
  symbol = paradox:::C_param_set_get_domain
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 3L)
  expect_error(
    .Call("param_set_get_domain", PACKAGE = "paradox"),
    "not available"
  )
})

test_that("get_domain is the scalar view of the shared Domains engine", {
  object = ps(
    number = p_dbl(-2, 2, tags = "numeric", trafo = exp),
    integer = p_int(-3L, 3L, init = 2L),
    factor = p_fct(c("small", "large")),
    payload = p_uty(init = list(marker = TRUE))
  )
  object$add_dep("factor", "integer", CondEqual(2L))

  domains = object$domains
  for (id in object$ids()) {
    expect_identical(native_get_domain_call(object, id), domains[[id]])
    expect_identical(object$get_domain(id), domains[[id]])
  }
})

test_that("collection get_domain sees live child state", {
  child = ps(x = p_int(0L, 5L), y = p_lgl())
  collection = psc(component = child)
  child$values = list(x = 2L)
  child$add_dep("y", "x", CondEqual(2L))

  domain = collection$get_domain("component.y")
  expect_false(domain$.init_given)
  expect_identical(domain$.requirements[[1L]][[1L]]$on, "component.x")
  expect_identical(domain$.requirements[[1L]][[1L]]$cond$rhs, 2L)
  expect_identical(
    collection$get_domain("component.x")$.init[[1L]],
    2L
  )
})

test_that("get_domain returns a detached valid data.table", {
  object = ps(x = p_dbl(0, 1, tags = c("a", "b")))
  domain = object$get_domain("x")
  expect_identical(data.table:::selfrefok(domain, FALSE), 1L)
  data.table::set(domain, i = 1L, j = "lower", value = -1)
  data.table::set(domain, j = "new", value = TRUE)
  expect_identical(object$get_domain("x")$lower, 0)
  expect_false("new" %in% names(object$get_domain("x")))
})

test_that("unknown get_domain IDs preserve text and reject bytes", {
  unknown_utf8 = enc2utf8("m\u00fcssing")
  unknown_latin1 = iconv(unknown_utf8, from = "UTF-8", to = "latin1")
  skip_if(
    is.na(unknown_latin1),
    "this platform cannot represent the latin1 fixture"
  )
  Encoding(unknown_latin1) = "latin1"
  unknown_bytes = unknown_utf8
  Encoding(unknown_bytes) = "bytes"

  object = ps(x = p_int())
  error = tryCatch(
    native_get_domain_call(object, unknown_latin1),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_identical(
    enc2utf8(conditionMessage(error)),
    "No param with id 'm\u00fcssing'"
  )
  expect_error(
    native_get_domain_call(object, unknown_bytes),
    "Unknown bytes-encoded parameter ID",
    fixed = TRUE
  )
})

test_that("get_domain validates requests without revalidating private labels", {
  object = ps(x = p_int())
  expect_error(native_get_domain_call(object, NA_character_), "non-missing")
  expect_error(native_get_domain_call(object, character()), "one non-missing")
  expect_error(native_get_domain_call(object, "missing"), "No param")

  private = object$.__enclos_env__$private
  params = paradox:::param_set_core_state(private)$.params
  params$storage_type[[1L]] = "extension"
  paradox:::param_set_core_replace(private, params = params)
  expect_identical(object$get_domain("x")$storage_type, "extension")
})

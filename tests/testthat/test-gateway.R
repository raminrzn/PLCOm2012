# The ModelsCloud platform calls <pkg>::gateway(func = "model_run", ...).
expit_risk <- model_run(get_default_input())$risk

test_that("gateway dispatches func='model_run' and returns JSON", {
  js <- gateway(func = "model_run", model_input = get_default_input())
  expect_type(js, "character")
  parsed <- jsonlite::fromJSON(js)
  expect_true("risk" %in% names(parsed))
  expect_equal(parsed$risk, expit_risk, tolerance = 1e-9)
})

test_that("gateway defaults func to model_run and strips control fields", {
  js <- gateway(model_input = get_default_input(), api_key = "x", session_id = "y")
  expect_equal(jsonlite::fromJSON(js)$risk, expit_risk, tolerance = 1e-9)
})

test_that("gateway handles no-arg dispatch (get_default_input)", {
  parsed <- jsonlite::fromJSON(gateway(func = "get_default_input"))
  expect_true("age" %in% names(parsed))
})

test_that("gateway accepts the platform's unwrapped + aliased payload", {
  js <- gateway(func = "model_run", age = 62, race = 1, edu6 = 3, bmi = 27,
                copd = 0, phist = 0, famhx = 1, smkstat = 1, cpd = 20,
                smkyears = 40, qtyears = 0, female = 0)
  expect_equal(round(jsonlite::fromJSON(js)$risk, 4), 0.0447)
})

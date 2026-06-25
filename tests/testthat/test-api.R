test_that("model_run returns risk for the sample cohort", {
  out <- model_run(get_sample_input())
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 4L)
  expect_true(all(c("risk", "risk_percent") %in% names(out)))
  expect_true(all(out$risk > 0 & out$risk < 1))
  expect_equal(out$risk_percent, round(100 * out$risk, 2))
})

test_that("model_run accepts a single-patient list and the default", {
  out <- model_run(get_default_input())
  expect_equal(nrow(out), 1L)
  expect_true(out$risk > 0 && out$risk < 1)

  expect_equal(model_run(NULL)$risk, out$risk)  # NULL -> default
})

test_that("model_run ignores unknown fields but requires the core predictors", {
  ok <- get_default_input()
  ok$female <- 0                         # extra field -> ignored, not fatal
  expect_no_error(model_run(ok))

  short <- get_default_input()
  short$bmi <- NULL                      # drop a required variable
  expect_error(model_run(short), "Missing required variable")
})

test_that("model_run accepts unwrapped named args (do.call style)", {
  d <- get_default_input()
  wrapped   <- model_run(d)              # model_input = list(...)
  unwrapped <- do.call(model_run, d)     # fields as direct named args
  expect_equal(wrapped$risk, unwrapped$risk)
})

test_that("model_run accepts lcmodels-style aliases and a raw payload", {
  out <- model_run(list(
    age = 62, race = 1, edu6 = 3, bmi = 27, copd = 0, phist = 0, famhx = 1,
    smkstat = 1, cpd = 20, smkyears = 40, qtyears = 0, female = 0
  ))
  expected <- plcom2012(
    age = 62, race = 1, education = 3, bmi = 27, copd = 0, cancer_hist = 0,
    family_hist_lung_cancer = 1, smoking_status = 1, smoking_intensity = 20,
    duration_smoking = 40, smoking_quit_time = 0
  )
  expect_equal(out$risk, expected)
})

test_that("race code 0 is treated as the White reference", {
  base_args <- list(age = 62, education = 4, bmi = 27, copd = 0,
                    cancer_hist = 0, family_hist_lung_cancer = 0,
                    smoking_status = 0, smoking_intensity = 20,
                    duration_smoking = 27, smoking_quit_time = 10)
  r0 <- model_run(c(list(race = 0), base_args))$risk
  r1 <- model_run(c(list(race = 1), base_args))$risk
  expect_equal(r0, r1)
})

test_that("get_sample_input(n) limits rows and validates n", {
  expect_equal(nrow(get_sample_input(2)), 2L)
  expect_error(get_sample_input(0), "positive integer")
})

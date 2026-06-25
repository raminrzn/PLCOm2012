# Reference values: with every continuous predictor at its centring value and
# the smoking-intensity term zeroed (intensity = 10 / 0.4021541613), the linear
# predictor reduces to the intercept plus the race adjustment.
int <- -4.532506
zero_intensity <- 10 / 0.4021541613

base <- function(race) {
  plcom2012(
    age = 62, race = race, education = 4, bmi = 27, copd = 0,
    cancer_hist = 0, family_hist_lung_cancer = 0, smoking_status = 0,
    smoking_intensity = zero_intensity, duration_smoking = 27,
    smoking_quit_time = 10
  )
}
expit <- function(x) exp(x) / (1 + exp(x))

test_that("race adjustments match the published log-odds", {
  expect_equal(base("White"),    expit(int))
  expect_equal(base("Black"),    expit(int + 0.3944778))
  expect_equal(base("Hispanic"), expit(int - 0.7434744))
  expect_equal(base("Asian"),    expit(int - 0.466585))
  expect_equal(base("American Indian or Alaskan Native"), expit(int + 1.027152))
  expect_equal(base("Native Hawaiian or Pacific Islander"), expit(int))
})

test_that("race aliases and numeric codes agree with canonical labels", {
  expect_equal(base("caucasian"),        base("White"))
  expect_equal(base("African American"), base("Black"))
  expect_equal(base("Pacific Islander"), base("Native Hawaiian or Pacific Islander"))
  expect_equal(base(1), base("White"))
  expect_equal(base(2), base("Black"))
  expect_equal(base(5), base("American Indian or Alaskan Native"))
  expect_equal(base(6), base("Native Hawaiian or Pacific Islander"))
})

test_that("a worked example reproduces the documented value", {
  # resplab/PLCOm2012 documented example output.
  r <- plcom2012(
    age = 62, race = "White", education = 4, bmi = 27, copd = 0,
    cancer_hist = 0, family_hist_lung_cancer = 0, smoking_status = 0,
    smoking_intensity = 80, duration_smoking = 27, smoking_quit_time = 10
  )
  expect_equal(round(r, 8), 0.01750922)
})

test_that("the function is vectorised and recycles scalars", {
  r <- plcom2012(
    age = c(62, 70), race = c("White", "Black"), education = 4, bmi = 27,
    copd = c(0, 1), cancer_hist = 0, family_hist_lung_cancer = c(0, 1),
    smoking_status = c(0, 1), smoking_intensity = c(20, 40),
    duration_smoking = c(27, 45), smoking_quit_time = c(10, 0)
  )
  expect_length(r, 2)
  expect_true(all(r > 0 & r < 1))
  expect_gt(r[2], r[1])
})

test_that("invalid inputs are rejected", {
  expect_error(base(0), "race")                 # never-smoker handled separately
  expect_error(base("Klingon"), "Unrecognised")
  expect_error(
    plcom2012(age = 62, race = "White", education = 4, bmi = 27, copd = 0,
              cancer_hist = 0, family_hist_lung_cancer = 0, smoking_status = 0,
              smoking_intensity = 0, duration_smoking = 27, smoking_quit_time = 10),
    "ever-smokers"
  )
})

# ---------------------------------------------------------------------------
# ModelsCloud API surface for PLCOm2012.
#
# These three functions are the contract the ModelsCloud (pexa) executor calls:
#   - model_run(model_input)  : synchronous; risk for each input row
#   - get_sample_input(n)     : an example cohort
#   - get_default_input()     : one baseline patient to modify
# They are the package's only exported entry points besides plcom2012().
# ---------------------------------------------------------------------------

# Predictor columns accepted by the API (single source of truth for validation).
.plco_vars <- c(
  "age", "race", "education", "bmi", "copd", "cancer_hist",
  "family_hist_lung_cancer", "smoking_status", "smoking_intensity",
  "duration_smoking", "smoking_quit_time"
)

# Accepted aliases (alias -> canonical), so common lcmodels-style payloads work.
.plco_alias <- c(
  edu6 = "education", edu = "education", education_level = "education",
  cpd = "smoking_intensity", cigs_per_day = "smoking_intensity",
  smkyears = "duration_smoking", duration = "duration_smoking",
  qtyears = "smoking_quit_time", quit_time = "smoking_quit_time",
  years_quit = "smoking_quit_time",
  phist = "cancer_hist", prior_cancer = "cancer_hist",
  cancer_history = "cancer_hist",
  famhx = "family_hist_lung_cancer", fam_hist = "family_hist_lung_cancer",
  family_history = "family_hist_lung_cancer",
  smkstat = "smoking_status", smkstatus = "smoking_status",
  smoke_status = "smoking_status"
)

# Normalise an incoming payload: accept either a wrapped `model_input` object or
# fields passed directly as named arguments (`dots`); rename known aliases to
# canonical names; drop any unrecognised extra fields (e.g. `female`). Returns
# NULL when no input at all was supplied.
.plco_normalize <- function(model_input, dots) {
  if (is.null(model_input)) {
    if (length(dots) == 0) return(NULL)
    model_input <- dots
  }
  df <- as.data.frame(model_input, stringsAsFactors = FALSE)
  for (a in intersect(names(df), names(.plco_alias))) {
    canon <- .plco_alias[[a]]
    if (!canon %in% names(df)) names(df)[match(a, names(df))] <- canon
  }
  df[, intersect(names(df), .plco_vars), drop = FALSE]
}

#' Run the PLCOm2012 model (ModelsCloud entry point)
#'
#' Scores one or more patients and returns the input augmented with the
#' predicted 6-year lung cancer risk. This is the synchronous prediction
#' pattern expected by ModelsCloud: a table of patients in, the same table
#' plus predictions out.
#'
#' @details
#' The function is deliberately forgiving about how inputs arrive, so it works
#' whether the platform wraps the fields under `model_input` or passes them
#' directly:
#'
#' * **Wrapped** — `model_run(model_input = list(age = 62, ...))` or a data
#'   frame (the form produced by the `modelscloud` client).
#' * **Unwrapped** — `model_run(age = 62, race = 1, ...)` (the form produced by
#'   a raw `do.call(model_run, funcInput)` when `funcInput` holds the fields
#'   directly).
#'
#' Common aliases are accepted and mapped to the canonical names: `edu6`/`edu`
#' -> `education`, `cpd` -> `smoking_intensity`, `smkyears` ->
#' `duration_smoking`, `qtyears` -> `smoking_quit_time`, `phist` ->
#' `cancer_hist`, `famhx` -> `family_hist_lung_cancer`, `smkstat` ->
#' `smoking_status`. Unrecognised extra fields (e.g. `female`, which PLCOm2012
#' does not use) are ignored.
#'
#' @param model_input A named list (one patient) or data frame (one row per
#'   patient) whose columns are the PLCOm2012 predictors. See [plcom2012()]
#'   for the meaning and coding of each field, or call [get_sample_input()] /
#'   [get_default_input()] for ready-made examples. If `NULL` and no fields are
#'   supplied via `...`, the model's [get_default_input()] is used.
#' @param ... Alternative to `model_input`: the predictor fields supplied
#'   directly as named arguments (e.g. from an unwrapped API call).
#'
#' @return A data frame: the input columns plus `risk` (6-year probability in
#'   `[0, 1]`) and `risk_percent` (the same value as a percentage, rounded to
#'   two decimals).
#'
#' @seealso [plcom2012()], [get_sample_input()], [get_default_input()]
#' @examples
#' model_run(get_sample_input())
#' model_run(get_default_input())
#' # Unwrapped + aliases (e.g. a raw API call); `female` is ignored:
#' model_run(age = 62, race = 1, edu6 = 3, bmi = 27, copd = 0, phist = 0,
#'           famhx = 1, smkstat = 1, cpd = 20, smkyears = 40, qtyears = 0,
#'           female = 0)
#' @export
model_run <- function(model_input = NULL, ...) {
  df <- .plco_normalize(model_input, list(...))
  if (is.null(df)) df <- as.data.frame(get_default_input(), stringsAsFactors = FALSE)

  missing <- setdiff(.plco_vars, names(df))
  if (length(missing) > 0) {
    stop("Missing required variable(s): ", paste(missing, collapse = ", "),
         ". Accepted names (incl. aliases) are documented in ?model_run.",
         call. = FALSE)
  }

  df$risk <- plcom2012(
    age                     = df$age,
    race                    = df$race,
    education               = df$education,
    bmi                     = df$bmi,
    copd                    = df$copd,
    cancer_hist             = df$cancer_hist,
    family_hist_lung_cancer = df$family_hist_lung_cancer,
    smoking_status          = df$smoking_status,
    smoking_intensity       = df$smoking_intensity,
    duration_smoking        = df$duration_smoking,
    smoking_quit_time       = df$smoking_quit_time
  )
  df$risk_percent <- round(100 * df$risk, 2)
  df
}

#' Example PLCOm2012 input cohort
#'
#' Returns a small data frame of example ever-smokers that can be passed
#' straight to [model_run()], i.e. `model_run(get_sample_input())` works.
#'
#' @param n Optional positive integer; if supplied, the first `n` rows are
#'   returned. Defaults to all rows.
#' @return A data frame of example patients with the PLCOm2012 predictor columns.
#' @seealso [model_run()], [get_default_input()]
#' @examples
#' get_sample_input()
#' get_sample_input(n = 2)
#' @export
get_sample_input <- function(n = NULL) {
  df <- data.frame(
    age                     = c(62, 70, 58, 67),
    race                    = c("White", "Black", "Asian", "Hispanic"),
    education               = c(4, 2, 5, 3),
    bmi                     = c(27, 24, 22, 30),
    copd                    = c(0, 1, 0, 0),
    cancer_hist             = c(0, 0, 0, 1),
    family_hist_lung_cancer = c(0, 1, 0, 0),
    smoking_status          = c(0, 1, 0, 1),
    smoking_intensity       = c(20, 40, 15, 30),
    duration_smoking        = c(27, 45, 30, 38),
    smoking_quit_time       = c(10, 0, 5, 0),
    stringsAsFactors        = FALSE
  )
  if (!is.null(n)) {
    if (!is.numeric(n) || length(n) != 1L || n < 1L) {
      stop("`n` must be a single positive integer.", call. = FALSE)
    }
    df <- utils::head(df, n)
  }
  df
}

#' Default PLCOm2012 input
#'
#' Returns a single baseline ever-smoker as a named list, ready to modify and
#' pass to [model_run()]. The defaults correspond to the model's centring
#' values (age 62, education 4, BMI 27), a former smoker of 20 cigarettes/day
#' for 27 years who quit 10 years ago, with no comorbidities.
#'
#' @return A named list of default predictor values.
#' @seealso [model_run()], [get_sample_input()]
#' @examples
#' patient <- get_default_input()
#' patient$age <- 68
#' patient$copd <- 1
#' model_run(patient)
#' @export
get_default_input <- function() {
  list(
    age                     = 62,
    race                    = "White",
    education               = 4,
    bmi                     = 27,
    copd                    = 0,
    cancer_hist             = 0,
    family_hist_lung_cancer = 0,
    smoking_status          = 0,
    smoking_intensity       = 20,
    duration_smoking        = 27,
    smoking_quit_time       = 10
  )
}

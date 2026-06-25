# ---------------------------------------------------------------------------
# PLCOm2012 — core model
#
# Tammemägi MC, Katki HA, Hocking WG, et al. Selection criteria for
# lung-cancer screening. N Engl J Med. 2013;368(8):728-736.
# doi:10.1056/NEJMoa1211776  (correction: N Engl J Med 2013;369(4):394)
#
# The model is a logistic regression predicting the 6-year probability of a
# *first* lung cancer in an ever-smoker. All continuous predictors enter
# centred at the cohort means used in the original fit (e.g. age at 62).
# ---------------------------------------------------------------------------

# Fixed model coefficients (log-odds scale), exactly as published.
# Cross-verified against three independent implementations
# (resplab/PLCOm2012, marskar/lcmodels, WeiLab4Research/AIConsult-LC); where
# they disagreed on the two small race groups, the published odds ratios were
# used as the tie-breaker (American Indian/Alaskan Native OR = 2.79 -> +1.027152).
.plco_coef <- c(
  intercept    = -4.532506,
  age          =  0.0778868,   # per year, centred at 62
  education    = -0.0812744,   # per ordinal level, centred at 4
  bmi          = -0.0274194,   # per kg/m^2, centred at 27
  copd         =  0.3553063,   # COPD/emphysema (yes = 1)
  cancer_hist  =  0.4589971,   # personal history of cancer (yes = 1)
  family_hist  =  0.587185,    # family history of lung cancer (yes = 1)
  smoking_curr =  0.2597431,   # current (vs former) smoker
  smoke_int    = -1.822606,    # x ((cpd/10)^-1 - 0.4021541613)
  duration     =  0.0317321,   # per year smoked, centred at 27
  quit_time    = -0.0308572    # per year since quitting, centred at 10
)

# Centring constant for the (cigarettes-per-day / 10)^-1 smoking-intensity term.
.plco_smoke_int_centre <- 0.4021541613

# Race / ethnicity adjustments (log-odds), relative to the White reference.
# Native Hawaiian or Pacific Islander was estimated at the reference (0).
.plco_race_beta_map <- c(
  white                              = 0,
  black                              = 0.3944778,
  hispanic                           = -0.7434744,
  asian                              = -0.466585,
  `american indian or alaskan native` = 1.027152,
  `native hawaiian or pacific islander` = 0
)

# Numeric race codes -> canonical category name.
.plco_race_codes <- c(
  "white",
  "black",
  "hispanic",
  "asian",
  "american indian or alaskan native",
  "native hawaiian or pacific islander"
)

# String aliases -> canonical category name.
.plco_race_aliases <- c(
  "white"                               = "white",
  "caucasian"                           = "white",
  "non-hispanic white"                  = "white",
  "black"                               = "black",
  "african american"                    = "black",
  "african-american"                    = "black",
  "hispanic"                            = "hispanic",
  "latino"                              = "hispanic",
  "latina"                              = "hispanic",
  "asian"                               = "asian",
  "american indian or alaskan native"  = "american indian or alaskan native",
  "american indian"                     = "american indian or alaskan native",
  "alaskan native"                      = "american indian or alaskan native",
  "alaska native"                       = "american indian or alaskan native",
  "indigenous"                          = "american indian or alaskan native",
  "native hawaiian or pacific islander" = "native hawaiian or pacific islander",
  "native hawaiian"                     = "native hawaiian or pacific islander",
  "pacific islander"                    = "native hawaiian or pacific islander",
  "hawaiian"                            = "native hawaiian or pacific islander"
)

# Map a race vector (character aliases or numeric codes 1-6) to log-odds betas.
.plco_race_beta <- function(race) {
  if (is.numeric(race)) {
    if (any(!race %in% seq_along(.plco_race_codes), na.rm = TRUE)) {
      stop("Numeric `race` codes must be 1-6: ",
           "1 White, 2 Black, 3 Hispanic, 4 Asian, ",
           "5 American Indian/Alaskan Native, 6 Native Hawaiian/Pacific Islander.",
           call. = FALSE)
    }
    canon <- .plco_race_codes[race]
  } else {
    key <- tolower(trimws(as.character(race)))
    canon <- unname(.plco_race_aliases[key])
    if (any(is.na(canon))) {
      bad <- unique(race[is.na(canon)])
      stop("Unrecognised `race` value(s): ", paste(bad, collapse = ", "),
           ". See ?plcom2012 for accepted categories.", call. = FALSE)
    }
  }
  unname(.plco_race_beta_map[canon])
}

#' PLCOm2012 6-year lung cancer risk
#'
#' Computes the 6-year probability of a first lung cancer in an ever-smoker
#' using the PLCOm2012 logistic risk model (Tammemägi et al., *NEJM* 2013).
#' All arguments are vectorised and recycled to a common length, so the
#' function scores a single person or a whole cohort in one call.
#'
#' @details
#' The linear predictor is
#'
#' \deqn{
#'   \eta = -4.532506
#'        + 0.0778868\,(age-62)
#'        - 0.0812744\,(education-4)
#'        - 0.0274194\,(bmi-27)
#'        + 0.3553063\,copd
#'        + 0.4589971\,cancer\_hist
#'        + 0.587185\,family\_hist\_lung\_cancer
#'        + 0.2597431\,smoking\_status
#'        - 1.822606\,((intensity/10)^{-1} - 0.4021541613)
#'        + 0.0317321\,(duration-27)
#'        - 0.0308572\,(quit\_time-10)
#'        + \beta_{race}
#' }
#'
#' and the returned risk is \eqn{exp(\eta) / (1 + exp(\eta))}.
#'
#' The model is defined for **ever-smokers only**. Smoking intensity must be a
#' positive number of cigarettes per day; `smoking_intensity = 0` (never-smoker)
#' is undefined and raises an error.
#'
#' Race/ethnicity log-odds adjustments (relative to the White reference):
#' Black `+0.3944778`, Hispanic `-0.7434744`, Asian `-0.466585`,
#' American Indian or Alaskan Native `+1.027152`, and Native Hawaiian or
#' Pacific Islander `0` (estimated at the reference).
#'
#' @param age Age in years (model developed for 55-80; not clamped).
#' @param race Race/ethnicity, as a character label or a numeric code.
#'   Accepted labels (case-insensitive): `"White"`, `"Black"`, `"Hispanic"`,
#'   `"Asian"`, `"American Indian or Alaskan Native"`,
#'   `"Native Hawaiian or Pacific Islander"` (plus common aliases such as
#'   `"Caucasian"`, `"African American"`, `"Latino"`, `"Pacific Islander"`).
#'   Numeric codes: `1` White, `2` Black, `3` Hispanic, `4` Asian,
#'   `5` American Indian/Alaskan Native, `6` Native Hawaiian/Pacific Islander.
#' @param education Highest education on a 6-level ordinal scale:
#'   `1` < high-school, `2` high-school graduate, `3` some training after
#'   high school, `4` some college, `5` college graduate,
#'   `6` postgraduate/professional degree.
#' @param bmi Body-mass index in kg/m^2.
#' @param copd Chronic obstructive pulmonary disease / emphysema
#'   (`1` = yes, `0` = no).
#' @param cancer_hist Personal history of any cancer (`1` = yes, `0` = no).
#' @param family_hist_lung_cancer Family history of lung cancer
#'   (`1` = yes, `0` = no).
#' @param smoking_status Current smoker `1`, former smoker `0`.
#' @param smoking_intensity Average cigarettes smoked per day (must be > 0).
#' @param duration_smoking Total years smoked.
#' @param smoking_quit_time Years since quitting (`0` for current smokers).
#'
#' @return A numeric vector of 6-year lung cancer probabilities in `[0, 1]`,
#'   one element per (recycled) input row.
#'
#' @references
#' Tammemägi MC, Katki HA, Hocking WG, et al. Selection criteria for
#' lung-cancer screening. *N Engl J Med.* 2013;368(8):728-736.
#' \doi{10.1056/NEJMoa1211776}
#'
#' @examples
#' # A single 62-year-old former smoker
#' plcom2012(
#'   age = 62, race = "White", education = 4, bmi = 27,
#'   copd = 0, cancer_hist = 0, family_hist_lung_cancer = 0,
#'   smoking_status = 0, smoking_intensity = 20,
#'   duration_smoking = 27, smoking_quit_time = 10
#' )
#'
#' # Vectorised over two people
#' plcom2012(
#'   age = c(62, 70), race = c("White", "Black"), education = c(4, 2),
#'   bmi = c(27, 24), copd = c(0, 1), cancer_hist = c(0, 0),
#'   family_hist_lung_cancer = c(0, 1), smoking_status = c(0, 1),
#'   smoking_intensity = c(20, 40), duration_smoking = c(27, 45),
#'   smoking_quit_time = c(10, 0)
#' )
#' @export
plcom2012 <- function(age, race, education, bmi, copd, cancer_hist,
                      family_hist_lung_cancer, smoking_status,
                      smoking_intensity, duration_smoking, smoking_quit_time) {

  # Recycle all numeric predictors to a common length.
  num <- list(
    age = age, education = education, bmi = bmi, copd = copd,
    cancer_hist = cancer_hist,
    family_hist_lung_cancer = family_hist_lung_cancer,
    smoking_status = smoking_status, smoking_intensity = smoking_intensity,
    duration_smoking = duration_smoking, smoking_quit_time = smoking_quit_time
  )
  n <- max(lengths(num), length(race))
  rep_to_n <- function(x) {
    if (length(x) == 1L) return(rep(x, n))
    if (length(x) != n)
      stop("All inputs must have length 1 or a common length.", call. = FALSE)
    x
  }
  num <- lapply(num, function(x) as.numeric(rep_to_n(x)))
  race <- rep_to_n(race)

  if (any(num$smoking_intensity <= 0, na.rm = TRUE)) {
    stop("`smoking_intensity` must be > 0; PLCOm2012 is defined for ",
         "ever-smokers only.", call. = FALSE)
  }

  b <- .plco_coef
  eta <- b[["intercept"]] +
    b[["age"]]          * (num$age - 62) +
    b[["education"]]    * (num$education - 4) +
    b[["bmi"]]          * (num$bmi - 27) +
    b[["copd"]]         * num$copd +
    b[["cancer_hist"]]  * num$cancer_hist +
    b[["family_hist"]]  * num$family_hist_lung_cancer +
    b[["smoking_curr"]] * num$smoking_status +
    b[["smoke_int"]]    * ((num$smoking_intensity / 10)^(-1) - .plco_smoke_int_centre) +
    b[["duration"]]     * (num$duration_smoking - 27) +
    b[["quit_time"]]    * (num$smoking_quit_time - 10) +
    .plco_race_beta(race)

  unname(exp(eta) / (1 + exp(eta)))
}

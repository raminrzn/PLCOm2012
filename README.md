# PLCOm2012 <img src="https://img.shields.io/badge/lifecycle-stable-brightgreen.svg" align="right"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/raminrzn/PLCOm2012/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/raminrzn/PLCOm2012/actions/workflows/R-CMD-check.yaml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

A self-contained R implementation of **PLCOm2012**, the logistic risk model that
predicts the **6-year probability of a first lung cancer in ever-smokers**
(Tammemägi et al., *New England Journal of Medicine*, 2013).

The package has **no dependencies** beyond base R and exposes two layers:

1. **`plcom2012()`** — a vectorised model function for interactive / batch use in R.
2. The **ModelsCloud API** (`model_run()`, `get_sample_input()`,
   `get_default_input()`) so the same package can be hosted as a prediction
   model on the [ModelsCloud](https://modelscloud.resp.core.ubc.ca/) platform
   (RESP Lab, UBC).

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("raminrzn/PLCOm2012")
```

---

## Quick start

```r
library(PLCOm2012)

# A single 62-year-old former smoker
plcom2012(
  age = 62, race = "White", education = 4, bmi = 27,
  copd = 0, cancer_hist = 0, family_hist_lung_cancer = 0,
  smoking_status = 0, smoking_intensity = 20,
  duration_smoking = 27, smoking_quit_time = 10
)
#> [1] 0.0125...

# Score a whole cohort through the ModelsCloud API
model_run(get_sample_input())
#>   age     race education bmi copd ... risk risk_percent
#> 1  62    White         4  27    0 ... 0.01x        1.x
#> ...
```

---

## The model

PLCOm2012 is a logistic regression. The log-odds (linear predictor) is:

```
eta = -4.532506
      + 0.0778868 * (age - 62)
      - 0.0812744 * (education - 4)
      - 0.0274194 * (bmi - 27)
      + 0.3553063 * copd
      + 0.4589971 * cancer_hist
      + 0.587185  * family_hist_lung_cancer
      + 0.2597431 * smoking_status            # current vs former
      - 1.822606  * ((smoking_intensity/10)^-1 - 0.4021541613)
      + 0.0317321 * (duration_smoking - 27)
      - 0.0308572 * (smoking_quit_time - 10)
      + beta_race

risk = exp(eta) / (1 + exp(eta))
```

Continuous predictors are **centred** at the values used in the original fit
(age 62, education 4, BMI 27, duration 27, quit-time 10). The model is defined
for **ever-smokers only**; `smoking_intensity` must be greater than zero.

### Coefficients

| Predictor | Coefficient (log-odds) |
|---|---:|
| Intercept | −4.532506 |
| Age (per year, centred 62) | 0.0778868 |
| Education (per level, centred 4) | −0.0812744 |
| BMI (per kg/m², centred 27) | −0.0274194 |
| COPD / emphysema | 0.3553063 |
| Personal cancer history | 0.4589971 |
| Family history of lung cancer | 0.587185 |
| Current smoker (vs former) | 0.2597431 |
| Smoking intensity `((cpd/10)^-1 − 0.4021541613)` | −1.822606 |
| Duration smoked (per year, centred 27) | 0.0317321 |
| Years since quitting (per year, centred 10) | −0.0308572 |

### Race / ethnicity (relative to White reference)

| Category | Coefficient | Numeric code |
|---|---:|:--:|
| White | 0 (reference) | 1 |
| Black | 0.3944778 | 2 |
| Hispanic | −0.7434744 | 3 |
| Asian | −0.466585 | 4 |
| American Indian or Alaskan Native | 1.027152 | 5 |
| Native Hawaiian or Pacific Islander | 0 | 6 |

> **Note on the two small race groups.** Published implementations disagree on
> which of the two small groups carries the large `+1.027152` adjustment. This
> package follows the original NEJM odds ratios — *American Indian or Alaskan
> Native* OR = 2.79 → ln(2.79) ≈ 1.027 — placing `+1.027152` on that group and
> `0` on *Native Hawaiian or Pacific Islander*. This matches `marskar/lcmodels`
> and `WeiLab4Research/AIConsult-LC`; note that `resplab/PLCOm2012` assigns
> these two groups the opposite way.

---

## Input coding

| Variable | Meaning | Coding |
|---|---|---|
| `age` | Age in years | numeric (model developed for 55–80) |
| `race` | Race / ethnicity | label or numeric code (see table above) |
| `education` | Highest education | 1 < HS · 2 HS grad · 3 some post-HS training · 4 some college · 5 college grad · 6 postgraduate |
| `bmi` | Body-mass index | numeric, kg/m² |
| `copd` | COPD / emphysema | 1 yes · 0 no |
| `cancer_hist` | Personal history of any cancer | 1 yes · 0 no |
| `family_hist_lung_cancer` | Family history of lung cancer | 1 yes · 0 no |
| `smoking_status` | Smoking status | 1 current · 0 former |
| `smoking_intensity` | Cigarettes per day | numeric > 0 |
| `duration_smoking` | Total years smoked | numeric |
| `smoking_quit_time` | Years since quitting | numeric (0 if current) |

`race` accepts case-insensitive labels and common aliases (e.g. `"Caucasian"`,
`"African American"`, `"Latino"`, `"Pacific Islander"`) as well as the numeric
codes 1–6.

---

## ModelsCloud entry points

The functions the ModelsCloud (pexa) executor calls — the package's hosted API:

| Function | Description |
|---|---|
| `model_run(model_input)` | Score a named list (one patient) or data frame (one row per patient); returns the input plus `risk` and `risk_percent`. |
| `get_sample_input(n)` | An example cohort, ready to pass to `model_run()`. |
| `get_default_input()` | One baseline patient to modify. |

Once hosted, end users call it through the
[`modelscloud`](https://github.com/resplab/modelscloud) client:

```r
library(modelscloud)
connect_to_model("raminrzn/plcom2012", access_key = "YOUR_API_KEY")
result <- model_run(get_sample_input())
```

---

## Clinical interpretation

PLCOm2012 estimates 6-year lung cancer risk to help identify candidates for
low-dose CT screening. Commonly used eligibility thresholds are **≥1.5%** (as in
the original NEJM analysis), **≥1.7%**, and **≥2.0%**, depending on the
jurisdiction and screening program. This package returns the raw probability and
does **not** impose a threshold — interpretation is left to the user / program.

> This software is for research use. It is **not** a medical device and is not a
> substitute for clinical judgement.

---

## Reference

> Tammemägi MC, Katki HA, Hocking WG, et al. Selection criteria for lung-cancer
> screening. *N Engl J Med.* 2013;368(8):728–736.
> doi:[10.1056/NEJMoa1211776](https://doi.org/10.1056/NEJMoa1211776)
> (correction: *N Engl J Med.* 2013;369(4):394).

## License

GPL-3. Model © its original authors; package implementation © Ramin Rezaeianzadeh.

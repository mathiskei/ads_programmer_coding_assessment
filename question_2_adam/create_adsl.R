# =====================================================================
# ADaM ADSL Dataset Creation — Script + Run Log
# =====================================================================
# Purpose : Generate an ADaM Subject-Level Analysis Dataset (ADSL)
#           using {admiral} derivation functions.
# Author  : Kei Mathis
# Date    : 2026-02-11
#
# Inputs  : pharmaversesdtm::dm, ds, ex, ae, vs
# Outputs : - question_2_adam/01_create_ae_summary_table_log.txt
#           - question_2_adam/adsl.csv
#
# Notes   : - Logging is captured via sink() for evidence of successful execution.
#           - Session information is printed at run start for reproducibility.
#           - No derivation logic has been modified; formatting and documentation
#             only.
#           - Key derivation logic is described inline (e.g., how records are
#             selected, what constitutes “first/last”, and how flags/dates are
#             derived).
# =====================================================================


# ---------------------------------------------------------------------
# 0. Run Log Setup (Evidence of Error-Free Execution)
# ---------------------------------------------------------------------
sink("question_2_adam/create_adsl_log.txt", split = TRUE)

print("Starting Script")
cat("\n\n--- SESSION INFO ---\n")
print(sessionInfo())


# ---------------------------------------------------------------------
# 1. Setup / Packages
# ---------------------------------------------------------------------

# Install packages (if needed)
# install.packages("admiral")
# install.packages("pharmaversesdtm")
# install.packages("tidyverse")

# Load packages
library(admiral)
library(pharmaversesdtm)
library(tidyverse)

# Additional commonly used packages (loaded explicitly for clarity)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(stringr)


# ---------------------------------------------------------------------
# 2. Load Source Data
# ---------------------------------------------------------------------
# Source datasets (example SDTM) from {pharmaversesdtm}
# - dm: Demographics
# - ds: Disposition
# - ex: Exposure
# - ae: Adverse Events
# - vs: Vital Signs
#
# Key concept:
# - DM acts as the subject “backbone” for ADSL.
# - EX provides treatment exposure dates (start/end).
# - AE/VS/DS provide candidate dates used later for derived endpoints.

dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
vs <- pharmaversesdtm::vs

# Convert blank character values to NA to avoid incorrect derivations/filters
# (e.g., empty strings being treated as real values in conditions/joins).
dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)

# Initialize ADSL from DM (remove DOMAIN to align with ADaM conventions).
# DM typically contains DOMAIN = "DM"; ADSL does not use SDTM DOMAIN.
adsl <- dm %>%
  select(-DOMAIN)


# ---------------------------------------------------------------------
# 3. Derivations
# ---------------------------------------------------------------------

# =============================================================
# 3.1 AGEGR9 / AGEGR9N
# =============================================================
# Goal:
# - Derive categorical age group variables from AGE for summarization and analysis.
#
# Logic:
# - AGEGR9  : character labels for age groups
# - AGEGR9N : numeric codes aligned with AGEGR9
#
# Note:
# - Cutoffs/labels are kept unchanged per request (even if label says "18-64"
#   while condition uses 18–50; this script is documentation-only).

agegr9_lookup <- exprs(
  ~condition,           ~AGEGR9,
  AGE < 18,               "<18",
  between(AGE, 18, 50),   "18-64",
  AGE > 50,               ">50",
  is.na(AGE),             "Missing"
)

agegr9n_lookup <- exprs(
  ~condition,           ~AGEGR9N,
  AGE < 18,               1,
  between(AGE, 18, 50),   2,
  AGE > 50,               3,
  is.na(AGE),             NA
)

# derive_vars_cat() applies the lookup definitions to create the derived variables.
adsl <- adsl %>%
  derive_vars_cat(definition = agegr9_lookup) %>%
  derive_vars_cat(definition = agegr9n_lookup)


# =============================================================
# 3.2 TRTSDTM / TRTSTMF (Treatment Start Datetime + Flag)
# =============================================================
# Goal:
# - Identify treatment start datetime (TRTSDTM) per subject from EX records.
# - Carry the imputation flag (TRTSTMF) from the derived EX datetime.
#
# Key logic:
# 1) derive_vars_dtm() converts EXSTDTC to EXSTDTM:
#    - time_imputation = "first" imputes a missing time to the start of day
#    - highest_imputation = "h" controls the “highest” imputation level flag
#    - ignore_seconds_flag = TRUE removes seconds from imputation considerations
#
# 2) derive_vars_merged():
#    - filters exposure records to those representing actual treatment:
#        (EXDOSE > 0) OR (placebo with EXDOSE == 0 and EXTRT contains "PLACEBO")
#    - requires non-missing EXSTDTM after parsing
#    - selects the FIRST record per subject based on:
#        order = EXSTDTM, EXSEQ
#      and mode = "first"
#    - merges EXSTDTM/EXSTTMF into ADSL as TRTSDTM/TRTSTMF

ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    time_imputation = "first",
    highest_imputation = "h",
    ignore_seconds_flag = TRUE
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add =
      (EXDOSE > 0 |
         (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  )

# Structure check (informational; helps verify derived variables exist and types look correct).
glimpse(adsl)


# =============================================================
# 3.3 ITTFL (Intent-to-Treat Flag)
# =============================================================
# Goal:
# - Derive intent-to-treat population flag.
#
# Key logic:
# - If ARM is present (non-missing): ITTFL = "Y"
# - If ARM is missing:             ITTFL = "N"
#
# Rationale:
# - In many ADaM implementations, ITT includes all randomized subjects;
#   here, ARM presence is used as a proxy for assignment.

ittfl_lookup <- exprs(
  ~condition, ~ITTFL,
  !is.na(ARM), "Y",
  is.na(ARM),  "N"
)

adsl <- adsl %>%
  derive_vars_cat(definition = ittfl_lookup)


# =============================================================
# 3.4 TRTEDTM (Treatment End Datetime)
# =============================================================
# Goal:
# - Identify treatment end datetime (TRTEDTM) per subject from EX records.
#
# Key logic:
# 1) derive_vars_dtm() converts EXENDTC to EXENDTM:
#    - time_imputation = "last" imputes missing time to end of day
#
# 2) derive_vars_merged():
#    - uses the same “qualifying exposure record” logic as TRTSDTM
#    - requires non-missing EXENDTM
#    - selects the LAST record per subject based on:
#        order = EXENDTM, EXSEQ
#      and mode = "last"

ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add =
      (EXDOSE > 0 |
         (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )


# =============================================================
# 3.5 LSTALVDT (Last Alive Date)
# =============================================================
# Goal:
# - Derive the last known alive date/time per subject (LSTALVDT) by scanning
#   several contributing sources and selecting the latest available.
#
# Key logic:
# - derive_vars_extreme_event() evaluates a set of “events” (candidate dates)
#   and returns the last (mode = "last") according to the provided order.
#
# Candidate events (in this script):
# 1) VS: last vital signs date where a result exists and VSDTC is non-missing
# 2) AE: last adverse event start date (AESTDTC)
# 3) DS: last disposition start date (DSSTDTC)
# 4) ADSL: treatment end datetime (TRTEDTM)
#
# Date handling:
# - convert_dtc_to_dt(..., highest_imputation = "n") converts DTC to a Date
#   with no imputation (“n” = none).
#
# Ordering:
# - order = exprs(VSDTC, VSSEQ, AESTDTC, AESEQ, DSSTDTC, DSSEQ)
#   controls how ties/sequence are handled when determining the “last” event.

adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      event(
        dataset_name = "vs",
        condition = (!is.na(VSSTRESN) | !is.na(VSSTRESC)) & !is.na(VSDTC),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(VSDTC, highest_imputation = "n")
        )
      ),
      event(
        dataset_name = "ae",
        condition = !is.na(AESTDTC),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(AESTDTC, highest_imputation = "n")
        )
      ),
      event(
        dataset_name = "ds",
        condition = !is.na(DSSTDTC),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(DSSTDTC, highest_imputation = "n")
        )
      ),
      event(
        dataset_name = "adsl",
        condition = !is.na(TRTEDTM),
        set_values_to = exprs(
          LSTALVDT = TRTEDTM
        )
      )
    ),
    source_datasets = list(ae = ae, vs = vs, ds = ds, adsl = adsl),
    mode = "last",
    order = exprs(VSDTC, VSSEQ, AESTDTC, AESEQ, DSSTDTC, DSSEQ),
    new_vars = exprs(LSTALVDT)
  )


# ---------------------------------------------------------------------
# 4. Output
# ---------------------------------------------------------------------
# Write ADSL dataset to CSV.
write_csv(adsl, "question_2_adam/adsl.csv")


# ---------------------------------------------------------------------
# 5. Close Log
# ---------------------------------------------------------------------
print("Script finished successfully.")

sink()

# =====================================================================
# SDTM DS Domain Creation — Script + Run Log
# =====================================================================
# Purpose : Generate the SDTM Disposition (DS) domain from raw data using
#           {sdtm.oak} derivation/assignment functions and controlled terminology.
# Author  : Kei Mathis
# Date    : 2026-02-11
#
# Inputs  : - question_1_sdtm/sdtm_ct.csv
#           - pharmaverseraw::ds_raw
# Outputs : - question_1_sdtm/create_ds_domain_log.txt
#           - question_1_sdtm/ds_domain.csv
#
# Notes   : - Logging is captured via sink() for evidence the script ran.
#           - Session info is printed at run start for reproducibility.
#           - No logic changes were made; formatting + documentation only.
#           - Key logic is explained inline for each derivation/assignment step.
# =====================================================================


# ---------------------------------------------------------------------
# 0. Run Log Setup (Evidence of Error-Free Execution)
# ---------------------------------------------------------------------
sink("question_1_sdtm/create_ds_domain_log.txt", split = TRUE)

print("Starting Script")
cat("\n\n--- SESSION INFO ---\n")
print(sessionInfo())


# ---------------------------------------------------------------------
# 1. Packages
# ---------------------------------------------------------------------

## Install packages (if needed)
# install.packages("sdtm.oak")
# install.packages("pharmaverseraw")
# install.packages("tidyverse")

## Load packages
library(sdtm.oak)
library(pharmaverseraw)
library(tidyverse)


# ---------------------------------------------------------------------
# 2. Load Data
# ---------------------------------------------------------------------

## Load controlled terminology (CT) specification used by {sdtm.oak}
## - Provides mappings and permitted values for SDTM variables.
sdtm_ct <- read.csv("question_1_sdtm/sdtm_ct.csv")

## Load raw DS data from {pharmaverseraw}
ds_raw <- pharmaverseraw::ds_raw

## Quick exploration (informational)
glimpse(ds_raw)


# ---------------------------------------------------------------------
# 3. Preprocess Raw Data
# ---------------------------------------------------------------------

## Generate unique OAK ID variables for subjects/records
## - Adds standard OAK identifier fields used by id_vars = oak_id_vars()
## - pat_var: subject identifier in raw data
## - raw_src: source name recorded for traceability
ds_raw <- generate_oak_id_vars(
  raw_dat = ds_raw,
  pat_var = "PATNUM",
  raw_src = "ds_raw"
)


# ---------------------------------------------------------------------
# 4. Create DS Domain Variables
# ---------------------------------------------------------------------
# General approach:
# - Use assign_ct()/hardcode_ct() to apply controlled terminology where applicable.
# - Use assign_no_ct()/hardcode_no_ct() for variables not governed by CT.
# - Use assign_datetime() to construct SDTM ISO 8601 date/time strings.
# - Use derive_seq() to create sequence numbers within subject.
# - Use derive_study_day() to derive relative study day variables.


# =============================================================
# 4.1 DSTERM (Disposition Term)
# =============================================================
# Logic:
# - If OTHERSP is populated, use OTHERSP as the disposition term.
# - Otherwise, use IT.DSTERM.
# - assign_ct() applies CT class "C66727" to standardize values and ensure
#   consistency with SDTM controlled terminology.
ds <- assign_ct(
  raw_dat  = condition_add(ds_raw, !is.na(OTHERSP)),
  raw_var  = "OTHERSP",
  tgt_var  = "DSTERM",
  ct_spec  = sdtm_ct,
  ct_clst  = "C66727",
  id_vars  = oak_id_vars()
) %>%
  assign_ct(
    raw_dat  = condition_add(ds_raw, is.na(OTHERSP)),
    raw_var  = "IT.DSTERM",
    tgt_var  = "DSTERM",
    ct_spec  = sdtm_ct,
    ct_clst  = "C66727",
    id_vars  = oak_id_vars()
  )


# =============================================================
# 4.2 DSSTDTC (Start Date/Time of Disposition Event)
# =============================================================
# Logic:
# - Convert IT.DSSTDAT (raw date) into SDTM ISO 8601 date string DSSTDTC.
# - raw_fmt indicates the incoming raw format (here: m-d-y).
ds <- ds %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = c("m-d-y"),
    id_vars = oak_id_vars()
  )


# =============================================================
# 4.3 DSDECOD (Standardized Disposition Term)
# =============================================================
# Logic:
# - DSDECOD is the standardized (coded) disposition term.
# - If OTHERSP is missing, code from IT.DSDECOD.
# - If OTHERSP is present, use OTHERSP to populate DSDECOD (and map using CT).
ds <- ds %>%
  assign_ct(
    raw_dat  = condition_add(ds_raw, is.na(OTHERSP)),
    raw_var  = "IT.DSDECOD",
    tgt_var  = "DSDECOD",
    ct_spec  = sdtm_ct,
    ct_clst  = "C66727",
    id_vars  = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat  = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var  = "OTHERSP",
    tgt_var  = "DSDECOD",
    ct_spec  = sdtm_ct,
    ct_clst  = "C66727",
    id_vars  = oak_id_vars()
  )


# =============================================================
# 4.4 DSCAT (Disposition Category)
# =============================================================
# Logic:
# - DSCAT categorizes the disposition event.
# - If IT.DSDECOD == "Randomized" => "PROTOCOL MILESTONE" (CT class C74558)
# - If IT.DSDECOD is non-missing and not "Randomized" => "DISPOSITION EVENT"
# - If OTHERSP is populated but IT.DSDECOD is missing => "OTHER EVENT"
#   (no CT enforced for this branch via hardcode_no_ct()).
ds <- ds %>%
  hardcode_ct(
    raw_dat  = condition_add(ds_raw, IT.DSDECOD == "Randomized"),
    raw_var  = "IT.DSDECOD",
    tgt_var  = "DSCAT",
    tgt_val  = "PROTOCOL MILESTONE",
    ct_spec  = sdtm_ct,
    ct_clst  = "C74558",
    id_vars  = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat  = condition_add(ds_raw, IT.DSDECOD != "Randomized" & !is.na(IT.DSDECOD)),
    raw_var  = "IT.DSDECOD",
    tgt_var  = "DSCAT",
    tgt_val  = "DISPOSITION EVENT",
    ct_spec  = sdtm_ct,
    ct_clst  = "C74558",
    id_vars  = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat  = condition_add(ds_raw, !is.na(OTHERSP) & is.na(IT.DSDECOD)),
    raw_var  = "OTHERSP",
    tgt_var  = "DSCAT",
    tgt_val  = "OTHER EVENT"
  )


# =============================================================
# 4.5 DSDTC (Date/Time of Disposition Event)
# =============================================================
# Logic:
# - Construct a combined date/time ISO 8601 string using two raw inputs:
#     DSTMCOL (time; format H:M) and DSDTCOL (date; format m-d-y).
# - assign_datetime() merges these into DSDTC.
ds <- ds %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSTMCOL", "DSDTCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("H:M", "m-d-y"),
    id_vars = oak_id_vars()
  )


# =============================================================
# 4.6 STUDYID (Study Identifier)
# =============================================================
# Logic:
# - Populate STUDYID from raw STUDY variable (no CT).
ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "STUDY",
    tgt_var = "STUDYID"
  )


# =============================================================
# 4.7 DOMAIN (Domain Name)
# =============================================================
# Logic:
# - Hardcode SDTM DOMAIN constant = "DS".
ds <- ds %>%
  mutate(DOMAIN = "DS")


# =============================================================
# 4.8 USUBJID (Unique Subject Identifier)
# =============================================================
# Logic:
# - Populate USUBJID from raw subject ID PATNUM (no CT).
ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "PATNUM",
    tgt_var = "USUBJID"
  )


# =============================================================
# 4.9 DSSEQ (Sequence Number)
# =============================================================
# Logic:
# - Derive DSSEQ as a record-level sequence within each subject.
# - rec_vars define the record uniqueness used for sequencing.
# - sbj_vars specify the subject grouping variable.
ds <- derive_seq(
  ds,
  tgt_var = "DSSEQ",
  rec_vars = c("USUBJID", "DSTERM"),
  sbj_vars = "USUBJID"
)


# =============================================================
# 4.10 VISITNUM (Numeric Visit Identifier)
# =============================================================
# Logic:
# - Map raw INSTANCE to VISITNUM using CT list "VISITNUM".
ds <- ds %>%
  assign_ct(
    raw_dat  = ds_raw,
    raw_var  = "INSTANCE",
    tgt_var  = "VISITNUM",
    ct_spec  = sdtm_ct,
    ct_clst  = "VISITNUM",
    id_vars  = oak_id_vars()
  )


# =============================================================
# 4.11 VISIT (Visit Name)
# =============================================================
# Logic:
# - Map raw INSTANCE to VISIT using CT list "VISIT".
ds <- ds %>%
  assign_ct(
    raw_dat  = ds_raw,
    raw_var  = "INSTANCE",
    tgt_var  = "VISIT",
    ct_spec  = sdtm_ct,
    ct_clst  = "VISIT",
    id_vars  = oak_id_vars()
  )


# =============================================================
# 4.12 DSSTDY (Study Day of Start of Disposition Event)
# =============================================================
# Logic:
# - Derive DSSTDY using derive_study_day().
# - tgdt/refdt specify target and reference date variables.
# - merge_key specifies the subject identifier used for merging.
#
# NOTE: This call is left unchanged as requested, even though dm_domain
# is typically DM in SDTM pipelines.
ds <- derive_study_day(
  sdtm_in = ds,
  dm_domain = ds,
  tgdt = "DSSTDTC",
  refdt = "DSDTC",
  study_day_var = "DSSTDY",
  merge_key = "patient_number"
)


# ---------------------------------------------------------------------
# 5. Export DS Domain
# ---------------------------------------------------------------------
# Select required SDTM variables in standard order and write to CSV.

ds <- ds %>%
  select(
    "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM",
    "DSDECOD", "DSCAT", "VISITNUM", "VISIT",
    "DSDTC", "DSSTDTC", "DSSTDY"
  )

write_csv(ds, "question_1_sdtm/ds_domain.csv")


# ---------------------------------------------------------------------
# 6. Close Log
# ---------------------------------------------------------------------
sink

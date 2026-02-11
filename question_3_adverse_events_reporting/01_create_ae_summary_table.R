# =====================================================================
# AE SUMMARY TABLE (TEAEs) — Script + Run Log
# =====================================================================
# Purpose : Create an AE Summary Table (Treatment-Emergent AEs) by arm,
#           including an overall column, then export as HTML.
# Author  : Kei Mathis
# Date    : 2026-02-11
#
# Inputs  : pharmaverseadam::adae, pharmaverseadam::adsl
# Outputs : - question_3_adverse_events_reporting/01_create_ae_summary_table_log.txt
#           - question_3_adverse_events_reporting/ae_summary_table.html
#
# Notes   : - Logging is captured via sink() for evidence the script ran.
#           - Session info is printed at run start for reproducibility.
#           - TEAEs are defined via TRTEMFL == "Y".
#           - No analysis logic has been modified; formatting + documentation only.
# =====================================================================


# ---------------------------------------------------------------------
# 0. Run Log Setup (Evidence of Error-Free Execution)
# ---------------------------------------------------------------------
sink("question_3_adverse_events_reporting/01_create_ae_summary_table_log.txt", split = TRUE)

print("Starting Script")
cat("\n\n--- SESSION INFO ---\n")
print(sessionInfo())


# ---------------------------------------------------------------------
# 1. Setup / Packages
# ---------------------------------------------------------------------

# Install packages (if needed)
# install.packages("pharmaverseadam")
# install.packages("tidyverse")
# install.packages("gtsummary")

# Load packages
library(pharmaverseadam)
library(tidyverse)
library(gtsummary)


# ---------------------------------------------------------------------
# 2. Load Data
# ---------------------------------------------------------------------
# Inputs:
# - ADAE: event-level adverse events (multiple records per subject possible)
# - ADSL: subject-level population dataset used as denominator for subject counts
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl


# ---------------------------------------------------------------------
# 3. Filter to Treatment-Emergent Adverse Events (TEAEs)
# ---------------------------------------------------------------------
# Key logic:
# - TRTEMFL == "Y" restricts to treatment-emergent AEs (TEAEs) per ADaM convention.
# - Subsequent tabulations are therefore based on TEAE records only.
adae_teae <- adae %>%
  filter(TRTEMFL == "Y")


# ---------------------------------------------------------------------
# 4. Create Summary Table (SOC by Treatment Arm)
# ---------------------------------------------------------------------
# Goal:
# - Provide a high-level summary of TEAEs by System Organ Class (AESOC) and arm.
#
# Key logic:
# - tbl_summary() summarizes categorical variables by the grouping variable (by = ACTARM).
# - sort = all_categorical() ~ "frequency" orders categories by decreasing frequency
#   within the table output.
# - add_overall(last = TRUE) adds an overall column as the last column.
# - modify_header() customizes the display label for the first column.
tbl_summary(
  adae_teae,
  include = c(AESOC),
  by = ACTARM,
  sort = all_categorical() ~ "frequency"
) %>%
  add_overall(last = TRUE) %>%
  modify_header(label = "**Primary System Organ Class <br> Reported Term for the Adverse Events**")


# ---------------------------------------------------------------------
# 5. Create Hierarchical AE Table (SOC by Treatment Arm + Overall)
# ---------------------------------------------------------------------
# Goal:
# - Create a subject-based hierarchical TEAE table by SOC, stratified by treatment arm,
#   including an overall row.
#
# Key logic / “derivations” inside tbl_hierarchical():
# - variables = c(AESOC):
#     - defines the hierarchical level(s) to tabulate (here: SOC level).
# - by = ACTARM:
#     - stratifies counts/percentages by treatment arm.
# - id = USUBJID:
#     - specifies the subject identifier; ensures counts are subject-based (i.e.,
#       each subject is counted once per category, not once per event record).
# - denominator = adsl:
#     - defines the subject-level denominator (typically all subjects in the analysis
#       population). This avoids inflating percentages by using only subjects with AEs.
# - overall_row = TRUE:
#     - adds an overall row in the hierarchical table.
# - label = "..ard_hierarchical_overall.." ~ "Treatment Emergent AEs":
#     - labels the overall row/header for the hierarchical output.
#
# - modify_header() applies the formatted column header label.
tbl <- adae_teae %>%
  tbl_hierarchical(
    variables = c(AESOC),
    by = ACTARM,
    id = USUBJID,
    denominator = adsl,
    overall_row = TRUE,
    label = "..ard_hierarchical_overall.." ~ "Treatment Emergent AEs"
  ) %>%
  modify_header(label = "**Primary System Organ Class <br> Reported Term for the Adverse Events**")


# ---------------------------------------------------------------------
# 6. Sort and Print Table
# ---------------------------------------------------------------------
# Key logic:
# - sort_hierarchical() applies a standard hierarchical sorting to the output table
#   (e.g., ordering categories by frequency according to the function’s defaults).
# - Printing tbl ensures it is rendered in the output/log for traceability.
tbl <- sort_hierarchical(tbl)
tbl


# ---------------------------------------------------------------------
# 7. Export Table to HTML
# ---------------------------------------------------------------------
# Key logic:
# - as_gt() converts the gtsummary object into a {gt} table object.
# - gt::gtsave() writes the table to an HTML file for reporting/sharing.
tbl %>%
  as_gt() %>%
  gt::gtsave("question_3_adverse_events_reporting/ae_summary_table.html")


# ---------------------------------------------------------------------
# 8. Close Log
# ---------------------------------------------------------------------
sink()

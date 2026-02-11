# =====================================================================
# VISUALIZATIONS — Script + Run Log
# =====================================================================
# Purpose : Create AE visualizations and export as PNG files.
# Author  : Kei Mathis
# Date    : 2026-02-11
#
# Inputs  : pharmaverseadam::adae, pharmaverseadam::adsl
# Outputs : - question_3_adverse_events_reporting/02_create_visualizations_log.txt
#           - question_3_adverse_events_reporting/plot_1.png
#           - question_3_adverse_events_reporting/plot_2.png
#
# Notes   : - Logging is captured via sink() for evidence the script ran.
#           - Session info is printed at run start for reproducibility.
#           - Plot 1: AE severity distribution by treatment arm (event counts).
#           - Plot 2: Top 10 most frequent AEs (subject-based), with 95% exact
#             binomial (Clopper–Pearson) confidence intervals.
#           - No analysis logic has been modified; formatting + documentation
#             only.
# =====================================================================


# ---------------------------------------------------------------------
# 0. Run Log Setup (Evidence of Error-Free Execution)
# ---------------------------------------------------------------------
sink("question_3_adverse_events_reporting/02_create_visualizations_log.txt", split = TRUE)

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
# install.packages("scales")

# Load packages
library(pharmaverseadam)
library(tidyverse)
library(gtsummary)
library(scales)


# ---------------------------------------------------------------------
# 2. Load Data
# ---------------------------------------------------------------------
# Inputs:
# - ADAE: event-level adverse events (multiple records per subject possible)
# - ADSL: subject-level population dataset (loaded for completeness/consistency)
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl


# ---------------------------------------------------------------------
# 3. Plot 1 — AE Severity Distribution by Treatment Arm
# ---------------------------------------------------------------------
# Goal:
# - Summarize and visualize the distribution of AE severity (AESEV) by treatment arm.
#
# Key logic:
# - count(ACTARM, AESEV) produces event counts per arm x severity combination.
#   This is event-based (not subject-based): a subject with multiple AEs contributes
#   multiple records to the counts.
adae_plot <- adae %>%
  count(ACTARM, AESEV)

plot_1 <- ggplot(data = adae_plot, aes(x = ACTARM, y = n, fill = AESEV)) +
  geom_col() +
  theme(
    legend.position = "right"
  ) +
  labs(
    title = "AE severity distribution by treatment",
    y = "Count of AE",
    x = "Treatment Arm"
  )

# Export Plot 1 as PNG (publication/report-ready resolution)
ggsave(
  filename = "question_3_adverse_events_reporting/plot_1.png",
  plot = plot_1,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)


# ---------------------------------------------------------------------
# 4. Plot 2 — Top 10 Most Frequent Adverse Events (Subject-Based)
# ---------------------------------------------------------------------
# Goal:
# - Identify the top 10 most frequent adverse events (by preferred term AEDECOD)
#   and plot subject-based percentages with 95% exact binomial confidence intervals.
#
# Key logic:
# 1) N:
#    - Total number of unique subjects in ADAE (used as denominator).
#
# 2) distinct(USUBJID, AEDECOD):
#    - Converts event-level ADAE to subject-level “ever had this AEDECOD”.
#    - Ensures each subject contributes at most 1 count per term, preventing
#      inflated percentages due to repeated events for the same subject/term.
#
# 3) pct:
#    - Subject-based proportion = n / N.
#
# 4) ci / low / high:
#    - Exact (Clopper–Pearson) 95% CI from stats::binom.test().
#
# 5) slice_max(n = 10, order_by = pct):
#    - Selects the 10 terms with the highest percentages.
#
# 6) fct_rev(as_factor(AEDECOD)):
#    - Reverses factor order so the plot displays the most frequent term at the top
#      when using a horizontal layout (y = AEDECOD).
#
# NOTE:
# - The plot displays proportions on the x-axis formatted as percentages.
# - The code below is unchanged; comments added for interpretation only.

# Total number of subjects (used in subtitle and CI calculation)
N <- n_distinct(adae$USUBJID)

adae_plot2 <- adae %>%
  distinct(USUBJID, AEDECOD) %>%
  count(AEDECOD, name = "n") %>%
  mutate(
    N = n_distinct(adae$USUBJID),
    pct = n / N,
    ci = map2(n, N, ~ stats::binom.test(.x, .y)$conf.int),
    low = map_dbl(ci, 1),
    high = map_dbl(ci, 2)
  ) %>%
  slice_max(n = 10, order_by = pct) %>%
  mutate(
    AEDECOD = fct_rev(as_factor(AEDECOD))
  )

plot_2 <- ggplot(adae_plot2, aes(x = pct, y = AEDECOD)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = low, xmax = high), width = 0.2) +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", N, " subjects; 95% Clopper-Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  scale_x_continuous(labels = percent_format())

# Print Plot 2 to the current graphics device
plot_2

# Export Plot 2 as PNG (publication/report-ready resolution)
ggsave(
  filename = "question_3_adverse_events_reporting/plot_2.png",
  plot = plot_2,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)


# ---------------------------------------------------------------------
# 5. Close Log
# ---------------------------------------------------------------------
print("Script finished successfully.")

sink()

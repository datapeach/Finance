# global.R

# Author: Rosie Schmitz

# Get current calendar year (IRS limits are year-based)
current_year <- as.integer(format(Sys.Date(), "%Y"))

# Define global contribution limits (update yearly)
if (current_year <= 2025) {
  # 2025 limits
  lim401k     <- 23500
  limrothira  <- 7000
  limhsa_one  <- 4300
  limhsa_two  <- 8550
} else {
  # 2026 limits
  lim401k     <- 24500
  limrothira  <- 7500
  limhsa_one  <- 4400
  limhsa_two  <- 8750
}

# Catch-up Contributions
calculate_limits <- function(age) {
  list(
    lim401k50    = ifelse(age >= 50, lim401k + 7500, lim401k),
    limrothira50 = ifelse(age >= 50, limrothira + 1000, limrothira),
    limhsaone55  = ifelse(age >= 55, limhsa_one + 1000, limhsa_one),
    limhsatwo55  = ifelse(age >= 55, limhsa_two + 1000, limhsa_two)
  )
}


# global.R

# Author: Rosie Schmitz

# Shiny sources this file once before ui.R and server.R, so everything the app
# needs is loaded here.

source("packages.R")

# Get current calendar year (IRS limits are year-based)
current_year <- as.integer(format(Sys.Date(), "%Y"))

# Contribution limits live in contribution_limits.csv (one row per tax year).
# Add a row each fall when the IRS announces the next year's limits.
limits_table <- read.csv("contribution_limits.csv")
limits_year <- if (any(limits_table$year <= current_year)) {
  max(limits_table$year[limits_table$year <= current_year])
} else {
  min(limits_table$year)
}
limits_row <- limits_table[limits_table$year == limits_year, ]

limits_note <- if (limits_year == current_year) {
  paste0("Using ", limits_year, " IRS contribution limits.")
} else {
  paste0("No ", current_year, " row in contribution_limits.csv yet; using ", limits_year, " IRS limits.")
}

# Catch-up Contributions
calculate_limits <- function(age) {
  catchup401k <- if (isTRUE(age >= 60 && age <= 63)) {
    limits_row$catchup401k_60_63
  } else if (isTRUE(age >= 50)) {
    limits_row$catchup401k_50
  } else {
    0
  }
  list(
    lim401k50    = limits_row$lim401k + catchup401k,
    limrothira50 = limits_row$limira + if (isTRUE(age >= 50)) limits_row$catchupira_50 else 0,
    limhsaone55  = limits_row$limhsa_one + if (isTRUE(age >= 55)) limits_row$catchuphsa_55 else 0,
    limhsatwo55  = limits_row$limhsa_two + if (isTRUE(age >= 55)) limits_row$catchuphsa_55 else 0
  )
}

# Highest limit any age can reach (age 61 is in every catch-up tier); used as input maximums
max_limits <- calculate_limits(61)

# UI constants
figure.height <- '600px'
loading.spinner.color <- "#6f42c1"

source("defaults.R")

# Calculations
source("budgetfunc.R")
source("investfunc.R")
source("retirementgrowthfunc.R")
source("montecarlofunc.R")

# One Shiny module per tab
source("modules/mod_budget.R")
source("modules/mod_invest.R")
source("modules/mod_retirement.R")

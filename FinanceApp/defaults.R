# defaults.R

# Author: Rosie Schmitz

# Generic starting values for every input. Personal numbers go in defaults_local.R
# (git-ignored), which overrides these when it exists.

app_defaults <- list(
  budget = list(
    take_home = 5000, rent_mort = 1500, car_payment = 0, groceries = 500,
    home_ins = 50, home_maint = 100, car_ins = 120, car_maint = 50,
    utilities = 150, phones = 80, pets = 0, subscriptions = 30,
    dining_out = 200, hobbies = 150, savings = 500, investing = 500
  ),
  invest = list(
    money_to_invest = 1000,
    num_investments = 3
  ),
  p1 = list(
    age = 30, salary = 70000, wagegrowth = 3, emp_match = 5, emp_contrib = 10000,
    ratereturn = 7, roth = 5000, brokerage = 0, hsa = 0, lastworkyear = current_year + 30,
    current401k = 50000, currentroth = 20000, currentbrokerage = 10000, currenthsa = 0
  ),
  p2 = list(
    age = 30, salary = 60000, wagegrowth = 3, emp_match = 5, emp_contrib = 10000,
    ratereturn = 7, roth = 5000, brokerage = 0, hsa = 0, lastworkyear = current_year + 30,
    current401k = 40000, currentroth = 15000, currentbrokerage = 5000, currenthsa = 0
  ),
  plan = list(
    plan_through_age = 95,
    anchor_year = current_year + 31,
    inflation = 3,
    withdraw_start_year = current_year + 31,
    vpw_rate = 3,
    annual_spend = 60000,
    withdraw_rule = "capped",
    retire_return = 7,
    fire_target = 1500000,
    spousal_roth = TRUE
  ),
  monte_carlo = list(
    horizon = 50, rates = "3, 3.5, 4, 4.5, 5, 5.5", floor = 60000,
    vol = 16, sims = 10000, seed = 42
  )
)

if (file.exists("defaults_local.R")) source("defaults_local.R")

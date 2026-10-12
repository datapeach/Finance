# Author: Rosie Schmitz & David Nita

#' retirement.growth()
#' 
#' @description 
#' returns a dataframe for retirement growth to be used in other places of the app
#' loaded in the main application

# retirementgrowthfunc.R

retirement.growth <- function(age, wagegrowth, emp_match, emp_contrib, ratereturn, roth, brokerage, hsa, current_year, 
                              current401k, currentroth, currentbrokerage, currenthsa, salary) {
 
  salary <- salary * (1 + wagegrowth)
  employer_match_dollar <- emp_match * salary
  bal_401k <- (current401k) * (1 + ratereturn) + (employer_match_dollar + emp_contrib)
  bal_roth <- (currentroth) * (1 + ratereturn) + (roth)
  bal_brokerage <- (currentbrokerage) * (1 + ratereturn) + (brokerage)
  bal_hsa <- (currenthsa) * (1 + ratereturn) + (hsa)
  balance <- bal_401k + bal_roth + bal_hsa + bal_brokerage
  
  df <- data.frame(
    Year = current_year + 1,
    Age = age + 1,
    Salary = salary,
    Employer_Contrib = employer_match_dollar,
    Employee_Contrib = emp_contrib,
    Roth = bal_roth,
    Brokerage = bal_brokerage,
    HSA = bal_hsa, 
    `401k` = bal_401k,
    Balance = balance
  )
  
  return(df)
}

#' retirement.projection()
#'
#' @description
#' Year-by-year account growth for one person. Contributions stop after
#' last_work_year; a retired person can keep a spousal Roth while the spouse
#' still has earned income (spouse_last_work_year).
retirement.projection <- function(projection, starting_401k, starting_roth, starting_brokerage, starting_hsa, starting_salary,
                                  wage_growth, rate_of_return, age, employer_match, employee_contrib, roth_contrib, brokerage_contrib, hsa_contrib,
                                  last_work_year = Inf, spouse_last_work_year = NA, spousal_roth = FALSE,
                                  start_year = as.numeric(format(Sys.Date(), "%Y"))) {
  
  wage_growth <- wage_growth/100
  rate_of_return <- rate_of_return/100
  employer_match <- employer_match/100
  current_year <- start_year
  
  contribs_for_year <- function(year) {
    working <- year <= last_work_year
    spousal <- !working && isTRUE(spousal_roth) && !is.na(spouse_last_work_year) && year <= spouse_last_work_year
    list(
      working   = working,
      emp_match = if (working) employer_match else 0,
      emp_contrib = if (working) employee_contrib else 0,
      roth      = if (working || spousal) roth_contrib else 0,
      brokerage = if (working) brokerage_contrib else 0,
      hsa       = if (working) hsa_contrib else 0
    )
  }
  
  df <- data.frame(
    Year = rep(0, projection + 1),
    Age = rep(0, projection + 1),
    Salary = rep(0, projection + 1),
    Employer_Contrib = rep(0, projection + 1),
    Employee_Contrib = rep(0, projection + 1),
    Roth = rep(0, projection + 1),
    Brokerage = rep(0, projection +1),
    HSA = rep(0, projection + 1), 
    Bal_401k = rep(0, projection + 1),
    Balance = rep(0, projection + 1)
  )
  # this is initializing the first row (previous year)
  df[1, ] <- retirement.growth(age - 2, 0, 0, 0, 0, 0, 0, 0, current_year - 2,
                               starting_401k, starting_roth, starting_brokerage, starting_hsa, 0) 
  
  
  # this is initializing the second row (current year)
  c2 <- contribs_for_year(current_year)
  df[2, ] <- retirement.growth(age - 1, 0, c2$emp_match, c2$emp_contrib, rate_of_return, c2$roth, c2$brokerage, c2$hsa, current_year - 1,
                              starting_401k, starting_roth, starting_brokerage, starting_hsa,
                              if (c2$working) starting_salary else 0) 

  
  if (projection >= 2) {
    for( i in 3:(projection + 1)) { #rowwise
      ci <- contribs_for_year(df[i-1, "Year"] + 1)
      df[i, ] <- retirement.growth(
        df[i-1, "Age"],
        wage_growth,
        ci$emp_match,
        ci$emp_contrib,
        rate_of_return,
        ci$roth,
        ci$brokerage,
        ci$hsa,
        df[i-1, "Year"],
        df[i-1, "Bal_401k"],
        df[i-1, "Roth"],
        df[i-1, "Brokerage"],
        df[i-1, "HSA"],
        if (ci$working) df[i-1, "Salary"] else 0
      )
    }
  }
  
  df$Working <- df$Year <= last_work_year
  
  return(df)
}

#' household.drawdown()
#'
#' @description
#' Adds household withdrawal columns on top of the accumulation projection.
#' Balances are treated as base-year (real) dollars. From withdraw_start_year,
#' each year withdraws VPW rate x prior year-end net balance (optionally capped
#' at annual spend); the withdrawn dollars stop compounding at retire_return.
#' Anchor-year columns restate base-year dollars with inflation.
household.drawdown <- function(years, total_balance, any_working, all_working,
                               withdraw_start_year, vpw_rate, annual_spend, cap_withdrawals,
                               retire_return, fire_target, base_year, anchor_year, inflation) {
  vpw_rate <- vpw_rate / 100
  retire_return <- retire_return / 100
  anchor_factor <- (1 + inflation / 100)^(anchor_year - base_year)
  
  n <- length(years)
  withdrawal <- numeric(n)
  gap <- numeric(n)
  net <- total_balance
  
  for (t in seq_len(n)[-1]) {
    if (years[t] >= withdraw_start_year && net[t - 1] > 0) {
      wd <- vpw_rate * net[t - 1]
      if (isTRUE(cap_withdrawals)) wd <- min(wd, annual_spend)
      withdrawal[t] <- wd
    }
    gap[t] <- (gap[t - 1] + withdrawal[t]) * (1 + retire_return)
    net[t] <- max(total_balance[t] - gap[t], 0)
  }
  
  start_balance <- c(NA, head(net, -1))
  phase <- case_when(
    years >= withdraw_start_year ~ "Withdrawing",
    all_working ~ "Saving",
    any_working ~ "Transition",
    TRUE ~ "Coast"
  )
  vpw_capacity <- vpw_rate * net
  
  tibble(
    Phase = phase,
    Withdrawal = withdrawal,
    `Effective Rate` = ifelse(withdrawal > 0, withdrawal / start_balance, 0),
    `Net Balance` = net,
    `VPW Capacity` = vpw_capacity,
    Coverage = if (annual_spend > 0) vpw_capacity / annual_spend else NA_real_,
    `Hit Target` = ifelse(net >= fire_target, "YES", ""),
    `Total Balance (anchor$)` = total_balance * anchor_factor,
    `Net Balance (anchor$)` = net * anchor_factor,
    `Withdrawal (anchor$)` = withdrawal * anchor_factor
  )
}

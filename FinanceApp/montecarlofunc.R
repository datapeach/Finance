# Author: Rosie Schmitz

#' monte.carlo.vpw()
#'
#' @description
#' Variable percentage withdrawal (VPW) Monte Carlo in base-year (real) dollars.
#' Each year withdraws rate x current portfolio (optionally capped at spend_cap),
#' then applies a Normal(mean_return, vol) real return. All rates share the same
#' random return paths so scenarios are directly comparable.
#'
#' @return list(summary = metric x rate table, bands = percentile paths per rate)

# montecarlofunc.R

monte.carlo.vpw <- function(start_wealth, horizon, rates, floor, mean_return, vol, n_sims,
                            spend_cap = NA, start_year = 0, seed = 42) {
  set.seed(seed)
  returns <- matrix(rnorm(n_sims * horizon, mean_return / 100, vol / 100), nrow = n_sims)
  probs <- c(0.10, 0.50, 0.90)
  
  results <- lapply(rates, function(rate_pct) {
    rate <- rate_pct / 100
    wealth <- rep(start_wealth, n_sims)
    withdrawals <- matrix(0, nrow = n_sims, ncol = horizon)
    bands <- matrix(NA_real_, nrow = horizon + 1, ncol = length(probs))
    bands[1, ] <- start_wealth
    
    for (t in seq_len(horizon)) {
      wd <- rate * wealth
      if (!is.na(spend_cap)) wd <- pmin(wd, spend_cap)
      withdrawals[, t] <- wd
      wealth <- pmax((wealth - wd) * (1 + returns[, t]), 0)
      bands[t + 1, ] <- quantile(wealth, probs, names = FALSE)
    }
    
    meets_floor <- withdrawals >= floor - 0.005
    worst_year <- apply(withdrawals, 1, min)
    avg_spend <- rowMeans(withdrawals)
    
    list(
      metrics = c(
        `Year-1 withdrawal`                 = withdrawals[1, 1],
        `Portfolio survives horizon`        = mean(wealth > 0),
        `Never spend below lifestyle floor` = mean(rowSums(!meets_floor) == 0),
        `% of all years >= lifestyle floor` = mean(meets_floor),
        `Median worst-year spend`           = median(worst_year),
        `5th pct worst-year spend`          = quantile(worst_year, 0.05, names = FALSE),
        `10th pct worst-year spend`         = quantile(worst_year, 0.10, names = FALSE),
        `Median average annual spend`       = median(avg_spend),
        `10th pct average annual spend`     = quantile(avg_spend, 0.10, names = FALSE),
        `Median ending balance`             = median(wealth),
        `10th pct ending balance`           = quantile(wealth, 0.10, names = FALSE),
        `5th pct ending balance`            = quantile(wealth, 0.05, names = FALSE)
      ),
      bands = data.frame(
        Rate = rate_pct,
        Year = start_year + 0:horizon,
        P10 = bands[, 1],
        P50 = bands[, 2],
        P90 = bands[, 3]
      )
    )
  })
  
  summary <- data.frame(
    Metric = names(results[[1]]$metrics),
    sapply(results, `[[`, "metrics"),
    check.names = FALSE,
    row.names = NULL
  )
  names(summary)[-1] <- paste0(format(rates, nsmall = 1), "% VPW")
  
  list(
    summary = summary,
    bands = do.call(rbind, lapply(results, `[[`, "bands"))
  )
}

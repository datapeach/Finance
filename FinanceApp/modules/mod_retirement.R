# mod_retirement.R

# Author: Rosie Schmitz

# Retirement Analysis tab: per-person accumulation, household withdrawals,
# growth plot/table, and VPW Monte Carlo

# Per-person inputs; ids are prefixed with p1_ / p2_. max is a function of the person
# so HSA can use the individual (Person 1) or family (Person 2) limit.
person_fields <- tibble::tribble(
  ~id,                ~label,                                          ~min, ~max,                                   ~step,
  "age",              "Age:",                                          18,   function(p) 120,                        1,
  "salary",           "Salary ($):",                                   1000, function(p) 1000000,                    1,
  "wagegrowth",       "Wage Growth Rate (%):",                         0,    function(p) 100,                        0.5,
  "emp_match",        "Employer Matching (%):",                        0,    function(p) 100,                        0.5,
  "emp_contrib",      "401k Employee Contributions ($):",              0,    function(p) max_limits$lim401k50,       1,
  "ratereturn",       "Rate of Return (%):",                           0,    function(p) 1000,                       0.1,
  "roth",             "Annual Roth Contrib ($):",                      0,    function(p) max_limits$limrothira50,    0.1,
  "brokerage",        "Annual Taxable Brokerage Contrib ($):",         0,    function(p) 50000000,                   0.1,
  "hsa",              "Annual HSA Contrib ($):",                       0,    function(p) if (p == "p1") max_limits$limhsaone55 else max_limits$limhsatwo55, 0.1,
  "lastworkyear",     "Last Working Year (contributions stop after):", 2000, function(p) 2150,                       1,
  "current401k",      "Current 401k Amount ($):",                      0,    function(p) 10000000,                   0.1,
  "currentroth",      "Current Roth Amount ($):",                      0,    function(p) 10000000,                   0.1,
  "currentbrokerage", "Current Taxable Brokerage Amount ($):",         0,    function(p) 100000000,                  0.1,
  "currenthsa",       "Current HSA Amount ($):",                       0,    function(p) 10000000,                   0.1
)

person_inputs_ui <- function(ns, p, person_label) {
  lapply(seq_len(nrow(person_fields)), function(i) {
    f <- person_fields[i, ]
    label <- if (f$id %in% c("age", "salary")) paste(person_label, f$label) else f$label
    numericInput(
      inputId = ns(paste0(p, "_", f$id)),
      label = label,
      value = app_defaults[[p]][[f$id]],
      min = f$min,
      max = f$max[[1]](p),
      step = f$step
    )
  })
}

retirementUI <- function(id) {
  ns <- NS(id)
  plan <- app_defaults$plan
  mc <- app_defaults$monte_carlo
  two_person <- "input.household_count == 'Two Person'"
  
  tagList(
    h2('Retirement Analysis'),
    infoBox(
      title = "Welcome to the Finance App",
      value = "Choose the number of earners in the sidebar, then fill out each person's inputs and the Household Plan on the Inputs tab. Click Run Analysis on Growth Plot to update the plot and table, then use the Monte Carlo tab to stress-test withdrawals.",
      icon = icon('comment-dollar'),
      color = "green",
      fill = FALSE, 
      width = 12
    ),
    
    tabBox(
      title = "Retirement",
      width = 12,
      
      # Inputs ----
      tabPanel(
        title = strong('Inputs'),
        fluidRow(
          column(width = 6, class = "compact-labels",
                 infoBox("", "Person One", icon = icon("user"), width = 12, color = "light-blue"),
                 person_inputs_ui(ns, "p1", "Person 1")
          ),
          column(width = 6, class = "compact-labels",
                 conditionalPanel(
                   condition = two_person,
                   infoBox("", "Person Two", icon = icon("user-plus"), width = 12, color = "teal"),
                   person_inputs_ui(ns, "p2", "Person 2")
                 )
          )
        ),
        fluidRow(
          hr(),
          column(width = 12, em(limits_note)),
          column(width = 6, uiOutput(ns("limits_p1"))),
          column(width = 6, conditionalPanel(condition = two_person, uiOutput(ns("limits_p2"))))
        ),
        fluidRow(
          hr(),
          column(width = 12,
                 infoBox("", "Household Plan (balances are in base-year dollars; returns are real)",
                         icon = icon("house"), width = 12, color = "purple")
          ),
          column(width = 4,
                 numericInput(ns("base_year"), "Base Year (present $; projection starts here, opening balances are the year before):",
                              value = current_year, min = 2000, max = 2150, step = 1),
                 numericInput(ns("plan_through_age"), "Plan Through Age (Person 1):",
                              value = plan$plan_through_age, min = 19, max = 120, step = 1),
                 numericInput(ns("anchor_year"), "Future Value Year (e.g. 2040$):",
                              value = plan$anchor_year, min = 2000, max = 2150, step = 1),
                 numericInput(ns("inflation"), "Inflation (%) for future value columns:",
                              value = plan$inflation, min = 0, max = 20, step = 0.1)
          ),
          column(width = 4,
                 numericInput(ns("withdraw_start_year"), "First Withdrawal Year:",
                              value = plan$withdraw_start_year, min = 2000, max = 2150, step = 1),
                 numericInput(ns("vpw_rate"), "VPW Withdrawal Rate (% of portfolio):",
                              value = plan$vpw_rate, min = 0, max = 20, step = 0.1),
                 numericInput(ns("annual_spend"), "Annual Spend ($, base-year):",
                              value = plan$annual_spend, min = 0, max = 10000000, step = 1000),
                 radioButtons(ns("withdraw_rule"), "Withdrawal Rule:",
                              choices = c("VPW capped at annual spend" = "capped",
                                          "Full VPW (spend the whole %)" = "full"),
                              selected = plan$withdraw_rule)
          ),
          column(width = 4,
                 numericInput(ns("retire_return"), "Real Return During Withdrawals (%; follows Person 1's rate until changed):",
                              value = app_defaults$p1$ratereturn, min = -10, max = 20, step = 0.1),
                 numericInput(ns("fire_target"), "FIRE Target ($, base-year):",
                              value = plan$fire_target, min = 0, max = 100000000, step = 10000),
                 conditionalPanel(
                   condition = two_person,
                   checkboxInput(ns("spousal_roth"),
                                 "Retired spouse keeps Roth contributions while the other still works (spousal Roth)",
                                 value = plan$spousal_roth)
                 )
          )
        )
      ),
      
      # Growth Plot ----
      tabPanel(
        title = strong('Growth Plot'),
        fluidRow(
          column(width = 6,
                 radioButtons(ns("plot_dollars"), "Dollars shown:",
                              choices = c("Base year (present $)" = "base", "Future value year" = "anchor"),
                              selected = "base", inline = TRUE)
          ),
          column(width = 6, actionButton(ns("growthplotbtn"), "Run Analysis"))
        ),
        fluidRow(plotlyOutput(ns('retireGrowthPlot')))
      ),
      
      # Growth Table ----
      tabPanel(
        title = strong('Growth Table'),
        fluidRow(
          column(width = 12,
                 p("Shows the same run as the Growth Plot. Click Run Analysis on the Growth Plot tab after changing inputs."))
        ),
        fluidRow(DT::dataTableOutput(ns('retireGrowthTbl'), height = "50em")), 
        fluidRow(
          column(width = 12, offset = 2, downloadButton(ns("downloadbtn"), "Download Table"))
        )
      ),
      
      # Monte Carlo ----
      tabPanel(
        title = strong('Monte Carlo'),
        fluidRow(
          column(width = 12,
                 p("Variable percentage withdrawal (VPW) simulation in base-year dollars with real returns. ",
                   "Start wealth, withdrawal rule, and annual spend come from the last Run Analysis on the Growth Plot tab."),
                 textOutput(ns("mc_start_display"))
          )
        ),
        fluidRow(
          column(width = 4,
                 numericInput(ns("mc_start_override"), "Start Wealth Override ($, 0 = use projection):",
                              value = 0, min = 0, max = 1e9, step = 10000),
                 numericInput(ns("mc_horizon"), "Horizon (years):",
                              value = mc$horizon, min = 1, max = 100, step = 1),
                 textInput(ns("mc_rates"), "VPW Rates to Compare (%):", value = mc$rates)
          ),
          column(width = 4,
                 numericInput(ns("mc_floor"), "Lifestyle Floor ($, base-year):",
                              value = mc$floor, min = 0, max = 10000000, step = 1000),
                 numericInput(ns("mc_mean"), "Mean Real Return (%; follows Person 1's rate until changed):",
                              value = app_defaults$p1$ratereturn, min = -10, max = 20, step = 0.1),
                 numericInput(ns("mc_vol"), "Volatility (% std dev):",
                              value = mc$vol, min = 0, max = 60, step = 0.5)
          ),
          column(width = 4,
                 numericInput(ns("mc_sims"), "Simulations:",
                              value = mc$sims, min = 100, max = 100000, step = 1000),
                 numericInput(ns("mc_seed"), "Random Seed:", value = mc$seed, min = 1, step = 1),
                 actionButton(ns("mcbtn"), "Run Monte Carlo")
          )
        ),
        fluidRow(
          hr(),
          column(width = 12, DT::dataTableOutput(ns('mcSummaryTbl')))
        ),
        fluidRow(
          hr(),
          column(width = 4, uiOutput(ns("mc_band_rate_ui"))),
          column(width = 12, plotlyOutput(ns('mcBandPlot'), height = figure.height))
        )
      )
    )
  )
}

# household_count: reactive returning "One Person" or "Two Person" (sidebar input)
retirementServer <- function(id, household_count) {
  moduleServer(id, function(input, output, session) {
    
    person_values <- function(p) {
      values <- lapply(setNames(person_fields$id, person_fields$id),
                       function(f) input[[paste0(p, "_", f)]])
      validate(need(all(vapply(values, isTruthy, logical(1))),
                    paste0("Fill in every ", if (p == "p1") "Person 1" else "Person 2",
                           " field on the Inputs tab (use 0 for none).")))
      values
    }
    
    project_person <- function(v, projection, spouse_last_work_year = NA) {
      retirement.projection(projection, v$current401k, v$currentroth, v$currentbrokerage, v$currenthsa,
                            v$salary, v$wagegrowth, v$ratereturn,
                            v$age, v$emp_match, v$emp_contrib, v$roth, v$brokerage, v$hsa,
                            last_work_year = v$lastworkyear,
                            spouse_last_work_year = spouse_last_work_year,
                            spousal_roth = isTRUE(input$spousal_roth),
                            start_year = input$base_year)
    }
    
    # Values stay numeric (base-year dollars); formatting happens in the table/plot
    projected_data <- reactive({
      plan_ids <- c("plan_through_age", "withdraw_start_year", "base_year", "anchor_year",
                    "vpw_rate", "annual_spend", "retire_return", "inflation", "fire_target")
      validate(need(all(vapply(plan_ids, function(f) isTruthy(input[[f]]), logical(1))),
                    "Fill in every Household Plan field on the Inputs tab."))
      p1 <- person_values("p1")
      
      # Both people share one horizon: base year through the year Person 1 reaches plan_through_age
      projection <- input$plan_through_age - p1$age + 1
      validate(need(projection >= 2, "Plan Through Age must be at least one year above Person 1's age."))
      
      if (household_count() == "One Person") {
        df <- project_person(p1, projection)
        any_working <- df$Working
        all_working <- df$Working
        df <- df %>% select(-Working)
        total <- df$Balance
      } else {
        p2 <- person_values("p2")
        temp_df1 <- project_person(p1, projection, spouse_last_work_year = p2$lastworkyear)
        temp_df2 <- project_person(p2, projection, spouse_last_work_year = p1$lastworkyear)
        
        names(temp_df1)[-1] <- paste0("p1_", names(temp_df1)[-1])
        names(temp_df2)[-1] <- paste0("p2_", names(temp_df2)[-1])
        df <- full_join(temp_df1, temp_df2, by = "Year")
        df$`Total Balance` <- df$p1_Balance + df$p2_Balance
        
        any_working <- df$p1_Working | df$p2_Working
        all_working <- df$p1_Working & df$p2_Working
        df <- df %>% select(-p1_Working, -p2_Working)
        total <- df$`Total Balance`
      }
      
      bind_cols(
        df,
        household.drawdown(
          years = df$Year,
          total_balance = total,
          any_working = any_working,
          all_working = all_working,
          withdraw_start_year = input$withdraw_start_year,
          vpw_rate = input$vpw_rate,
          annual_spend = input$annual_spend,
          cap_withdrawals = input$withdraw_rule == "capped",
          retire_return = input$retire_return,
          fire_target = input$fire_target,
          base_year = input$base_year,
          anchor_year = input$anchor_year,
          inflation = input$inflation
        )
      )
    })
    
    # Withdrawal-phase and Monte Carlo returns copy Person 1's rate, unless the user
    # has changed them (i.e. they no longer equal the last value copied in)
    synced_return <- reactiveVal(NULL)
    observeEvent(input$p1_ratereturn, {
      rate <- input$p1_ratereturn
      req(!is.na(rate))
      previous <- synced_return()
      for (id in c("retire_return", "mc_mean")) {
        if (is.null(previous) || isTRUE(all.equal(input[[id]], previous))) {
          updateNumericInput(session, id, value = rate)
        }
      }
      synced_return(rate)
    })
    
    # Table/CSV version: dollar columns labelled with the year their dollars are in
    labelled_projection <- reactive({
      df <- projected_data()
      base_cols <- intersect(c("Balance", "p1_Balance", "p2_Balance", "Total Balance", "Withdrawal",
                               "Net Balance", "VPW Capacity"), names(df))
      names(df)[match(base_cols, names(df))] <- paste0(base_cols, " (", input$base_year, "$)")
      names(df) <- sub("(anchor$)", paste0("(", input$anchor_year, "$)"), names(df), fixed = TRUE)
      df
    })
    
    # Contribution limits ----
    limits_text <- function(p, person_label, hsa_type) {
      renderUI({
        l <- calculate_limits(input[[paste0(p, "_age")]])
        hsa <- if (hsa_type == "Individual") l$limhsaone55 else l$limhsatwo55
        tagList(
          div(paste0(person_label, " 401k Contribution Limit: ", dollar(l$lim401k50))),
          div(paste0(person_label, " Roth IRA Contribution Limit: ", dollar(l$limrothira50))),
          div(paste0(person_label, " HSA Contribution Limit (", hsa_type, "): ", dollar(hsa)))
        )
      })
    }
    output$limits_p1 <- limits_text("p1", "Person 1", "Individual")
    output$limits_p2 <- limits_text("p2", "Person 2", "Family")
    
    # Shared growth snapshot ----
    # Run Analysis refreshes one snapshot so the plot, table, CSV, and Monte Carlo
    # always show the same run; it also runs once at startup so the table isn't empty
    growth_snapshot <- reactiveVal(NULL)
    
    observeEvent(input$growthplotbtn, {
      growth_snapshot(list(
        data = projected_data(),
        labelled = labelled_projection(),
        household_count = household_count(),
        base_year = input$base_year,
        anchor_year = input$anchor_year,
        anchor_factor = (1 + input$inflation / 100)^(input$anchor_year - input$base_year),
        withdraw_start_year = input$withdraw_start_year,
        withdraw_rule = input$withdraw_rule,
        annual_spend = input$annual_spend,
        vpw_rate = input$vpw_rate
      ))
    }, ignoreNULL = FALSE)
    
    # Growth Plot ----
    output$retireGrowthPlot <- renderPlotly({
      snap <- req(growth_snapshot())
      
      use_anchor <- input$plot_dollars == "anchor"
      factor <- if (use_anchor) snap$anchor_factor else 1
      unit <- paste0(if (use_anchor) snap$anchor_year else snap$base_year, "$")
      
      series <- if (snap$household_count == "One Person") {
        c("Total" = "Balance", "Net of Withdrawals" = "Net Balance")
      } else {
        c("Person 1" = "p1_Balance", "Person 2" = "p2_Balance",
          "Total" = "Total Balance", "Net of Withdrawals" = "Net Balance")
      }
      
      plot_df <- snap$data %>%
        select(Year, all_of(series)) %>%
        pivot_longer(-Year, names_to = "Series", values_to = "Balance") %>%
        mutate(Balance = Balance * factor,
               Series = factor(Series, levels = names(series)))
      
      p <- ggplot(plot_df, aes(x = Year, y = Balance, color = Series, group = Series,
                               text = paste0("Year: ", Year, "<br>", Series, ": ", dollar(Balance)))) +
        geom_line() +
        geom_vline(xintercept = snap$withdraw_start_year, linetype = "dashed", color = "grey50") +
        scale_color_manual(values = c("Person 1" = "#5f939a", "Person 2" = "#e0a458",
                                      "Total" = "#c09bd5", "Net of Withdrawals" = "#6f42c1")) +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(title = paste0("Projected Balance (", unit, "; dashed line = first withdrawal year)"),
             x = "Year",
             y = paste0("Balance (", unit, ")"),
             color = "Household") +
        theme_minimal()
      
      ggplotly(p, tooltip = c("text"))
    })
    
    # Growth Table ----
    output$retireGrowthTbl <- DT::renderDataTable({
      snap <- req(growth_snapshot())
      df <- snap$labelled
      non_dollar <- c("Year", "Age", "p1_Age", "p2_Age", "Phase", "Effective Rate", "Coverage", "Hit Target")
      dollar_cols <- setdiff(names(df), non_dollar)
      total_col <- grep("^(Balance|Total Balance) \\(", names(df), value = TRUE)
      net_cols <- grep("^Net Balance", names(df), value = TRUE)
      person_cols <- grep("^p[12]_Balance", names(df), value = TRUE)
      
      tbl <- datatable(df,
                       fillContainer = TRUE, 
                       rownames = FALSE,
                       options = list(pageLength = 50, 
                                      autoWidth = FALSE, 
                                      scrollX = TRUE,
                                      searching = FALSE,
                                      lengthChange = FALSE)) %>%
        formatCurrency(dollar_cols, digits = 0) %>%
        formatPercentage("Effective Rate", digits = 2) %>%
        formatRound("Coverage", digits = 2) %>%
        formatStyle(total_col, backgroundColor = "lightyellow") %>%
        formatStyle(net_cols, backgroundColor = "#e6dcf5") %>%
        formatStyle("Phase", backgroundColor = styleEqual(
          c("Saving", "Transition", "Coast", "Withdrawing"),
          c("white", "#fff2cc", "#fce4d6", "#e2efda")))
      
      if (length(person_cols) > 0) {
        tbl <- tbl %>% formatStyle(person_cols, backgroundColor = "lightblue")
      }
      
      tbl %>% formatStyle(columns = names(df), `font-size` = '12px')
    })
    
    output$downloadbtn <- downloadHandler(
      filename = function() {
        paste0("RetirementProjections-", Sys.Date(), ".csv")
      },
      content = function(file) {
        snap <- growth_snapshot()
        write.csv(if (is.null(snap)) labelled_projection() else snap$labelled, file, row.names = FALSE)
      }
    )
    
    # Monte Carlo ----
    # Starts from the projected net balance at the end of the year before withdrawals begin
    mc_start <- reactive({
      snap <- req(growth_snapshot())
      df <- snap$data
      row <- df[which.min(abs(df$Year - (snap$withdraw_start_year - 1))), ]
      list(year = row$Year, wealth = row$`Net Balance`, snap = snap)
    })
    
    output$mc_start_display <- renderText({
      s <- mc_start()
      paste0("Start wealth from the last Run Analysis: ", dollar(s$wealth, accuracy = 1),
             " at end of ", s$year, " (", s$snap$base_year, "$). First simulated withdrawal year: ",
             s$year + 1, ".")
    })
    
    mc_results <- eventReactive(input$mcbtn, {
      snap <- mc_start()$snap
      rates <- suppressWarnings(as.numeric(strsplit(input$mc_rates, ",")[[1]]))
      rates <- sort(unique(c(rates[!is.na(rates)], snap$vpw_rate)))
      validate(
        need(length(rates) > 0, "Enter at least one VPW rate."),
        need(isTruthy(input$mc_horizon) && isTruthy(input$mc_sims), "Enter a horizon and number of simulations.")
      )
      
      start <- if (isTruthy(input$mc_start_override) && input$mc_start_override > 0) {
        input$mc_start_override
      } else {
        mc_start()$wealth
      }
      
      withProgress(message = "Running Monte Carlo...", {
        result <- monte.carlo.vpw(
          start_wealth = start,
          horizon = input$mc_horizon,
          rates = rates,
          floor = input$mc_floor,
          mean_return = input$mc_mean,
          vol = input$mc_vol,
          n_sims = input$mc_sims,
          spend_cap = if (snap$withdraw_rule == "capped") snap$annual_spend else NA,
          start_year = mc_start()$year,
          seed = input$mc_seed
        )
      })
      c(result, list(base_year = snap$base_year, vpw_rate = snap$vpw_rate))
    })
    
    output$mcSummaryTbl <- DT::renderDataTable({
      summary <- mc_results()$summary
      pct_rows <- c("Portfolio survives horizon", "Never spend below lifestyle floor",
                    "% of all years >= lifestyle floor")
      shown <- summary
      for (col in names(shown)[-1]) {
        shown[[col]] <- ifelse(summary$Metric %in% pct_rows,
                               percent(summary[[col]], accuracy = 0.1),
                               dollar(summary[[col]], accuracy = 1))
      }
      datatable(shown,
                rownames = FALSE,
                options = list(dom = 't', pageLength = nrow(shown), ordering = FALSE, scrollX = TRUE))
    })
    
    output$mc_band_rate_ui <- renderUI({
      res <- mc_results()
      rates <- unique(res$bands$Rate)
      selectInput(session$ns("mc_band_rate"), "Show percentile bands for VPW rate (%):",
                  choices = rates,
                  selected = if (res$vpw_rate %in% rates) res$vpw_rate else rates[1])
    })
    
    output$mcBandPlot <- renderPlotly({
      req(input$mc_band_rate)
      res <- mc_results()
      bands <- res$bands %>% filter(Rate == as.numeric(input$mc_band_rate))
      
      # Ribbon is fill only; separate lines carry the hover values (a ribbon only labels its top edge)
      plot_ly(bands, x = ~Year) %>%
        add_ribbons(ymin = ~P10, ymax = ~P90, name = "10th-90th percentile range",
                    fillcolor = "rgba(192,155,213,0.35)", line = list(color = "transparent"),
                    hoverinfo = "skip") %>%
        add_lines(y = ~P90, name = "90th percentile", line = list(color = "#c09bd5", width = 1, dash = "dot"),
                  hovertemplate = "%{y:$,.0f}") %>%
        add_lines(y = ~P50, name = "Median", line = list(color = "#6f42c1"),
                  hovertemplate = "%{y:$,.0f}") %>%
        add_lines(y = ~P10, name = "10th percentile", line = list(color = "#c09bd5", width = 1, dash = "dot"),
                  hovertemplate = "%{y:$,.0f}") %>%
        layout(title = paste0("Simulated Portfolio at ", input$mc_band_rate, "% VPW (", res$base_year, "$)"),
               yaxis = list(title = paste0("Portfolio (", res$base_year, "$)"), tickformat = "$,.0f"),
               xaxis = list(title = "Year"),
               hovermode = "x unified")
    })
  })
}

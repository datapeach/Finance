# server.R

# Author: Rosie Schmitz

# Preamble ----
source("global.R") # Load global variables
source("budgetfunc.R") # Compute budget outputs for pie chart and table
source("investfunc.R") # Compute investment outputs for DT tbl
source("retirementgrowthfunc.R") # Compute the retirement growth for the table and plot
source("montecarlofunc.R") # VPW Monte Carlo simulations

# Server function ----
function(input, output, session) {
  
  # Reactivity ----
  rv <- reactiveValues(
    # budget 
    take_home = 0,
    rent_mort = 0,
    car_payment = 0,
    groceries = 0, 
    home_ins = 0,
    home_maint = 0,
    car_ins = 0,
    car_maint = 0,
    utilities = 0,
    subscriptions = 0,
    dining_out = 0,
    hobbies = 0,
    savings = 0,
    investing = 0,
    # investment
    money_to_invest = 0,
    num_investments = 0,
    # retirement
    p1_age = 0,
    p2_age = 0,
    p1_wagegrowth = 0,
    p1_emp_contrib = 0,
    p1_ratereturn = 0,
    p1_roth = 0,
    p1_brokerage = 0,
    p1_hsa = 0,
    p1_current401k = 0,
    p1_currentroth = 0,
    p1_currentbrokerage = 0,
    p1_currenthsa = 0,
    p1_salary = 0,
    p2_wagegrowth = 0,
    p2_emp_contrib = 0,
    p2_ratereturn = 0,
    p2_roth = 0,
    p2_brokerage = 0,
    p2_hsa = 0,
    p2_current401k = 0,
    p2_currentroth = 0,
    p2_currentbrokerage = 0,
    p2_currenthsa = 0,
    p2_salary = 0,
    household_count = "One Person",
  )
  observe({
    # budget
    rv$take_home     <- input$take_home
    rv$rent_mort     <- input$rent_mort
    rv$car_payment   <- input$car_payment
    rv$groceries     <- input$groceries
    rv$home_ins      <- input$home_ins
    rv$home_maint    <- input$home_maint
    rv$car_ins       <- input$car_ins
    rv$car_maint     <- input$car_maint
    rv$utilities     <- input$utilities
    rv$subscriptions <- input$subscriptions
    rv$dining_out    <- input$dining_out
    rv$hobbies       <- input$hobbies
    rv$savings       <- input$savings
    rv$investing     <- input$investing
  })
  observe({
    # investment
    rv$p1_age              <- input$p1_age
    rv$p2_age              <- input$p2_age
    rv$p1_wagegrowth       <- input$p1_wagegrowth
    rv$p1_emp_match        <- input$p1_emp_match
    rv$p1_emp_contrib      <- input$p1_emp_contrib
    rv$p1_ratereturn       <- input$p1_ratereturn 
    rv$p1_roth             <- input$p1_roth
    rv$p1_brokerage        <- input$p1_brokerage
    rv$p1_hsa              <- input$p1_hsa
    rv$p1_current401k      <- input$p1_current401k
    rv$p1_currentroth      <- input$p1_currentroth
    rv$p1_currentbrokerage <- input$p1_currentbrokerage
    rv$p1_currenthsa       <- input$p1_currenthsa
    rv$p1_salary           <- input$p1_salary
    rv$p2_wagegrowth       <- input$p2_wagegrowth
    rv$p2_emp_match        <- input$p2_emp_match
    rv$p2_emp_contrib      <- input$p2_emp_contrib
    rv$p2_ratereturn       <- input$p2_ratereturn
    rv$p2_roth             <- input$p2_roth
    rv$p2_brokerage        <- input$p2_brokerage
    rv$p2_hsa              <- input$p2_hsa
    rv$p2_current401k      <- input$p2_current401k
    rv$p2_currentroth      <- input$p2_currentroth
    rv$p2_currentbrokerage <- input$p2_currentbrokerage
    rv$p2_currenthsa       <- input$p2_currenthsa
    rv$p2_salary           <- input$p2_salary
    rv$household_count     <- input$household_count
  })
  
  
  # Reactive expression to create the dataframe based on reactive values for retirement growth
  # Values stay numeric (base-year dollars); formatting happens in the table/plot
  projected_data <- reactive({
    req(rv$p1_age, input$plan_through_age, input$p1_lastworkyear, input$withdraw_start_year, input$base_year,
        input$anchor_year, !is.na(input$vpw_rate), !is.na(input$annual_spend),
        !is.na(input$retire_return), !is.na(input$inflation), !is.na(input$fire_target))
    
    # Both people share one horizon: base year through the year Person 1 reaches plan_through_age
    projection <- input$plan_through_age - rv$p1_age + 1
    validate(need(projection >= 2, "Plan Through Age must be at least one year above Person 1's age."))
    
    if(input$household_count == "One Person") {
      
      df <- retirement.projection(projection, rv$p1_current401k,  rv$p1_currentroth,rv$p1_currentbrokerage, rv$p1_currenthsa,
                                  rv$p1_salary, rv$p1_wagegrowth, rv$p1_ratereturn,
                                  rv$p1_age, rv$p1_emp_match, rv$p1_emp_contrib, rv$p1_roth, rv$p1_brokerage, rv$p1_hsa,
                                  last_work_year = input$p1_lastworkyear,
                                  start_year = input$base_year)
      
      any_working <- df$Working
      all_working <- df$Working
      df <- df %>% select(-Working)
      total <- df$Balance
      
    } else {
      req(rv$p2_age, input$p2_lastworkyear)
      temp_df1 <- retirement.projection(projection, rv$p1_current401k,  rv$p1_currentroth, rv$p1_currentbrokerage, rv$p1_currenthsa,
                                        rv$p1_salary, rv$p1_wagegrowth, rv$p1_ratereturn,
                                        rv$p1_age, rv$p1_emp_match, rv$p1_emp_contrib, rv$p1_roth, rv$p1_brokerage, rv$p1_hsa,
                                        last_work_year = input$p1_lastworkyear,
                                        spouse_last_work_year = input$p2_lastworkyear,
                                        spousal_roth = input$spousal_roth,
                                        start_year = input$base_year)
      
      temp_df2 <- retirement.projection(projection, rv$p2_current401k,  rv$p2_currentroth, rv$p2_currentbrokerage, rv$p2_currenthsa,
                                        rv$p2_salary, rv$p2_wagegrowth, rv$p2_ratereturn,
                                        rv$p2_age, rv$p2_emp_match, rv$p2_emp_contrib, rv$p2_roth, rv$p2_brokerage, rv$p2_hsa,
                                        last_work_year = input$p2_lastworkyear,
                                        spouse_last_work_year = input$p1_lastworkyear,
                                        spousal_roth = input$spousal_roth,
                                        start_year = input$base_year)
      
      names(temp_df1)[-1] <- paste("p1_", names(temp_df1)[-1], sep ="")
      names(temp_df2)[-1] <- paste("p2_", names(temp_df2)[-1], sep ="")
      df <- full_join(temp_df1, temp_df2, by = "Year")
      
      df$`Total Balance` <- df$p1_Balance + df$p2_Balance
      
      p1_working <- df$p1_Working
      p2_working <- df$p2_Working
      any_working <- p1_working | p2_working
      all_working <- p1_working & p2_working
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
  
  anchor_factor <- reactive({
    (1 + input$inflation / 100)^(input$anchor_year - input$base_year)
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
  
  # Calculating retirement limits based on age 
  limits <- reactive({
    list(
      p1_limits = calculate_limits(rv$p1_age),
      p2_limits = calculate_limits(rv$p2_age)
    )
  })
  
  
  
  # Retirement Input Page ----
  output$limit_401k_one_display <- renderText({
    if(input$household_count %in% c("One Person", "Two Person")) {
      paste("Person 1 401k Contribution Limit: $", limits()$p1_limits$lim401k50)
    } else {
      input$household_count
    }
    
  })
  
  output$limit_401k_two_display <- renderText({
    ifelse(input$household_count == "One Person", "",paste("Person 2 401k Contribution Limit: $", limits()$p2_limits$lim401k50))
  })
  
  output$limit_roth_ira_one_catchup_display <- renderText({
    if(input$household_count %in% c("One Person", "Two Person")) {
      paste("Person 1 Roth IRA Contribution Limit: $", limits()$p1_limits$limrothira50)
    } else {
      ""
    }
    
  })
  
  output$limit_roth_ira_two_catchup_display <- renderText({
    ifelse(input$household_count == "One Person", "", paste("Person 2 Roth IRA Contribution Limit: $", limits()$p2_limits$limrothira50))
  })
  
  
  output$limit_hsa_one_55_display <- renderText({
    if (input$household_count %in% c("One Person", "Two Person")) {
      paste("Person 1 HSA Contribution Limit (Individual): $", limits()$p1_limits$limhsaone55)
    } else {
      ""
    }
  })
  
  output$limit_hsa_two_55_display <- renderText({
    ifelse(input$household_count == "One Person", "", paste("Person 2 HSA Contribution Limit (Family): $", limits()$p2_limits$limhsatwo55))
  })
  
  
  # Reactive expression to create dataframe based on reactive values for budget
  # budget_data <- reactive({
  #   df <- budget.output(rv$take_home,rv$rent_mort,rv$car_payment, rv$groceries, rv$home_ins,
  #                       rv$home_maint, rv$car_ins, rv$car_maint, rv$utilities, rv$pets, rv$subscriptions, 
  #                       rv$dining_out, rv$hobbies, rv$savings,  rv$investing)
  # })  
  
  budget_data <- reactive({
    validate(need(isTruthy(input$take_home) && input$take_home > 0,
                  "Enter a Total Take Home amount above $0 on the Inputs tab."))
    budget.output(
      take_home     = input$take_home,
      rent_mort     = input$rent_mort,
      car_payment   = input$car_payment,
      groceries     = input$groceries,
      home_ins      = input$home_ins,
      home_maint    = input$home_maint,
      car_ins       = input$car_ins,
      car_maint     = input$car_maint,
      utilities     = input$utilities,
      phones        = input$phones,
      pets          = input$pets,
      subscriptions = input$subscriptions,
      dining_out    = input$dining_out,
      hobbies       = input$hobbies,
      savings       = input$savings,
      investing     = input$investing
    )
  })
  
  # Budget outputs ----
  # Reactive expression for pie chart data
  # Pie charts can't draw negative or zero slices (e.g. Unallocated when overspending);
  # overspending is reported in the note instead
  reactivePieData <- eventReactive(input$budgetbtn, {
    list(
      pieChart1 = budget_data() %>%
        select(c('Needs', 'Wants', 'Savings_Investing', 'Unallocated')) %>%
        pivot_longer(everything(), 
                     cols_vary = "slowest", 
                     names_to = "Type") %>%
        filter(value > 0),
      pieChart2 = budget_data() %>%
        select(-c('Needs %', 'Wants %', 'Savings/Investing %', 'Needs', 'Wants', 'Savings_Investing', 'Take_Home', 'Unallocated %')) %>%
        pivot_longer(everything(), 
                     cols_vary = "slowest", 
                     names_to = "Type") %>%
        filter(value > 0)
    )
  })
  
  # Render the first pie chart
  output$budgetPieChart <- renderPlotly({
    data <- reactivePieData()$pieChart1
    
    colors <- c('#fc8d62', '#8da0cb', '#66c2a5')
    
    fig <- plot_ly(data, labels = ~Type,
                   values = ~value,
                   type = 'pie',
                   hoverinfo = 'label+text',
                   insidetextfont = list(color = '#FFFFFF'),
                   text = ~dollar(value),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   showlegend = TRUE) %>%
      layout(title = 'Monthly Budget Expenditures',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             margin = list(l = 40, r = 40, b = 100, t = 100),
             height = 500)
    fig
  })
  
  # Render the second pie chart
  output$budgetPieChart2 <- renderPlotly({
    data <- reactivePieData()$pieChart2
    
    fig <- plot_ly(data, labels = ~Type,
                   values = ~value,
                   type = 'pie',
                   hoverinfo = 'label+text',
                   insidetextfont = list(color = '#FFFFFF', size = 9),
                   text = ~dollar(value),
                   showlegend = TRUE) %>%
      layout(title = 'Detailed Monthly Budget Expenditures',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             margin = list(l = 40, r = 40, b = 100, t = 100),
             height = 500)   
    fig
  })
  
  # Reactive expression for data table
  reactiveTableData <- eventReactive(input$budgetbtn, {
    budget_data() %>%
      select(c('Needs %', 'Wants %', 'Savings/Investing %'))
  })
  
  # Render the data table
  output$budgetTbl <- DT::renderDataTable({
    selected_data <- reactiveTableData()
    
    datatable(selected_data,
              rownames = FALSE,
              fillContainer = TRUE,
              options = list(
                dom = 't',
                ordering = FALSE,  # Disable sorting
                rowCallback = JS(
                  "function(row, data, index) {",
                  "$('td', row).css('height', '50px');",  # Custom row height
                  "}"
                ),
                columnDefs = list(
                  list(
                    className = 'dt-column-separator',
                    targets = '_all'  # Apply the class to all columns
                  ),
                  list(
                    targets = 0,  # Index of the first column
                    width = '150px'  # Width for the first column
                  ),
                  list(
                    targets = 1,  # Index of the second column
                    width = '150px'  # Width for the second column
                  ),
                  list(
                    targets = 2,  # Index of the third column
                    width = '150px'  # Width for the third column
                  )
                  # Add more targets and widths if you have more columns
                )
              )
    ) %>%
      formatStyle(
        'Needs %',
        backgroundColor = styleInterval(50, c('#91cf60', '#fc8d59'))
      ) %>%
      formatStyle(
        'Wants %',
        backgroundColor = styleInterval(30, c('#91cf60', '#fc8d59'))
      ) %>%
      formatStyle(
        'Savings/Investing %',
        backgroundColor = styleInterval(20, c('#fc8d59', '#91cf60'))
      )
  })
  
  
  
  # Budget Note Output
  
  unallocated <- reactive({
    budget_data()$Unallocated
  })
  # Reactive expression to create the note output
  reactiveNoteOutput <- eventReactive(input$budgetbtn, {
    unallocated_value <- unallocated()
    
    # Create the general 50/30/20 principle note
    principle_note <- div(
      "This is following the 50/30/20 principle. It is advised to have 'Needs' less than or equal to 50%, 'Wants' less than or equal to 30%, and 'Savings/Investing' greater than or equal to 20%.", 
      style = "font-size:14px; color: gray; padding: 10px;"
    )
    
    # Generate conditional note based on unallocated value
    conditional_note <- if (unallocated_value > 0) {
      div(
        sprintf("You have $%0.2f surplus unallocated.", unallocated_value),
        style = "font-size:14px; color: green; padding: 10px;"
      )
    } else if (unallocated_value < 0) {
      div(
        sprintf("You are overspending by $%0.2f.", abs(unallocated_value)),
        style = "font-size:14px; color: red; padding: 10px;"
      )
    } else {
      div(
        "Your budget is perfectly allocated with no surplus or deficit.",
        style = "font-size:14px; color: gray; padding: 10px;"
      )
    }
    
    # Combine both notes into one output
    tagList(principle_note, conditional_note)
  })
  # Render the note output
  output$noteOutput <- renderUI({
    reactiveNoteOutput()
  })
  
  # Investment Editable DT ----
  
  # Reactive values to hold the investment table
  investments <- reactiveVal(data.frame(
    `Ticker(s)` = character(0),
    `Current Shares` = numeric(0),
    `Current Dollars` = numeric(0),
    `Desired %` = numeric(0),
    `Shares Needed` = numeric(0),
    `Dollars Needed` = numeric(0),
    stringsAsFactors = FALSE
  ))
  
  # Update table when num_investments changes, keeping rows already filled in
  observeEvent(input$num_investments, {
    req(input$money_to_invest)  # To require money to invest to be not NA
    
    num <- as.numeric(input$num_investments)
    existing <- investments()
    kept <- head(existing, num)
    new_rows <- seq_len(num - nrow(kept)) + nrow(kept)
    if (length(new_rows) == 0) {
      investments(kept)
      return()
    }
    
    temp_tbl <- data.frame(
      `Ticker(s)` = paste0("Ticker ", new_rows),  # Example placeholder names
      `Current Shares` = rep(0, length(new_rows)),
      `Current Dollars` = rep(0, length(new_rows)),
      `Desired %` = rep(0, length(new_rows)),
      `Shares Needed` = rep(0, length(new_rows)),
      `Dollars Needed` = rep(0, length(new_rows)),
      stringsAsFactors = FALSE
    )
    names(temp_tbl) <- names(existing)
    
    investments(rbind(kept, temp_tbl))
  })
  
  # Capture edits made to table (DT sends edited values as text, so convert back to the column's type)
  observeEvent(input$investTbl_cell_edit, {
    info <- input$investTbl_cell_edit
    updated_data <- investments()
    updated_data[info$row, info$col] <- DT::coerceValue(info$value, updated_data[info$row, info$col])
    investments(updated_data)
  })
  
  # Render the editable data table
  output$investTbl <- DT::renderDataTable({
    # Rename columns before rendering the table
    renamed_data <- investments()
    colnames(renamed_data) <- c("Ticker", "Current Shares", "Current Dollars", "Desired %", "Shares Needed", "Dollars Needed")
    
    datatable(
      renamed_data, 
      editable = list(target = "cell", disable = list(columns = c(0,3,5,6))),
      options = list(
        searching = FALSE,  # Disable the search box
        lengthChange = FALSE,  # Hide the "Show entries" dropdown
        striped = FALSE,  # Disable alternating row colors
        ordering = FALSE, # Disable sort
        paging = FALSE, # Disable page num
        info = FALSE # Disable page text info
      )
    ) %>%
      formatStyle(
        c("Ticker", "Current Shares", "Desired %"),  # Columns to style
        backgroundColor = "#ffcccb",  # Apply red background color
        color = "black"  # Optional: change text color to white for contrast
      ) %>%
      formatRound("Shares Needed", digits = 3) %>%
      formatCurrency("Current Dollars", currency = "$") %>%
      formatCurrency("Dollars Needed", currency = "$") %>%
      formatStyle(
        'Shares Needed',
        color = styleInterval(0, c('#ff4d4d', '#91cf60'))
      ) %>%
      formatStyle(
        'Dollars Needed',
        color = styleInterval(0, c('#ff4d4d', '#91cf60'))
      )
  })
  
  # Run analysis button
  observeEvent(input$investbtn, {
    temp_tbl = investments()
    tickers <- trimws(temp_tbl[[1]])
    
    problem <- if (!isTruthy(input$money_to_invest) || input$money_to_invest < 0) {
      "Enter a Money To Invest amount of $0 or more."
    } else if (any(tickers == "" | grepl("^Ticker [0-9]+$", tickers))) {
      "Replace the placeholder tickers with real ticker symbols."
    } else if (anyDuplicated(toupper(tickers))) {
      "Each ticker can only appear once."
    } else if (any(is.na(temp_tbl[[2]])) || any(temp_tbl[[2]] < 0)) {
      "Current Shares must be numbers of 0 or more."
    } else if (any(is.na(temp_tbl[[4]])) || abs(sum(temp_tbl[[4]]) - 100) > 0.01) {
      paste0("Desired % must add up to 100 (currently ", round(sum(temp_tbl[[4]], na.rm = TRUE), 2), ").")
    }
    if (!is.null(problem)) {
      showNotification(problem, type = "error", duration = 8)
      return()
    }
    
    shinyjs::show("loading_spinner")
    on.exit(shinyjs::hide("loading_spinner"))
    
    output_tbl <- tryCatch({
      #create a for loop going through each of my 'investments' in temp tbl. as it goes through, assign it as new("Investment")
      temp_port <- new("Portfolio") 
      list_desire <-list()
      
      for(i in 1:nrow(temp_tbl)){
        temp_investment <- new("Investment")
        temp_investment <- setTicker(temp_investment, tickers[i])
        temp_investment <- setShare(temp_investment, as.numeric(temp_tbl[i,2]))
        temp_investment <- updateSharePrice(temp_investment)
        if (length(temp_investment@share_price) != 1 || is.na(temp_investment@share_price)) {
          stop("No price found for ticker '", tickers[i], "'.")
        }
        temp_investment <- updateTotalValue(temp_investment)
        
        temp_port <- addInvestToPortfolio(temp_port, temp_investment)
        list_desire <-append(list_desire, as.numeric(temp_tbl[i,4]))
      }
      temp_port <- updatePortfolio(temp_port)
      temp_pm <- new("PortfolioManager")
      temp_pm@portfolio = temp_port
      temp_pm <- updateInvestPerc(temp_pm)
      
      temp_pm@money_to_invest <- input$money_to_invest
      temp_pm = setDesiredInvestPerc(temp_pm, list_desire)
      temp_pm = updateFutureNeeds(temp_pm)
      
      returnPortfolioManagerTable(temp_pm)
    }, error = function(e) {
      showNotification(paste("Analysis failed:", conditionMessage(e)), type = "error", duration = 10)
      NULL
    })
    
    if (!is.null(output_tbl)) {
      names(output_tbl) <- names(temp_tbl)
      investments(output_tbl)
    }
  })
  
  
  
  # Shared growth snapshot ----
  # Run Analysis refreshes one snapshot so the plot and table always show the same run;
  # it also runs once at startup so the table isn't empty
  growth_snapshot <- reactiveVal(NULL)
  
  observeEvent(input$growthplotbtn, {
    growth_snapshot(list(
      data = projected_data(),
      labelled = labelled_projection(),
      household_count = input$household_count,
      base_year = input$base_year,
      anchor_year = input$anchor_year,
      anchor_factor = anchor_factor(),
      withdraw_start_year = input$withdraw_start_year,
      withdraw_rule = input$withdraw_rule,
      annual_spend = input$annual_spend,
      vpw_rate = input$vpw_rate
    ))
  }, ignoreNULL = FALSE)
  
  # Retirement Growth Plot ----
  # Reactive expression for retirement growth plot data
  reactiveGrowthPlotData <- reactive({
    snap <- req(growth_snapshot())
    data <- snap$data
    
    use_anchor <- input$plot_dollars == "anchor"
    factor <- if (use_anchor) snap$anchor_factor else 1
    unit <- paste0(if (use_anchor) snap$anchor_year else snap$base_year, "$")
    
    series <- if (snap$household_count == "One Person") {
      c("Total" = "Balance", "Net of Withdrawals" = "Net Balance")
    } else {
      c("Person 1" = "p1_Balance", "Person 2" = "p2_Balance",
        "Total" = "Total Balance", "Net of Withdrawals" = "Net Balance")
    }
    
    plot_df <- data %>%
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
  
  # Render the retirement growth plot
  output$retireGrowthPlot <- renderPlotly({
    reactiveGrowthPlotData()
  })
  
  # Reactive expression for retirement growth table data
  reactiveGrowthTableData <- reactive({
    snap <- req(growth_snapshot())
    df <- snap$labelled
    non_dollar <- c("Year", "Age", "p1_Age", "p2_Age", "Phase", "Effective Rate", "Coverage", "Hit Target")
    dollar_cols <- setdiff(names(df), non_dollar)
    total_col <- grep("^(Balance|Total Balance) \\(", names(df), value = TRUE)
    net_cols <- grep("^Net Balance", names(df), value = TRUE)
    
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
    
    if (snap$household_count == "Two Person") {
      tbl <- tbl %>% formatStyle(c(paste0("p1_Balance (", snap$base_year, "$)"),
                                   paste0("p2_Balance (", snap$base_year, "$)")),
                                 backgroundColor = "lightblue")
    }
    
    tbl %>% formatStyle(columns = names(df), `font-size` = '12px')
  })
  
  # Render the retirement growth table
  output$retireGrowthTbl <- DT::renderDataTable({
    reactiveGrowthTableData()
  })
  
  output$downloadbtn <- downloadHandler(
    filename = function() {
      paste("RetirementProjections-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      snap <- growth_snapshot()
      write.csv(if (is.null(snap)) labelled_projection() else snap$labelled, file, row.names = FALSE)
    }
  )
  
  # Monte Carlo ----
  # Uses the same Run Analysis snapshot as the growth plot and table; starts from the
  # projected net balance at the end of the year before withdrawals begin
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
    selectInput("mc_band_rate", "Show percentile bands for VPW rate (%):",
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
} #End of server

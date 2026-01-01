# server.R

# Author: Rosie Schmitz

# Preamble ----
source("global.R") # Load global variables
source("budgetfunc.R") # Compute budget outputs for pie chart and table
source("investfunc.R") # Compute investment outputs for DT tbl
source("retirementgrowthfunc.R") # Compute the retirement growth for the table and plot

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
    p1_projections = 0,
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
    p2_projections = 0,
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
    rv$home_maint    <- input$car_maint
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
    rv$p1_projections      <- input$p1_projections
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
    rv$p2_projections      <- input$p2_projections
    rv$p2_current401k      <- input$p2_current401k
    rv$p2_currentroth      <- input$p2_currentroth
    rv$p2_currentbrokerage <- input$p2_currentbrokerage
    rv$p2_currenthsa       <- input$p2_currenthsa
    rv$p2_salary           <- input$p2_salary
    rv$household_count     <- input$household_count
  })
  
  
  # Reactive expression to create the dataframe based on reactive values for retirement growth
  projected_data <- reactive({
    
    
    if(input$household_count == "One Person") {
      
      df <- retirement.projection(rv$p1_projections, rv$p1_current401k,  rv$p1_currentroth,rv$p1_currentbrokerage, rv$p1_currenthsa,
                                  rv$p1_salary, rv$p1_wagegrowth, rv$p1_ratereturn,
                                  rv$p1_age, rv$p1_emp_match, rv$p1_emp_contrib, rv$p1_roth, rv$p1_brokerage, rv$p1_hsa)
      
      df[,3:10] <- lapply(df[,3:10], dollar,accuracy = 0.01)
      
      df
      
    } else {
      temp_df1 <- retirement.projection(rv$p1_projections, rv$p1_current401k,  rv$p1_currentroth, rv$p1_currentbrokerage, rv$p1_currenthsa,
                                        rv$p1_salary, rv$p1_wagegrowth, rv$p1_ratereturn,
                                        rv$p1_age, rv$p1_emp_match, rv$p1_emp_contrib, rv$p1_roth, rv$p1_brokerage, rv$p1_hsa)
      
      temp_df2 <- retirement.projection(rv$p2_projections, rv$p2_current401k,  rv$p2_currentroth, rv$p2_currentbrokerage, rv$p2_currenthsa,
                                        rv$p2_salary, rv$p2_wagegrowth, rv$p2_ratereturn,
                                        rv$p2_age, rv$p2_emp_match, rv$p2_emp_contrib, rv$p2_roth, rv$p2_brokerage, rv$p2_hsa)
      
      names(temp_df1)[2:10] <- paste("p1_", names(temp_df1)[2:10], sep ="")
      names(temp_df2)[2:10] <- paste("p2_", names(temp_df2)[2:10], sep ="")
      df <- full_join(temp_df1, temp_df2, by = "Year") #need for taking larger of the two projected years in df (ie someone 24 years and someone 25)
      
      # case for nrows(p1) and p2 not equal length
      for(i in 2:nrow(df)) {
        df[i, "p1_Balance"] <-if_else(is.na(df[i,"p1_Balance"]), df[i-1, "p1_Balance"], df[i, "p1_Balance"]) 
        df[i, "p2_Balance"] <-if_else(is.na(df[i,"p2_Balance"]), df[i-1, "p2_Balance"], df[i, "p2_Balance"]) 
      }
      
      df$`Total Balance` <- df$p1_Balance + df$p2_Balance
      
      df[,c(3:10, 12:20)] <- lapply(df[,c(3:10, 12:20)], dollar,accuracy = 0.01)
      
      df
      
    }
    
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
  reactivePieData <- eventReactive(input$budgetbtn, {
    list(
      pieChart1 = budget_data() %>%
        select(c('Needs', 'Wants', 'Savings_Investing', 'Unallocated')) %>%
        pivot_longer(everything(), 
                     cols_vary = "slowest", 
                     names_to = "Type"),
      pieChart2 = budget_data() %>%
        select(-c('Needs %', 'Wants %', 'Savings/Investing %', 'Needs', 'Wants', 'Savings_Investing', 'Take_Home', 'Unallocated %')) %>%
        pivot_longer(everything(), 
                     cols_vary = "slowest", 
                     names_to = "Type")
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
                   text = ~paste('$', value),
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
                   text = ~paste('$', value),
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
  
  # Update table when num_investments changes
  observeEvent(input$num_investments, {
    req(input$money_to_invest)  # To require money to invest to be not NA
    
    num <- as.numeric(input$num_investments)
    
    temp_tbl <- data.frame(
      `Ticker(s)` = paste0("Ticker ", seq_len(num)),  # Example placeholder names
      `Current Shares` = rep(0, num),
      `Current Dollars` = rep(0, num),
      `Desired %` = rep(0, num),
      `Shares Needed` = rep(0, num),
      `Dollars Needed` = rep(0, num),
      stringsAsFactors = FALSE
    )
    
    investments(temp_tbl)
  })
  
  # Capture edits made to table
  observeEvent(input$investTbl_cell_edit, {
    info <- input$investTbl_cell_edit
    updated_data <- investments()
    updated_data[info$row, info$col] <- info$value
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
    
    shinyjs::show("loading_spinner")
    temp_tbl = investments()
    
    #create a for loop going through each of my 'investments' in temp tbl. as it goes through, assign it as new("Investment")
    temp_port <- new("Portfolio") 
    list_desire <-list()
    
    for(i in 1:nrow(temp_tbl)){
      temp_investment <- new("Investment")
      temp_investment <- setTicker(temp_investment, temp_tbl[i,1])
      temp_investment <- setShare(temp_investment, temp_tbl[i,2])
      temp_investment <- updateSharePrice(temp_investment)
      temp_investment <- updateTotalValue(temp_investment)
      
      temp_port <- addInvestToPortfolio(temp_port, temp_investment)
      list_desire <-append(list_desire, temp_tbl[i,4])
    }
    temp_port <- updatePortfolio(temp_port)
    temp_pm <- new("PortfolioManager")
    temp_pm@portfolio = temp_port
    temp_pm <- updateInvestPerc(temp_pm)
    
    temp_pm@money_to_invest <- input$money_to_invest
    temp_pm = setDesiredInvestPerc(temp_pm, list_desire)
    temp_pm = updateFutureNeeds(temp_pm)
    #print temp_tbl <- returnPortfolioManagerTable(temp_pm)
    
    output_tbl <- returnPortfolioManagerTable(temp_pm)
    names(output_tbl) <- names(temp_tbl)
    
    shinyjs::hide("loading_spinner")
    
    investments(output_tbl)
  })
  
  
  
  # Retirement Growth Plot ----
  # Reactive expression for retirement growth plot data
  reactiveGrowthPlotData <- eventReactive(input$growthplotbtn, {
    data <- projected_data()  # Get the reactive data frame
    
    if (input$household_count == "One Person") {
      # Convert Balance column to numeric on the fly
      data <- data %>% mutate(Balance = as.numeric(gsub("[$,]", "", Balance)))
      
      p <- ggplot(data, aes(x = Year, y = Balance, group = 1, text= paste("Year: ", Year, "<br>", 
                                                                          "Age: ", Age, "<br>",
                                                                          "Balance: ", dollar(Balance)))) +
        geom_line(color = "#c09bd5") +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(title = "Projected Balance",
             x = "Year",
             y = "Balance") +
        theme_minimal()
      
    } else {
      # Convert columns to numeric on the fly for two persons scenario
      data <- data %>%
        mutate(p1_Balance = as.numeric(gsub("[$,]", "", p1_Balance)),
               p2_Balance = as.numeric(gsub("[$,]", "", p2_Balance)),
               `Total Balance` = as.numeric(gsub("[$,]", "", `Total Balance`)))
      
      p <- ggplot(data, aes(x = Year)) + 
        geom_line(aes(y = p1_Balance, color = "Person 1", group = 1, text= paste("Year: ", Year, "<br>", 
                                                                                 "P1 Age: ", p1_Age, "<br>",
                                                                                 "P1 Balance: ", dollar(p1_Balance)))) + 
        geom_line(aes(y = p2_Balance, color = "Person 2", group = 1, text= paste("Year: ", Year, "<br>", 
                                                                                 "P2 Age: ", p2_Age, "<br>",
                                                                                 "P2 Balance: ", dollar(p2_Balance)))) + 
        geom_line(aes(y = `Total Balance`, color = "Total", group = 1, text= paste("Year: ", Year, "<br>",
                                                                                   "Total Balance: ", dollar(`Total Balance`)))) + 
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(title = "Projected Balance",
             x = "Year",
             y = "Balance",
             color = "Household") +
        theme_minimal()
    }
    
    ggplotly(p, tooltip = c("text"))
  })
  
  # Render the retirement growth plot
  output$retireGrowthPlot <- renderPlotly({
    reactiveGrowthPlotData()
  })
  
  # Reactive expression for retirement growth table data
  reactiveGrowthTableData <- eventReactive(input$growthtablebtn, {
    if (input$household_count == "One Person") {
      datatable(projected_data(),
                fillContainer = TRUE, 
                options = list(pageLength = 50, 
                               autoWidth = FALSE, 
                               searching = FALSE,
                               lengthChange = FALSE)) %>%
        formatStyle("Balance", backgroundColor = "lightblue") %>%
        formatStyle(columns = colnames(.$x$data), `font-size` = '12px')
    } else {
      datatable(projected_data(),
                fillContainer = TRUE, 
                options = list(pageLength = 50, 
                               autoWidth = FALSE, 
                               searching = FALSE,
                               lengthChange = FALSE)) %>%
        formatStyle("p1_Balance", backgroundColor = "lightblue") %>% 
        formatStyle("p2_Balance", backgroundColor = "lightblue") %>%
        formatStyle("Total Balance", backgroundColor = "lightyellow") %>%
        formatStyle(columns = colnames(.$x$data), `font-size` = '12px')
    }
  })
  
  # Render the retirement growth table
  output$retireGrowthTbl <- DT::renderDataTable({
    reactiveGrowthTableData()
  }) # End of eventReactive
  
  output$downloadbtn <- downloadHandler(
    filename = function() {
      paste("RetirementProjections-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(projected_data(), file)
    }
  )
} #End of server

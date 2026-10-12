# mod_invest.R

# Author: Rosie Schmitz

# Investment Diversification tab: editable holdings table -> shares/dollars needed
# to reach desired allocation after investing new money

investUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2('Investment Diversification'),
    infoBox(
      title = "Welcome to the Finance App",
      value = "Please enter the inputs in the pink cells for your portfolio!",
      icon = icon('chart-line'),
      color = "purple",
      fill = FALSE, 
      width = 12
    ),
    tabBox(
      title = "Invest",
      width = 12,
      tabPanel(
        title = strong('Portfolio'), 
        fluidRow(
          column(width = 6, 
                 numericInput(
                   inputId = ns("money_to_invest"),
                   label = "Money To Invest ($):",
                   value = app_defaults$invest$money_to_invest,
                   min = 0,
                   max = 10000000,
                   step = 1
                 ), 
                 selectInput(
                   inputId = ns("num_investments"),
                   label = "Number of Investments:",
                   choices = 2:10,
                   selected = app_defaults$invest$num_investments
                 )
          )
        ),
        fluidRow(
          column(width = 12, 
                 tags$div(style = "margin-bottom: 10px;", DT::dataTableOutput(ns('investTbl'))))
        ),
        fluidRow(
          column(width = 6, offset = 3,
                 actionButton(inputId = ns("investbtn"), label = "Run Analysis"))
        )
      )
    )
  )
}

investServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    empty_rows <- function(row_numbers) {
      data.frame(
        `Ticker(s)` = sprintf("Ticker %d", row_numbers),  # Example placeholder names
        `Current Shares` = rep(0, length(row_numbers)),
        `Current Dollars` = rep(0, length(row_numbers)),
        `Desired %` = rep(0, length(row_numbers)),
        `Shares Needed` = rep(0, length(row_numbers)),
        `Dollars Needed` = rep(0, length(row_numbers)),
        stringsAsFactors = FALSE
      )
    }
    
    investments <- reactiveVal(empty_rows(integer(0)))
    
    # Update table when num_investments changes, keeping rows already filled in
    observeEvent(input$num_investments, {
      num <- as.numeric(input$num_investments)
      kept <- head(investments(), num)
      if (nrow(kept) < num) {
        kept <- rbind(kept, empty_rows(seq(nrow(kept) + 1, num)))
      }
      investments(kept)
    })
    
    # Capture edits made to table (DT sends edited values as text, so convert back to the column's type)
    observeEvent(input$investTbl_cell_edit, {
      info <- input$investTbl_cell_edit
      updated_data <- investments()
      updated_data[info$row, info$col] <- DT::coerceValue(info$value, updated_data[info$row, info$col])
      investments(updated_data)
    })
    
    output$investTbl <- DT::renderDataTable({
      renamed_data <- investments()
      colnames(renamed_data) <- c("Ticker", "Current Shares", "Current Dollars", "Desired %", "Shares Needed", "Dollars Needed")
      
      datatable(
        renamed_data, 
        editable = list(target = "cell", disable = list(columns = c(0,3,5,6))),
        options = list(
          searching = FALSE,
          lengthChange = FALSE,
          striped = FALSE,
          ordering = FALSE,
          paging = FALSE,
          info = FALSE
        )
      ) %>%
        formatStyle(c("Ticker", "Current Shares", "Desired %"), backgroundColor = "#ffcccb", color = "black") %>%
        formatRound("Shares Needed", digits = 3) %>%
        formatCurrency("Current Dollars", currency = "$") %>%
        formatCurrency("Dollars Needed", currency = "$") %>%
        formatStyle('Shares Needed', color = styleInterval(0, c('#ff4d4d', '#91cf60'))) %>%
        formatStyle('Dollars Needed', color = styleInterval(0, c('#ff4d4d', '#91cf60')))
    })
    
    observeEvent(input$investbtn, {
      temp_tbl <- investments()
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
      
      shinyjs::show("loading_spinner", asis = TRUE)
      on.exit(shinyjs::hide("loading_spinner", asis = TRUE))
      
      output_tbl <- tryCatch({
        # Each holding fetches its quote once; later steps reuse those prices
        temp_port <- new("Portfolio") 
        list_desire <- list()
        
        for (i in seq_len(nrow(temp_tbl))) {
          temp_investment <- new("Investment")
          temp_investment <- setTicker(temp_investment, tickers[i])
          temp_investment <- setShare(temp_investment, as.numeric(temp_tbl[i, 2]))
          temp_investment <- updateSharePrice(temp_investment)
          if (length(temp_investment@share_price) != 1 || is.na(temp_investment@share_price)) {
            stop("No price found for ticker '", tickers[i], "'.")
          }
          temp_port <- addInvestToPortfolio(temp_port, temp_investment)
          list_desire <- append(list_desire, as.numeric(temp_tbl[i, 4]))
        }
        temp_port <- updatePortfolio(temp_port, refresh = FALSE)
        
        temp_pm <- new("PortfolioManager")
        temp_pm@portfolio <- temp_port
        temp_pm <- updateInvestPerc(temp_pm)
        temp_pm@money_to_invest <- input$money_to_invest
        temp_pm <- setDesiredInvestPerc(temp_pm, list_desire)
        temp_pm <- updateFutureNeeds(temp_pm)
        
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
  })
}

# mod_budget.R

# Author: Rosie Schmitz

# Budget Quick Look tab: monthly expenses -> 50/30/20 breakdown, pie charts, notes

budget_fields <- tibble::tribble(
  ~id,             ~label,                        ~max,  ~step, ~column,
  "take_home",     "Total Take Home ($):",        20000, 10,    1,
  "rent_mort",     "Rent/Mortgage ($):",          5000,  1,     1,
  "car_payment",   "Car Payment ($):",            20000, 1,     1,
  "groceries",     "Groceries ($):",              10000, 0.5,   1,
  "home_ins",      "Home/Renters Insurance ($):", 500,   5,     1,
  "home_maint",    "Home Maintenance ($):",       1000,  10,    1,
  "car_ins",       "Car Insurance ($):",          500,   5,     1,
  "car_maint",     "Car Maintenance ($):",        500,   5,     1,
  "utilities",     "Utilities ($):",              1000,  10,    2,
  "phones",        "Phones ($):",                 1000,  10,    2,
  "pets",          "Pets ($):",                   1000,  10,    2,
  "subscriptions", "Subscriptions ($):",          1000,  5,     2,
  "dining_out",    "Dining Out ($):",             2000,  5,     2,
  "hobbies",       "Hobbies ($):",                3000,  5,     2,
  "savings",       "Savings ($):",                10000, 10,    2,
  "investing",     "Investing ($):",              10000, 10,    2
)

budgetUI <- function(id) {
  ns <- NS(id)
  
  field_inputs <- function(col) {
    fields <- budget_fields[budget_fields$column == col, ]
    lapply(seq_len(nrow(fields)), function(i) {
      numericInput(
        inputId = ns(fields$id[i]),
        label = fields$label[i],
        value = app_defaults$budget[[fields$id[i]]],
        min = 0,
        max = fields$max[i],
        step = fields$step[i]
      )
    })
  }
  
  tagList(
    h2('Budget Quick Look'),
    infoBox(
      title = "Welcome to the Finance App",
      value = "Please use the inputs page to fill in monthly household expenses. Next, view Budget Analysis to see graphs",
      icon = icon('chart-pie'),
      color = "blue",
      fill = FALSE, 
      width = 12
    ),
    
    tabBox(
      title = "Budget",
      width = 12,
      tabPanel(
        title = strong('Inputs'),
        fluidRow(
          column(width = 6, field_inputs(1)),
          column(width = 6, field_inputs(2))
        )
      ),
      tabPanel(
        title = strong('Budget Analysis'),
        fluidRow(
          column(width = 12, offset = 5,
                 actionButton(inputId = ns("budgetbtn"), label = "Run Analysis"))
        ),
        fluidRow(
          uiOutput(ns("noteOutput"))
        ),
        fluidRow(
          column(width = 12, DT::dataTableOutput(ns('budgetTbl')))
        ),
        fluidRow(
          column(width = 6,
                 div(style = "overflow: auto; height: 500px;", plotlyOutput(ns('budgetPieChart')))),
          column(width = 6,
                 div(style = "overflow: auto; height: 500px;", plotlyOutput(ns('budgetPieChart2'))))
        )
      )
    )
  )
}

budgetServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    budget_data <- reactive({
      values <- lapply(setNames(budget_fields$id, budget_fields$id), function(f) input[[f]])
      validate(
        need(all(vapply(values, isTruthy, logical(1))),
             "Fill in every field on the Inputs tab (use 0 for none)."),
        need(values$take_home > 0, "Enter a Total Take Home amount above $0 on the Inputs tab.")
      )
      do.call(budget.output, values)
    })
    
    # Pie charts can't draw negative or zero slices (e.g. Unallocated when overspending);
    # overspending is reported in the note instead
    reactivePieData <- eventReactive(input$budgetbtn, {
      list(
        pieChart1 = budget_data() %>%
          select(c('Needs', 'Wants', 'Savings_Investing', 'Unallocated')) %>%
          pivot_longer(everything(), cols_vary = "slowest", names_to = "Type") %>%
          filter(value > 0),
        pieChart2 = budget_data() %>%
          select(-c('Needs %', 'Wants %', 'Savings/Investing %', 'Needs', 'Wants', 'Savings_Investing', 'Take_Home', 'Unallocated %')) %>%
          pivot_longer(everything(), cols_vary = "slowest", names_to = "Type") %>%
          filter(value > 0)
      )
    })
    
    pie_layout <- function(fig, title) {
      fig %>% layout(title = title,
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     margin = list(l = 40, r = 40, b = 100, t = 100),
                     height = 500)
    }
    
    output$budgetPieChart <- renderPlotly({
      plot_ly(reactivePieData()$pieChart1, labels = ~Type,
              values = ~value,
              type = 'pie',
              hoverinfo = 'label+text',
              insidetextfont = list(color = '#FFFFFF'),
              text = ~dollar(value),
              marker = list(colors = c('#fc8d62', '#8da0cb', '#66c2a5'),
                            line = list(color = '#FFFFFF', width = 1)),
              showlegend = TRUE) %>%
        pie_layout('Monthly Budget Expenditures')
    })
    
    output$budgetPieChart2 <- renderPlotly({
      plot_ly(reactivePieData()$pieChart2, labels = ~Type,
              values = ~value,
              type = 'pie',
              hoverinfo = 'label+text',
              insidetextfont = list(color = '#FFFFFF', size = 9),
              text = ~dollar(value),
              showlegend = TRUE) %>%
        pie_layout('Detailed Monthly Budget Expenditures')
    })
    
    reactiveTableData <- eventReactive(input$budgetbtn, {
      budget_data() %>%
        select(c('Needs %', 'Wants %', 'Savings/Investing %'))
    })
    
    output$budgetTbl <- DT::renderDataTable({
      datatable(reactiveTableData(),
                rownames = FALSE,
                fillContainer = TRUE,
                options = list(
                  dom = 't',
                  ordering = FALSE,
                  rowCallback = JS(
                    "function(row, data, index) {",
                    "$('td', row).css('height', '50px');",
                    "}"
                  ),
                  columnDefs = list(
                    list(className = 'dt-column-separator', targets = '_all'),
                    list(targets = 0:2, width = '150px')
                  )
                )
      ) %>%
        formatStyle('Needs %', backgroundColor = styleInterval(50, c('#91cf60', '#fc8d59'))) %>%
        formatStyle('Wants %', backgroundColor = styleInterval(30, c('#91cf60', '#fc8d59'))) %>%
        formatStyle('Savings/Investing %', backgroundColor = styleInterval(20, c('#fc8d59', '#91cf60')))
    })
    
    reactiveNoteOutput <- eventReactive(input$budgetbtn, {
      unallocated_value <- budget_data()$Unallocated
      
      principle_note <- div(
        "This is following the 50/30/20 principle. It is advised to have 'Needs' less than or equal to 50%, 'Wants' less than or equal to 30%, and 'Savings/Investing' greater than or equal to 20%.", 
        style = "font-size:14px; color: gray; padding: 10px;"
      )
      
      conditional_note <- if (unallocated_value > 0) {
        div(sprintf("You have $%0.2f surplus unallocated.", unallocated_value),
            style = "font-size:14px; color: green; padding: 10px;")
      } else if (unallocated_value < 0) {
        div(sprintf("You are overspending by $%0.2f.", abs(unallocated_value)),
            style = "font-size:14px; color: red; padding: 10px;")
      } else {
        div("Your budget is perfectly allocated with no surplus or deficit.",
            style = "font-size:14px; color: gray; padding: 10px;")
      }
      
      tagList(principle_note, conditional_note)
    })
    
    output$noteOutput <- renderUI({
      reactiveNoteOutput()
    })
  })
}

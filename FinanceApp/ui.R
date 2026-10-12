# ui.R

# Author: Rosie Schmitz

# Preamble ----
source("global.R") # Load global variables
source("packages.R") # Load packages


# height of figures
figure.height <- '600px'
loading.spinner.color <<- "#6f42c1"

# UI Definition ----
shinyUI(
  dashboardPage(
    skin = 'black',
    
    # Header ----
    dashboardHeader(title = 'Finance App for Budgeting, Investment, and Retirement', titleWidth = '500'),
    
    # Sidebar ----
    dashboardSidebar(
      useShinyjs(),
      width = 250,
      sidebarMenu(
        id = 'sidebarMenu',
        
        # Script for Sidebar toggle
        tags$script(
          "$(document).on('click', '.sidebar-toggle',
          function () {
            Shiny.onInputChange('SideBar_col_react', Math.random())
          });"
        ),
        
        # Style for sidebar active state
        tags$head(
          tags$style(HTML(
            ".skin-black .sidebar-menu > li.active > a,
         .skin-black .sidebar-menu > li:hover > a {
           border-left-color: #5f939a;
           background-color: #5f939a;
         }"
          ))
        ),
        
        # Menu items
        menuItem(
          'Budget Quick Look',
          tabName = 'budgetTab',
          icon = icon('chart-pie')
        ),
        
        menuItem(
          'Investment Diversification',
          tabName = 'investmentTab',
          icon = icon('money-bill-trend-up')
        ),
        
        menuItem(
          'Retirement Analysis',
          tabName = 'retirementTab',
          icon = icon('umbrella-beach')
        ),
        
        hr(),  # Separator line
        
        # Radio Buttons for Retirement Analysis
        conditionalPanel(
          condition = "input.sidebarMenu == 'retirementTab'",
          radioButtons(
            inputId = "household_count",
            label = "House Hold Count:",
            choices = c("One Person", "Two Person"),
            selected = "One Person"
          )
        )
      )  # End sidebarMenu
    ),  # End dashboardSidebar
    
    
    # Body ----
    dashboardBody(
      useShinyjs(),
      tags$head(
        tags$style(HTML("
      hr {border-top: 1px solid #000000;}

      .modal-dialog {
        width: fit-content !important;
      }
      
   /* Custom Spinner */
      #loading_spinner {
        display: none;
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        font-size: 30px;
        color: ", loading.spinner.color, ";  /* Inject R variable here */
        z-index: 9999;
      }
        
      #p1_age-label,
      #p1_salary-label,
      #p1_wagegrowth-label,
      #p1_emp_match-label,
      #p1_emp_contrib-label,
      #p1_ratereturn-label,
      #p1_roth-label,
      #p1_brokerage-label,
      #p1_hsa-label,
      #p2_age-label,
      #p2_salary-label,
      #p2_wagegrowth-label,
      #p2_emp_match-label,
      #p2_emp_contrib-label, 
      #p2_ratereturn-label, 
      #p2_roth-label, 
      #p2_brokerage-label,
      #p2_hsa-label, 
      #p1_current401k-label, 
      #p1_currentroth-label,
      #p1_currentbrokerage-label,
      #p1_currenthsa-label,
      #p2_current401k-label,
      #p2_currentroth-label,
      #p2_currentbrokerage-label,
      #p2_currenthsa-label,
      #p1_lastworkyear-label,
      #p2_lastworkyear-label {
        font-size: 0.75em;
      }

     /* Custom styles for DataTable columns */
      .dataTables_wrapper .dataTables_scroll .dataTables_scrollHead .dataTables_scrollHeadInner {
        border-collapse: separate;
      }

      .dataTables_wrapper .dataTables_scroll .dataTables_scrollHead th {
        border-right: 2px solid white; /* Adjust color if needed */
      }

      .dataTables_wrapper .dataTables_scroll .dataTables_scrollHead th:last-child {
        border-right: none;
      }

      .dataTables_wrapper .dataTables_scroll .dataTables_scrollBody td {
        border-right: 2px solid white; /* Adjust color if needed */
      }
    "))  # End of tags$style
      ),  # End of tags$head
      
      div(id = "loading_spinner", 
          tags$span(class = "fa fa-spinner fa-spin", style = "font-size: 50px;")),
      
      
      # Tabs for 3 sections ----
      tabItems(
        # Budget Quick Look Tab ----
        tabItem(
          tabName = 'budgetTab',
          h2('Budget Quick Look'),
          infoBox(
            title = "Welcome to the Finance App",
            value = "Please use the inputs page to fill in monthly household expenses. Next, view Budget Analysis to see graphs",
            icon = icon('chart-pie'),
            color = "blue",
            fill = F, 
            width = 12
          ), # End of info box
          
          # Budget tab panels
          tabBox(
            title = "Budget",
            width = 12,
            tabPanel(
              title = strong('Inputs'),
              fluidRow(
                column(width = 6,
                       numericInput(
                         inputId = "take_home",
                         label = "Total Take Home ($):",
                         value = 7966,
                         min = 0,
                         max = 20000,
                         step = 10,
                         width = NA
                       ),
                       numericInput(
                         inputId = "rent_mort",
                         label = "Rent/Mortgage ($):",
                         value = 2133,
                         min = 0,
                         max = 5000,
                         step = 1,
                         width = NA
                       ),
                       numericInput(
                         inputId = "car_payment",
                         label = "Car Payment ($):",
                         value = 0,
                         min = 0,
                         max = 20000,
                         step = 1,
                         width = NA
                       ),
                       numericInput(
                         inputId = "groceries",
                         label = "Groceries ($):",
                         value = 700,
                         min = 0,
                         max = 10000,
                         step = 0.5,
                         width = NA
                       ),
                       numericInput(
                         inputId = "home_ins",
                         label = "Home/Renters Insurance ($):",
                         value = 98,
                         min = 0,
                         max = 500,
                         step = 5,
                         width = NA
                       ),
                       numericInput(
                         inputId = "home_maint",
                         label = "Home Maintenance ($):",
                         value = 100,
                         min = 0,
                         max = 1000,
                         step = 10,
                         width = NA
                       ),
                       numericInput(
                         inputId = "car_ins",
                         label = "Car Insurance ($):",
                         value = 140,
                         min = 0,
                         max = 500,
                         step = 5,
                         width = NA
                       ),
                       numericInput(
                         inputId = "car_maint",
                         label = "Car Maintenance ($):",
                         value = 50,
                         min = 0,
                         max = 500,
                         step = 5,
                         width = NA
                       )
                ), # End of col 1
                column(width = 6,
                       numericInput(
                         inputId = "utilities",
                         label = "Utilities ($):",
                         value = 145,
                         min = 0,
                         max = 1000,
                         step = 10,
                         width = NA
                       ),
                       numericInput(
                         inputId = "phones",
                         label = "Phones ($):",
                         value = 172,
                         min = 0,
                         max = 1000,
                         step = 10,
                         width = NA
                       ),
                       numericInput(
                         inputId = "pets",
                         label = "Pets ($):",
                         value = 50,
                         min = 0,
                         max = 1000,
                         step = 10,
                         width = NA
                       ),
                       numericInput(
                         inputId = "subscriptions",
                         label = "Subscriptions ($):",
                         value = 39,
                         min = 0,
                         max = 1000,
                         step = 5,
                         width = NA
                       ),
                       numericInput(
                         inputId = "dining_out",
                         label = "Dining Out ($):",
                         value = 300,
                         min = 0,
                         max = 2000,
                         step = 5,
                         width = NA
                       ),
                       numericInput(
                         inputId = "hobbies",
                         label = "Hobbies ($):",
                         value = 500,
                         min = 0,
                         max = 3000,
                         step = 5,
                         width = NA
                       ),
                       numericInput(
                         inputId = "savings",
                         label = "Savings ($):",
                         value = 800,
                         min = 0,
                         max = 10000,
                         step = 10,
                         width = NA
                       ),
                       numericInput(
                         inputId = "investing",
                         label = "Investing ($):",
                         value = 2350,
                         min = 0,
                         max = 10000,
                         step = 10,
                         width = NA
                       ) # End of input
                ) # End of col 2
              ) # End of fluid row
            ), # End of tab panel
            tabPanel(
              title = strong('Budget Analysis'),
              fluidRow(
                column(
                  width = 12,
                  offset = 5,
                  actionButton(
                    inputId = "budgetbtn",
                    label = "Run Analysis"
                  )
                )
              ),
              fluidRow(
                uiOutput("noteOutput")  # Conditionally display the note here
              ),
              fluidRow(
                column(
                  width = 12,
                  DT::dataTableOutput('budgetTbl')
                ) # End of col
              ), # End of fluid row
              # Row for the pie chart and data table
              fluidRow(
                column(
                  width = 6,
                  div(style = "overflow: auto; height: 500px;", plotlyOutput('budgetPieChart')
                  )
                ),
                column(
                  width = 6,
                  div(style = "overflow: auto; height: 500px;", plotlyOutput('budgetPieChart2')
                  )
                )
              ), # End of fluidRow
            ) # End of tab panel
          ) # End of tab box
        ), # End tab item
        
        # Investment Diversification Tab ----
        tabItem(
          tabName = 'investmentTab',
          h2('Investment Diversification'),
          infoBox(
            title = "Welcome to the Finance App",
            value = "Please enter the inputs in the pink cells for your portfolio!",
            icon = icon('chart-line'),
            color = "purple",
            fill = F, 
            width = 12
          ), # End of info box
          tabBox(
            title = "Invest",
            width = 12,
            tabPanel(
              title = strong('Portfolio'), 
              fluidRow(
                column(
                  width = 6, 
                  numericInput(
                    inputId = "money_to_invest",
                    label = "Money To Invest ($):",
                    value = 1000,
                    min = 0,
                    max = 10000000,
                    step = 1
                  ), 
                  selectInput(
                    inputId = "num_investments",
                    label = "Number of Investments:",
                    choices = 2:10,
                    selected = 3    
                  )
                )
              ), # End of 1st fluidrow
              fluidRow(
                column(
                  width = 12, 
                  tags$div(
                    style = "margin-bottom: 10px;", 
                    DT::dataTableOutput('investTbl')
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6, 
                  offset = 3,
                  actionButton(
                    inputId = "investbtn",
                    label = "Run Analysis"
                  )
                )
              ) # End of 2nd fluidrow
            ), # End of tab panel
          ) # End of tab box
        ), # End of tab item
        
        # Retirement Analysis Tab ----
        tabItem(
          tabName = 'retirementTab',
          h2('Retirement Analysis'),
          infoBox(
            title = "Welcome to the Finance App",
            value = "Choose the number of earners in the sidebar, then fill out each person's inputs and the Household Plan on the Inputs tab. Click Run Analysis on Growth Plot to update the plot and table, then use the Monte Carlo tab to stress-test withdrawals.",
            icon = icon('comment-dollar'),
            color = "green",
            fill = FALSE, 
            width = 12
          ),
          
          # Retirement tab panels
          tabBox(
            title = "Retirement",
            width = 12,
            tabPanel(
              title = strong('Inputs'),
              fluidRow(
                column(width = 6,
                       infoBox("","Person One",
                               icon = icon("user"),
                               width = 12, 
                               color = "light-blue"
                       ),
                       numericInput(
                         inputId = "p1_age",
                         label = "Person 1 Age:",
                         value = 30,
                         min = 18,
                         max = 120,
                         step = 1
                       ),
                       numericInput(
                         inputId = "p1_salary",
                         label = "Person 1 Salary ($):",
                         value = 81224,
                         min = 1000,
                         max = 1000000,
                         step = 1
                       ),
                       numericInput(
                         inputId = "p1_wagegrowth",
                         label = "Wage Growth Rate (%):",
                         value = 3,
                         min = 0,
                         max = 100,
                         step = 0.5
                       ),
                       numericInput(
                         inputId = "p1_emp_match",
                         label = "Employer Matching (%):",
                         value = 7,
                         min = 0,
                         max = 100,
                         step = 0.5
                       ),
                       numericInput(
                         inputId = "p1_emp_contrib",
                         label = "401k Employee Contributions ($):",
                         value = 24500,
                         min = 0,
                         max = max_limits$lim401k50,
                         step = 1
                       ),
                       numericInput(
                         inputId = "p1_ratereturn",
                         label = "Rate of Return (%):",
                         value = 7,
                         min = 0,
                         max = 1000,
                         step = 0.1
                       ),
                       numericInput(
                         inputId = "p1_roth",
                         label = "Annual Roth Contrib ($):",
                         value = 7500,
                         min = 0,
                         max = max_limits$limrothira50,
                         step = 0.1
                       ),
                       numericInput(
                         inputId = "p1_brokerage",
                         label = "Annual Taxable Brokerage Contrib ($):",
                         value = 6000,
                         min = 0,
                         max = 50000000,
                         step = 0.1
                       ),
                       numericInput(
                         inputId = "p1_hsa",
                         label = "Annual HSA Contrib ($):",
                         value = 0,
                         min = 0,
                         max = max_limits$limhsaone55,
                         step = 0.1
                       ),
                       numericInput(
                         inputId = "p1_lastworkyear",
                         label = "Last Working Year (contributions stop after):",
                         value = 2038,
                         min = 2000,
                         max = 2150,
                         step = 1
                       ),
                       numericInput(
                         inputId = "p1_current401k",
                         label = "Current 401k Amount ($):",
                         value = 57114.61,
                         min = 0,
                         max = 10000000,
                         step = 0.1
                       ),
                       numericInput(
                         inputId = "p1_currentroth",
                         label = "Current Roth Amount ($):",
                         value = 59233.75,
                         min = 0,
                         max = 10000000,
                         step = 0.1
                       ),
                       numericInput(
                         inputId = "p1_currentbrokerage",
                         label = "Current Taxable Brokerage Amount ($):",
                         value = 17015.88,
                         min = 0,
                         max = 100000000,
                         step = 0.1
                       ),
                       numericInput(
                         inputId = "p1_currenthsa",
                         label = "Current HSA Amount ($):",
                         value = 0,
                         min = 0,
                         max = 10000000,
                         step = 0.1
                       )
                ),
                column(width = 6,
                       conditionalPanel(
                         condition = "input.household_count == 'Two Person'",
                         infoBox("","Person Two",
                                 icon = icon("user-plus"),
                                 width = 12, 
                                 color = "teal"
                         ),
                         numericInput(
                           inputId = "p2_age",
                           label = "Person 2 Age:",
                           value = 43,
                           min = 18,
                           max = 120,
                           step = 1
                         ),
                         numericInput(
                           inputId = "p2_salary",
                           label = "Person 2 Salary ($):",
                           value = 96844.80,
                           min = 1000,
                           max = 1000000,
                           step = 1
                         ),
                         numericInput(
                           inputId = "p2_wagegrowth",
                           label = "Wage Growth Rate (%):",
                           value = 3,
                           min = 0,
                           max = 100,
                           step = 0.5
                         ),
                         numericInput(
                           inputId = "p2_emp_match",
                           label = "Employer Matching (%):",
                           value = 7,
                           min = 0,
                           max = 100,
                           step = 0.5
                         ),
                         numericInput(
                           inputId = "p2_emp_contrib",
                           label = "401k Employee Contribution ($):",
                           value = 24500,
                           min = 0,
                           max = max_limits$lim401k50,
                           step = 1
                         ),
                         numericInput(
                           inputId = "p2_ratereturn",
                           label = "Rate of Return (%):",
                           value = 7,
                           min = 0,
                           max = 1000,
                           step = 0.1
                         ),
                         numericInput(
                           inputId = "p2_roth",
                           label = "Annual Roth Contrib ($):",
                           value = 7500,
                           min = 0,
                           max = max_limits$limrothira50,
                           step = 0.1
                         ),
                         numericInput(
                           inputId = "p2_brokerage",
                           label = "Annual Taxable Brokerage Contrib ($):",
                           value = 6000,
                           min = 0,
                           max = 50000000,
                           step = 0.1
                         ),
                         numericInput(
                           inputId = "p2_hsa",
                           label = "Annual HSA Contrib ($):",
                           value = 0,
                           min = 0,
                           max = max_limits$limhsatwo55,
                           step = 0.1
                         ),
                         numericInput(
                           inputId = "p2_lastworkyear",
                           label = "Last Working Year (contributions stop after):",
                           value = 2037,
                           min = 2000,
                           max = 2150,
                           step = 1
                         ),
                         numericInput(
                           inputId = "p2_current401k",
                           label = "Current 401k Amount ($):",
                           value = 291885.52,
                           min = 0,
                           max = 10000000,
                           step = 0.1
                         ),
                         numericInput(
                           inputId = "p2_currentroth",
                           label = "Current Roth Amount ($):",
                           value = 24995.08,
                           min = 0,
                           max = 10000000,
                           step = 0.1
                         ),
                         numericInput(
                           inputId = "p2_currentbrokerage",
                           label = "Current Taxable Brokerage Amount ($):",
                           value = 17015.88,
                           min = 0,
                           max = 100000000,
                           step = 0.1
                         ),
                         numericInput(
                           inputId = "p2_currenthsa",
                           label = "Current HSA Amount ($):",
                           value = 0,
                           min = 0,
                           max = 10000000,
                           step = 0.1
                         )
                       ) # End of conditional panel
                )
              ), # End of fluidRow
              fluidRow(
                hr(),
                column(width = 12,
                       em(limits_note)
                ),
                column(width = 6,
                       textOutput("limit_401k_one_display"),
                       textOutput("limit_roth_ira_one_catchup_display"),
                       textOutput("limit_hsa_one_55_display")
                ),
                column(width = 6,
                       textOutput("limit_401k_two_display"),
                       textOutput("limit_roth_ira_two_catchup_display"),
                       textOutput("limit_hsa_two_55_display")
                )
              ), # End of 2nd fluid row
              fluidRow(
                hr(),
                column(width = 12,
                       infoBox("", "Household Plan (balances are in base-year dollars; returns are real)",
                               icon = icon("house"),
                               width = 12,
                               color = "purple"
                       )
                ),
                column(width = 4,
                       numericInput(
                         inputId = "base_year",
                         label = "Base Year (present $; projection starts here, opening balances are the year before):",
                         value = current_year,
                         min = 2000,
                         max = 2150,
                         step = 1
                       ),
                       numericInput(
                         inputId = "plan_through_age",
                         label = "Plan Through Age (Person 1):",
                         value = 95,
                         min = 19,
                         max = 120,
                         step = 1
                       ),
                       numericInput(
                         inputId = "anchor_year",
                         label = "Future Value Year (e.g. 2040$):",
                         value = 2040,
                         min = 2000,
                         max = 2150,
                         step = 1
                       ),
                       numericInput(
                         inputId = "inflation",
                         label = "Inflation (%) for future value columns:",
                         value = 3,
                         min = 0,
                         max = 20,
                         step = 0.1
                       )
                ),
                column(width = 4,
                       numericInput(
                         inputId = "withdraw_start_year",
                         label = "First Withdrawal Year:",
                         value = 2040,
                         min = 2000,
                         max = 2150,
                         step = 1
                       ),
                       numericInput(
                         inputId = "vpw_rate",
                         label = "VPW Withdrawal Rate (% of portfolio):",
                         value = 3,
                         min = 0,
                         max = 20,
                         step = 0.1
                       ),
                       numericInput(
                         inputId = "annual_spend",
                         label = "Annual Spend ($, base-year):",
                         value = 90000,
                         min = 0,
                         max = 10000000,
                         step = 1000
                       ),
                       radioButtons(
                         inputId = "withdraw_rule",
                         label = "Withdrawal Rule:",
                         choices = c("VPW capped at annual spend" = "capped",
                                     "Full VPW (spend the whole %)" = "full"),
                         selected = "capped"
                       )
                ),
                column(width = 4,
                       numericInput(
                         inputId = "retire_return",
                         label = "Real Return During Withdrawals (%; follows Person 1's rate until changed):",
                         value = 7,
                         min = -10,
                         max = 20,
                         step = 0.1
                       ),
                       numericInput(
                         inputId = "fire_target",
                         label = "FIRE Target ($, base-year):",
                         value = 3000000,
                         min = 0,
                         max = 100000000,
                         step = 10000
                       ),
                       conditionalPanel(
                         condition = "input.household_count == 'Two Person'",
                         checkboxInput(
                           inputId = "spousal_roth",
                           label = "Retired spouse keeps Roth contributions while the other still works (spousal Roth)",
                           value = TRUE
                         )
                       )
                )
              ) # End of plan fluid row
            ), # End Inputs Tab Panel
            
            tabPanel(
              title = strong('Growth Plot'),
              fluidRow(
                column(width = 6,
                       radioButtons(
                         inputId = "plot_dollars",
                         label = "Dollars shown:",
                         choices = c("Base year (present $)" = "base",
                                     "Future value year" = "anchor"),
                         selected = "base",
                         inline = TRUE
                       )
                ),
                column(width = 6,
                       actionButton(
                         inputId = "growthplotbtn",
                         label = "Run Analysis"
                       )
                )
              ),
              fluidRow(
                plotlyOutput('retireGrowthPlot')
              )
            ),
            
            tabPanel(
              title = strong('Growth Table'),
              fluidRow(
                column(width = 12,
                       p("Shows the same run as the Growth Plot. Click Run Analysis on the Growth Plot tab after changing inputs.")
                )
              ),
              fluidRow(
                DT::dataTableOutput('retireGrowthTbl', height = "50em")
              ), 
              fluidRow(
                column(width = 12, offset = 2,
                       downloadButton(
                         outputId = "downloadbtn",
                         label = "Download Table"
                       )
                )
              ) # End Fluid Row
            ), # End Growth Table Tab Panel
            
            tabPanel(
              title = strong('Monte Carlo'),
              fluidRow(
                column(width = 12,
                       p("Variable percentage withdrawal (VPW) simulation in base-year dollars with real returns. ",
                         "Start wealth, withdrawal rule, and annual spend come from the last Run Analysis on the Growth Plot tab."),
                       textOutput("mc_start_display")
                )
              ),
              fluidRow(
                column(width = 4,
                       numericInput(
                         inputId = "mc_start_override",
                         label = "Start Wealth Override ($, 0 = use projection):",
                         value = 0,
                         min = 0,
                         max = 1e9,
                         step = 10000
                       ),
                       numericInput(
                         inputId = "mc_horizon",
                         label = "Horizon (years):",
                         value = 50,
                         min = 1,
                         max = 100,
                         step = 1
                       ),
                       textInput(
                         inputId = "mc_rates",
                         label = "VPW Rates to Compare (%):",
                         value = "3, 3.5, 4, 4.5, 5, 5.5"
                       )
                ),
                column(width = 4,
                       numericInput(
                         inputId = "mc_floor",
                         label = "Lifestyle Floor ($, base-year):",
                         value = 90000,
                         min = 0,
                         max = 10000000,
                         step = 1000
                       ),
                       numericInput(
                         inputId = "mc_mean",
                         label = "Mean Real Return (%; follows Person 1's rate until changed):",
                         value = 7,
                         min = -10,
                         max = 20,
                         step = 0.1
                       ),
                       numericInput(
                         inputId = "mc_vol",
                         label = "Volatility (% std dev):",
                         value = 16,
                         min = 0,
                         max = 60,
                         step = 0.5
                       )
                ),
                column(width = 4,
                       numericInput(
                         inputId = "mc_sims",
                         label = "Simulations:",
                         value = 10000,
                         min = 100,
                         max = 100000,
                         step = 1000
                       ),
                       numericInput(
                         inputId = "mc_seed",
                         label = "Random Seed:",
                         value = 42,
                         min = 1,
                         step = 1
                       ),
                       actionButton(
                         inputId = "mcbtn",
                         label = "Run Monte Carlo"
                       )
                )
              ),
              fluidRow(
                hr(),
                column(width = 12,
                       DT::dataTableOutput('mcSummaryTbl')
                )
              ),
              fluidRow(
                hr(),
                column(width = 4,
                       uiOutput("mc_band_rate_ui")
                ),
                column(width = 12,
                       plotlyOutput('mcBandPlot', height = figure.height)
                )
              )
            ) # End Monte Carlo Tab Panel
          ) # End Tab Box
        ) # End Tab Item
      ) # End Tab Items
    ) # End Dashboard Body
  ) # End Dashboard Page
) # End Shiny UI


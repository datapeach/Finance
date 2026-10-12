# ui.R

# Author: Rosie Schmitz

# Page shell: header, sidebar, shared styles, and one module UI per tab.
# Everything this file uses is loaded by global.R.

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
        menuItem('Budget Quick Look', tabName = 'budgetTab', icon = icon('chart-pie')),
        menuItem('Investment Diversification', tabName = 'investmentTab', icon = icon('money-bill-trend-up')),
        menuItem('Retirement Analysis', tabName = 'retirementTab', icon = icon('umbrella-beach')),
        
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
        
      /* Smaller labels for the per-person retirement inputs */
      .compact-labels .control-label {
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
        tabItem(tabName = 'budgetTab', budgetUI("budget")),
        tabItem(tabName = 'investmentTab', investUI("invest")),
        tabItem(tabName = 'retirementTab', retirementUI("retirement"))
      )
    ) # End Dashboard Body
  ) # End Dashboard Page
) # End Shiny UI

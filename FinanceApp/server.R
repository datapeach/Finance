# server.R

# Author: Rosie Schmitz

# Wires each tab's module server. Everything this file uses is loaded by global.R.

# Server function ----
function(input, output, session) {
  budgetServer("budget")
  investServer("invest")
  retirementServer("retirement", household_count = reactive(input$household_count))
}

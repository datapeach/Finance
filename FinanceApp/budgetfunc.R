# Author: Rosie Schmitz 

#' budget.output()
#' 
#' @description 
#' returns a dataframe for budget to be used in other places of the app
#' loaded in the main application

# budgetfunc.R

budget.output <- function(take_home, rent_mort,car_payment, groceries, home_ins,
                          home_maint, car_ins, car_maint, utilities, phones, pets, subscriptions,
                          dining_out, hobbies, savings, investing) {
  
  total <- take_home
  Needs <- rent_mort + car_payment + groceries + home_ins + home_maint + car_ins + car_maint + utilities + phones + pets
  Needs_Perc <- round((Needs/total)*100, 2)
  Wants <- subscriptions + dining_out + hobbies
  Wants_Perc <- round((Wants/total)*100, 2)
  Savings_Investing <- savings + investing
  Savings_Investing_Perc <- round((Savings_Investing/total)*100,2)
  Unallocated <- total - Needs - Wants - Savings_Investing
  Unallocated_Perc <- round((Unallocated/total)*100,2)
  
  df <- data.frame(
    'Take_Home' = take_home,
    'Rent_Mortgage' = rent_mort,
    'Car_Payment' = car_payment,
    'Groceries' = groceries,
    'Home_Renters_Insurance' = home_ins,
    'Home_Maintenance' = home_maint, 
    'Car_Insurance' = car_ins, 
    'Car_Maintenance' = car_maint, 
    'Utilities' = utilities,
    'Phones' = phones,
    'Pets' = pets,
    'Subscriptions' = subscriptions, 
    'Dining_Out' = dining_out,
    'Hobbies' = hobbies, 
    'Savings' = savings,
    'Investing' = investing,
    'Needs' = Needs,
    'Needs_Percentage' = Needs_Perc,
    'Wants' = Wants,
    'Wants_Percentage' = Wants_Perc, 
    'Savings_Investing' = Savings_Investing,
    'Savings_Investing_Percentage' = Savings_Investing_Perc, 
    'Unallocated' = Unallocated, 
    'Unallocated_Perc' = Unallocated_Perc
    
  )
  
  df_renamed <- df %>%
    rename('Needs %' = Needs_Percentage,
           'Wants %' = Wants_Percentage, 
           'Savings/Investing %' = Savings_Investing_Percentage, 
           'Unallocated %' = Unallocated_Perc)

  return(df_renamed)
}


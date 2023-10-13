# joelfindlay 2023.09.08

# binary encoding
# FALSE = 0
# TRUE = 1
# NA = 2


library(readxl)
library(here)
library(dplyr)

#read in payment rate dat and convert to binary
youth_allowance_rate_data <- read_excel(here("transfers.xlsx")
           , sheet = "youth_allowance"
           , skip = 1) %>%
  head(10) %>%
  select(c(1:7)) %>%
  mutate_if(is.character, as.logical) %>%
  mutate_all(as.numeric) %>%
  replace(is.na(.), 2) 



#### Base rate ####
youth.allowance.rate.fn <- function(youth_allowance_rate_data
                                    , x_age
                                    , x_dependent
                                    , x_single
                                    , x_children
                                    , x_away_from_home
                                    ) {
  x_under_eighteen <- ifelse(x_age() < 18, 1, 0)
  
  base_rate <- youth_allowance_rate_data %>%
    filter(
      (dependent == x_dependent() | dependent > 1) &
        (single == x_single() | single > 1) &
        (children == x_children() | children > 1) &
        (away_from_home == x_away_from_home() | away_from_home > 1) &
        (under_eighteen == x_under_eighteen)
    ) %>%
    summarize(base_rate = sum(rate + energy_supplement)) %>%
    pull(base_rate)
  
  # If there are no matching rows, base_rate will be NA, so set it to 0 if necessary
  base_rate <- ifelse(is.na(base_rate), 0, base_rate)
  return(base_rate)

} 
  
# youth_allowance_rate_fn(youth_allowance_rate_data, x_age, x_dependent, x_single, x_children, x_away_from_home)
  




#### personal income test  ####
youth_allowance_personal_income_test_data <- read_excel(here("transfers.xlsx")
                                   , sheet = "youth_allowance_test"
                                   , skip = 1) %>%
  head(3) %>%
  select(c(1:2)) %>%
  mutate_if(is.character, as.numeric) 


youth.allowance.personal.income.test.fn <- function(youth_allowance_personal_income_test_data
                                                    , x_income_personal_fortnightly
                                                    ){
  
  reduction_personal_income <-
    max(0, (x_income_personal_fortnightly-youth_allowance_personal_income_test_data$income_personal_fortnightly[3])*youth_allowance_personal_income_test_data$reduction_rate[3]) +
    max((
      max(x_income_personal_fortnightly-youth_allowance_personal_income_test_data$income_personal_fortnightly[2], 0) - 
        max(x_income_personal_fortnightly-youth_allowance_personal_income_test_data$income_personal_fortnightly[3], 0)
      )*youth_allowance_personal_income_test_data$reduction_rate[2]
      , 0)    

  return(reduction_personal_income)
}  
# youth_allowance_personal_income_test_fn(youth_allowance_personal_income_test_data, x_income_personal_fortnightly)
    




### Parental income test ####
youth_allowance_parental_income_test_data <- read_excel(here("transfers.xlsx")
                                                   , sheet = "youth_allowance_test"
                                                   , skip = 8) %>%
  head(2) %>%
  select(c(1:3)) %>%
  mutate_at("dependent", as.logical) %>%
  mutate_if(is.character, as.numeric)  %>%
  mutate_all(as.numeric) %>%
  replace(is.na(.), 2)

youth.allowance.parental.income.test.fn <- function(youth_allowance_parental_income_test_data
                                                    , x_dependent
                                                    , x_income_parental_annual
                                                    ){
  if (x_dependent() ==1){
    reduction_parental_income <- 
      max(0, (x_income_parental_annual() - youth_allowance_parental_income_test_data$income_parental_annual[2])*youth_allowance_parental_income_test_data$reduction_rate[2]/365.25*14)
  } else {
    reduction_parental_income <- 0
  }
  return(reduction_parental_income)
}
#youth_allowance_parental_income_test_fn(youth_allowance_parental_income_test_data, x_dependent, x_income_parental_annual)





#### Partner income test ####
youth_allowance_partner_income_test_data <- read_excel(here("transfers.xlsx")
                                                   , sheet = "youth_allowance_test"
                                                   , skip = 13) %>%
  head(2) %>%
  select(c(1:3)) %>%
  mutate_at("single", as.logical) %>%
  mutate_if(is.character, as.numeric)  %>%
  mutate_all(as.numeric) %>%
  replace(is.na(.), 2)

youth.allowance.partner.income.test.fn <- function(youth_allowance_partner_income_test_data
                                                   , x_single
                                                   , x_income_partner_fortnightly
                                                   ){
  if (x_single() ==0){
    reduction_partner_income <- 
      max(0, (x_income_partner_fortnightly() - youth_allowance_partner_income_test_data$income_partner_fortnightly[2])*youth_allowance_partner_income_test_data$reduction_rate[2])
  } else {
    reduction_partner_income <- 0
  }
  return(reduction_partner_income)
}
#youth_allowance_partner_income_test_fn(youth_allowance_partner_income_test_data, x_single, x_income_partner_fortnightly)






#### personal asset test ####

youth_allowance_asset_test_data <- read_excel(here("transfers.xlsx")
                                                  , sheet = "youth_allowance_test"
                                                  , skip = 19) %>%
  head(4) %>%
  select(c(1:3)) %>%
  mutate_at("asset_cap", as.numeric) %>%
  mutate_if(is.character, as.logical)  %>%
  mutate_all(as.numeric) %>%
  replace(is.na(.), 2)


youth.allowance.asset.test.fn <- function(youth_allowance_asset_test_data
                                          , x_single
                                          , x_homeowner
                                          , x_personal_assets
                                          ){
  
  if (x_personal_assets() >
      youth_allowance_asset_test_data %>%
    filter(single != 1-x_single() 
         , homeowner !=  1-x_homeowner()) %>%
    pull(asset_cap)){
    terminal <<- 1
  } else {
    terminal <<- 0
  }
  return(terminal)
}
  
#youth_allowance_asset_test_fn(youth_allowance_asset_test_data, 1,1,200000)
  




#### scholarship reduction ####

youth_allowance_scholarship_exemption_data <- read_excel(here("transfers.xlsx")
                                         , sheet = "youth_allowance_test"
                                         , skip = 26) %>%
  head(1) %>%
  select(c(1:2)) %>%
  mutate_at("scholarship", as.logical) %>%
  mutate_if(is.character, as.numeric)  %>%
  mutate_all(as.numeric) %>%
  replace(is.na(.), 2)

youth.allowance.scholarship.exemption.fn <- function(youth_allowance_scholarship_exemption_data
                                                     , x_income_scholarship_annual
                                                     ){

  reduction_scholarship_income <- 
      max(0, (x_income_scholarship_annual() - youth_allowance_scholarship_exemption_data$scholarship_exemption_annual[1])/365.25*14)
  return(reduction_scholarship_income)
}
#youth_allowance_scholarship_exemption_fn(youth_allowance_scholarship_exemption_data, x_income_scholarship_annual)






#### deeming rates ####

youth_allowance_deeming_rates_data <- read_excel(here("transfers.xlsx")
                                                    , sheet = "youth_allowance_test"
                                                    , skip = 30) %>%
  head(6) %>%
  select(c(1:4)) %>%
  mutate_at(c(1,2), as.logical) %>%
  mutate_if(is.character, as.numeric)  %>%
  mutate_all(as.numeric) %>%
  replace(is.na(.), 2)


youth.allowance.deeming.rates.fn <- function(youth_allowance_deeming_rates_data
                                             , x_single
                                             , x_pension
                                             , x_assets_personal
                                             ){
   
    tmp <- youth_allowance_deeming_rates_data %>%
      filter(
        (single == x_single() | single > 1) 
        ,  (pension == x_pension() | pension > 1) 
      )
    
    deeming_reduction <- 
      max(0, (x_assets_personal() -   tmp$threshold[2]))*tmp$rate[2] +
      min(x_assets_personal(), tmp$threshold[2])*tmp$rate[1]
    
    return(deeming_reduction)
    
}

#youth_allowance_deeming_rates_fn(youth_allowance_deeming_rates_data, x_single, x_pension, x_assets_personal)






##### COMPLETE FN ####
youth.allowance <- function(
    x_income_personal_fortnightly,
    
    youth_allowance_rate_data, 
    youth_allowance_personal_income_test_data,
    youth_allowance_scholarship_exemption_data,
    youth_allowance_deeming_rates_data,
    youth_allowance_parental_income_test_data,
    youth_allowance_partner_income_test_data,
    
    x_income_partner_fortnightly,
    x_income_parental_annual,
    x_income_scholarship_annual,
    x_assets_personal,
    x_age,
    
    x_dependent,
    x_single,
    x_children,
    x_away_from_home,
    x_pension,
    x_homeowner,
    x_apprenticeship,
    x_completed_y12,
    x_ft_study
) {
  
  #### ELIGIBILITY
  if ((x_age() >= 18 & x_age() <= 24) |
      (x_age() >= 16 & x_age() < 18 & (x_apprenticeship() == 1 |
                                   (x_dependent() == 0 & x_away_from_home() == 1) |
                                   (x_ft_study() == 1 & x_completed_y12() == 1)))) {
    
    # Base rate
    base_rate <- youth.allowance.rate.fn(
      youth_allowance_rate_data
      , x_age
      , x_dependent
      , x_single
      , x_children
      , x_away_from_home)
    
    # Personal income test
    reduction_personal_income <- youth.allowance.personal.income.test.fn(
      youth_allowance_personal_income_test_data
      , x_income_personal_fortnightly
    )
    
    # Scholarship
    reduction_scholarship_income <- youth.allowance.scholarship.exemption.fn(
      youth_allowance_scholarship_exemption_data
      , x_income_scholarship_annual)
    
    # Deeming rates
    deeming_reduction <- youth.allowance.deeming.rates.fn(
      youth_allowance_deeming_rates_data
      , x_single
      , x_pension
      , x_assets_personal)

    # Parental means test
    reduction_parental_income <- youth.allowance.parental.income.test.fn(
      youth_allowance_parental_income_test_data
      , x_dependent
      , x_income_parental_annual)
    
    # Partner income test
    reduction_partner_income <- youth.allowance.partner.income.test.fn(
      youth_allowance_partner_income_test_data
      , x_single
      , x_income_partner_fortnightly)
    
    youth_allowance_payment <- max(0
                                   , (base_rate - 
                                        reduction_personal_income - 
                                        reduction_scholarship_income - 
                                        deeming_reduction - 
                                        reduction_parental_income - 
                                        reduction_partner_income
                                      )/ 14 * 365.25)

    return(youth_allowance_payment)
  }
}


EMTR_youth_allowance <- function(youth_allowance_payment){
  dat <- (youth_allowance_payment-lag(youth_allowance_payment))/custom_step
  dat[is.nan(dat)] <- 0
  dat[is.na(dat)] <- 0
  return(dat)
  
}

# 
# ################################## TEST-inputs #####
# taxable_income <- c(1:200)*1000
# x_income_personal_fortnightly <- taxable_income/365.25*14
# x_age <- 22
# x_dependent <- 1
# x_single <- 1
# x_children <- 0
# x_away_from_home <- 1
# x_pension <- 0
# x_homeowner <- 0
# x_income_partner_fortnightly <- 0
# x_income_parental_annual <- 52000
# x_income_scholarship_annual <- 6000
# x_assets_personal <- 23000
# x_apprenticeship <- 0
# x_completed_y12 <- 1
# x_ft_study <- 1
# 
# youth_allowance(
#     x_income_personal_fortnightly,
#     
#     youth_allowance_rate_data, 
#     youth_allowance_personal_income_test_data,
#     youth_allowance_scholarship_exemption_data,
#     youth_allowance_deeming_rates_data,
#     youth_allowance_parental_income_test_data,
#     youth_allowance_partner_income_test_data,
#     
#     x_income_partner_fortnightly,
#     x_income_parental_annual,
#     x_income_scholarship_annual,
#     x_assets_personal,
#     x_age,
#     
#     x_dependent,
#     x_single,
#     x_children,
#     x_away_from_home,
#     x_pension,
#     x_homeowner,
#     x_apprenticeship,
#     x_completed_y12,
#     x_ft_study
# )
# 
# youth_allowance_payment <- sapply(x_income_personal_fortnightly
#                                   , youth_allowance
#                                   , youth_allowance_rate_data
#                                   , youth_allowance_personal_income_test_data
#                                   , youth_allowance_scholarship_exemption_data
#                                   , youth_allowance_deeming_rates_data
#                                   , youth_allowance_parental_income_test_data
#                                   , youth_allowance_partner_income_test_data
# 
#                                   , x_income_partner_fortnightly
#                                   , x_income_parental_annual
#                                   , x_income_scholarship_annual
#                                   , x_assets_personal
#                                   , x_age
# 
#                                   , x_dependent
#                                   , x_single
#                                   , x_children
#                                   , x_away_from_home
#                                   , x_pension
#                                   , x_homeowner
#                                   , x_apprenticeship
#                                   , x_completed_y12
#                                   , x_ft_study)
# 
# 
# 
# ################################## TEST-charts #####
# source("Functions.R")
# 
# 
# df <- data.frame(taxable_income, takehome + youth_allowance_payment)
# 
# df_payments <- data.frame(taxable_income, lito, lmito, youth_allowance_payment) %>%
#     reshape2::melt(id.vars = "taxable_income")
# 
# 
# df_taxes <- data.frame(taxable_income, income_tax, medicare, mls, hecs) %>%
#     reshape2::melt(id.vars = "taxable_income")
# 
# 
# 
# 
# ggplotly(
#   df_payments() %>%
#     ggplot(aes(x=taxable_income, y=value, colour = variable)) +
#     geom_line() +
#     theme_minimal() +
#     xlab("Taxable Income") +
#     ylab("Payments") +
#     #ylim(-1,1) +
#     xlim(0, 200000)
# )
# 
#   ggplotly(
#     df_taxes() %>%
#       ggplot(aes(x=taxable_income, y=value, colour = variable)) +
#       geom_line() +
#       theme_minimal() +
#       xlab("Taxable Income") +
#       ylab("Taxes") +
#       #ylim(-1,1) +
#       xlim(0, 200000)
#   )
# 
# 
# ggplotly(
#   df() %>%
#     ggplot(aes(x=taxable_income, y=takehome()+ youth_allowance_payment())) +
#     geom_line() +
#     theme_minimal() +
#     xlab("Taxable Income") +
#     ylab("Net Income") +
#     #ylim(-1,1) +
#     xlim(0, 200000)
# )
# 
# 
# 
# 
# 

















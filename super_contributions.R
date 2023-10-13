# joelfindlay 2023.09.08

# binary encoding
# FALSE = 0
# TRUE = 1
# NA = 2

library(readxl)
library(here)
library(dplyr)
library(lubridate)

source("income_taxes.R") ## needs marginal tax rate function

# income_gross <- 112000
# super_rate_employer <- 0.125
# super_cont_vol <- 10000 #######post tax
# super_cont_last5 <- income_gross*super_rate_employer*5*0.8
# super_balance <- 600000
# 
# yr <- year(Sys.Date())
# resident_status <- "resident"


################################################################################ variables ####
# income_gross #from shiny app
# super_rate_data #loads from excel in-file
# 
# super_rate_employer #shiny, optional, assumes equal to current sg
# super_cont_vol #shiny, optional, assumes = 0
# super_cont_last5 #shiny, optional, assumes: income_gross*super_rate_employer*5*0.8
# super_balance #shiny, optional, imputes median balance
# tax_marginal_rate 
# yr #shiny, optional, assumed current year




################################################################################ superannuation #######
super_rate_data <- read_excel(here("taxes.xlsx")
                                 , sheet = "super_contributions"
                                 , skip = 0) %>%
  select(c(1:14)) %>%
  na.omit()


super_total_contributions <- function(income_gross ##compulsory
                                      , super_rate_data ## dataframe
                                      , tax_threshold_data # to calculate marginal tax rate
                                      , yr = year(Sys.Date())
                                      , super_rate_employer = NULL
                                      , super_cont_vol = NULL
                                      , super_cont_last5 = NULL
                                      , super_balance = NULL
                                      , resident_status = "resident"
                                      ) {
  ## find marginal tax rate
  tax_marginal_rate <<- tax_threshold_data %>%
    filter(year == yr
           , residency == resident_status
           , threshold < income_gross) %>%
    pull(rate) %>%
    max()
  
  ### filter super rate data
  tmp <- super_rate_data %>%
    filter(year == yr) 

  ##### pass remaining optional arguments
  if (is.null(super_rate_employer)) {super_rate_employer <- tmp$super_guarantee}
  if (is.null(super_cont_vol)) {super_cont_vol <- tmp$default_vol_cont}
  ## assumes contribution for last 5y at current employer rate using 80% of current gross income
  if (is.null(super_cont_last5)) {super_cont_last5 <- income_gross*super_rate_employer*5*0.8}
  if (is.null(super_balance)) {super_balance <- tmp$default_super_balance}
  
  super_guarantee <<- tmp$super_guarantee
  
  #calculate gross super contributions and reportable super contributions and income for hecs and mls purposes
  super_contributions_employer <<- income_gross*super_rate_employer
  super_contributions_total <<- super_contributions_employer + super_cont_vol
  super_contributions_rep <<- min(super_contributions_employer - income_gross*super_guarantee + super_cont_vol, tmp$cap_annual)
  income_hecs_mls <<- income_gross + super_contributions_rep
  income_gross_incl_super <<- income_gross*(1+super_rate_employer) 


  #return(super_contributions_total)
  #return(super_contributions_rep)
  #return(income_hecs_mls)
  #return(income_gross_incl_super)
  
  
  # calculate concessional and non-concessional super contributions
  super_concessional_carry_limit <<- super_rate_data %>%
    filter(year < yr, year >= yr-5) %>%
    summarise(sum(cap_annual)) %>%
    pull()
  concessional_cap <<- tmp$cap_annual + max(0, super_concessional_carry_limit - super_cont_last5)
  
  super_contributions_concessional <<- min(super_contributions_total, concessional_cap)
  super_contributions_excess <<- max(0, super_contributions_total - concessional_cap)
  

  ## calculate tax payable on superannuation contributions and your net super contribution, includes listo
  
  if(income_gross <= tmp$listo_cap_income) {
    super_listo_refund <<- max(10, min(super_contributions_concessional*tmp$listo_rate, tmp$listo_cap_refund))
  } else {
    super_listo_refund <<- 0
  }
      
  super_contrubtions_tax <<- super_contributions_concessional*tmp$rate +
    super_contributions_excess*tax_marginal_rate -
    super_listo_refund
  
  super_contrubtions_net <<- super_contributions_total - super_contrubtions_tax
  
  #return(super_contrubtions_tax)
  return(super_contrubtions_net)
  
  
  #calculate additional income taxable payable for high income earners (div293), generally payable from finances, not SUPER
  income_gross_div293 <<- income_gross*(1+super_rate_employer) 
  total_income_div293 <<- income_gross*(1+super_rate_employer) + super_cont_vol
  
  if(total_income_div293 >   tmp$cap_div293){
    tax_payable_div293 <<- min(total_income_div293 - tmp$cap_div293, super_contributions_total)*tmp$rate_div293
  } else {
    tax_payable_div293 <<- 0
  }
  
 # return(tax_payable_div293)


  # calculate tax refund resulting from voluntary post-tax super contributions
  #blabla - super_cont_vol
  super_deduction_cap <<- max(concessional_cap - super_contributions_employer, 0)
  super_deduction <<- min(super_deduction_cap, super_cont_vol)
  income_gross_super_deduction <<- income_gross - super_deduction
  

}

#super_total_contributions(income_gross, super_rate_data, tax_marginal_rate)

















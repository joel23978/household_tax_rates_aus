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

# income.gross <- 112000
# super.rate.employer <- 0.125
# super.cont.vol <- 10000 #######post tax
# super.cont.last5 <- income.gross*super.rate.employer*5*0.8
# super.balance <- 600000
# 
# yr <- year(Sys.Date())
# resident_status <- "resident"


################################################################################ variables ####
# income.gross #from shiny app
# super_rate_data #loads from excel in-file
# 
# super.rate.employer #shiny, optional, assumes equal to current sg
# super.cont.vol #shiny, optional, assumes = 0
# super.cont.last5 #shiny, optional, assumes: income.gross*super.rate.employer*5*0.8
# super.balance #shiny, optional, imputes median balance
# tax_marginal_rate 
# yr #shiny, optional, assumed current year




################################################################################ superannuation #######
super_rate_data <- read_excel(here("taxes.xlsx")
                                 , sheet = "super_contributions"
                                 , skip = 0) %>%
  select(c(1:14)) %>%
  na.omit()


super_total_contributions <- function(income.gross ##compulsory
                                      , super_rate_data ## dataframe
                                      , tax_threshold_data # to calculate marginal tax rate
                                      , yr = year(Sys.Date())
                                      , super.rate.employer = NULL
                                      , super.cont.vol = NULL
                                      , super.cont.last5 = NULL
                                      , super.balance = NULL
                                      , resident_status = "resident"
                                      ) {
  ## find marginal tax rate
  tax_marginal_rate <<- tax_threshold_data %>%
    filter(year == yr
           , residency == resident_status
           , threshold < income.gross) %>%
    pull(rate) %>%
    max()
  
  ### filter super rate data
  tmp <- super_rate_data %>%
    filter(year == yr) 

  ##### pass remaining optional arguments
  if (is.null(super.rate.employer)) {super.rate.employer <- tmp$super_guarantee}
  if (is.null(super.cont.vol)) {super.cont.vol <- tmp$default_vol_cont}
  ## assumes contribution for last 5y at current employer rate using 80% of current gross income
  if (is.null(super.cont.last5)) {super.cont.last5 <- income.gross*super.rate.employer*5*0.8}
  if (is.null(super.balance)) {super.balance <- tmp$default_super_balance}
  
  super.guarantee <<- tmp$super_guarantee
  
  #calculate gross super contributions and reportable super contributions and income for hecs and mls purposes
  super.contributions.employer <<- income.gross*super.rate.employer
  super.contributions.total <<- super.contributions.employer + super.cont.vol
  super.contributions.rep <<- min(super.contributions.employer - income.gross*super.guarantee + super.cont.vol, tmp$cap_annual)
  income.hecs.mls <<- income.gross + super.contributions.rep
  income.gross.incl.super <<- income.gross*(1+super.rate.employer) 


  #return(super.contributions.total)
  #return(super.contributions.rep)
  #return(income.hecs.mls)
  #return(income.gross.incl.super)
  
  
  # calculate concessional and non-concessional super contributions
  super.concessional.carry.limit <<- super_rate_data %>%
    filter(year < yr, year >= yr-5) %>%
    summarise(sum(cap_annual)) %>%
    pull()
  concessional.cap <<- tmp$cap_annual + max(0, super.concessional.carry.limit - super.cont.last5)
  
  super.contributions.concessional <<- min(super.contributions.total, concessional.cap)
  super.contributions.excess <<- max(0, super.contributions.total - concessional.cap)
  

  ## calculate tax payable on superannuation contributions and your net super contribution, includes listo
  
  if(income.gross <= tmp$listo_cap_income) {
    super.listo.refund <<- max(10, min(super.contributions.concessional*tmp$listo_rate, tmp$listo_cap_refund))
  } else {
    super.listo.refund <<- 0
  }
      
  super.contrubtions.tax <<- super.contributions.concessional*tmp$rate +
    super.contributions.excess*tax_marginal_rate -
    super.listo.refund
  
  super.contrubtions.net <<- super.contributions.total - super.contrubtions.tax
  
  #return(super.contrubtions.tax)
  return(super.contrubtions.net)
  
  
  #calculate additional income taxable payable for high income earners (div293), generally payable from finances, not SUPER
  income.gross.div293 <<- income.gross*(1+super.rate.employer) 
  total.income.div293 <<- income.gross*(1+super.rate.employer) + super.cont.vol
  
  if(total.income.div293 >   tmp$cap_div293){
    tax_payable_div293 <<- min(total.income.div293 - tmp$cap_div293, super.contributions.total)*tmp$rate_div293
  } else {
    tax_payable_div293 <<- 0
  }
  
 # return(tax_payable_div293)


  # calculate tax refund resulting from voluntary post-tax super contributions
  #blabla - super.cont.vol
  super.deduction.cap <<- max(concessional.cap - super.contributions.employer, 0)
  super.deduction <<- min(super.deduction.cap, super.cont.vol)
  income.gross.super.deduction <<- income.gross - super.deduction
  

}

#super_total_contributions(income.gross, super_rate_data, tax_marginal_rate)

















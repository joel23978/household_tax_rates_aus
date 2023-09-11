# joelfindlay 2023.09.08

# binary encoding
# FALSE = 0
# TRUE = 1
# NA = 2

library(readxl)
library(here)
library(dplyr)
library(lubridate)

# yr <- year(Sys.Date())
# resdent.status <- "resident"
# income.gross <- 101000
# rep.super.contributions <- 10000
# x.single <- 1
# income.hecs.mls <- income.gross + rep.super.contributions
# 




################################################################################ income taxes #######
# need to include impact of concessional super contributions
tax_threshold_data <- read_excel(here("taxes.xlsx")
                                        , sheet = "income_tax"
                                        , skip = 0) %>%
  select(c(1:6))



income_tax <- function(income.gross
                       , tax_threshold_data
                       , yr = year(Sys.Date()) # optional
                       , resident_status = "resident" # optional
                       ) {
  
  tmp <- tax_threshold_data %>%
    filter(year == yr
           , residency == resident_status) 
  
  income_tax_payable <<- 0
  for (i in 1:nrow(tmp)){
    if (income.gross > tmp$threshold[i]) {
      income_tax_payable <<- income_tax_payable +
        min(income.gross-tmp$threshold[i], replace_na(tmp$threshold[i+1]-tmp$threshold[i], Inf))*(tmp$rate[i]+tmp$levy[i]+tmp$repair_levy[i])
    }
  }
  return(income_tax_payable)
}

#income_tax(212000, tax_threshold_data)



################################################################################ marginal_rate #######

marginal_rate <- function(income.gross
                          , tax_threshold_data
                          , yr = year(Sys.Date()) # optional
                          , resident_status = "resident" # optional
                          ) {
  
  tax_marginal_rate <<- tax_threshold_data %>%
    filter(year == yr
           , residency == resident_status
           , threshold < income.gross) %>%
    pull(rate) %>%
    max()

  return(tax_marginal_rate)
}

#marginal_rate(income.gross, tax_threshold_data)

######################################################################################## hecs  #######
hecs_threshold_data <- read_excel(here("taxes.xlsx")
                                 , sheet = "hecs"
                                 , skip = 0) %>%
  select(c(1:3))


hecs_repayment <- function(income.hecs.mls
                           , hecs_threshold_data
                           , yr = year(Sys.Date()) # optional
                           ) {
  
  rate <- hecs_threshold_data %>%
    filter(year == yr
           , threshold <= income.hecs.mls) %>%
    select(rate) %>%
    max()
  
  hecs_payable <<- income.hecs.mls*rate
  return(hecs_payable)
  
}

#hecs_repayment(122000, hecs_threshold_data)




######################################################################################## medicare levy surcharge  #######
mls_threshold_data <- read_excel(here("taxes.xlsx")
                                  , sheet = "medicare_levy_surcharge"
                                  , skip = 0) %>%
  select(c(1:4)) %>%
  mutate_if(is.character, as.logical) %>%
  mutate_all(as.numeric) 


medicare_levy_surcharge <- function(income.hecs.mls
                                    , mls_threshold_data
                                    , yr = year(Sys.Date()) # optional
                                    , x.single = as.numeric(x.single()) # optional, defaults to single
                                    , private.health = 0 # optional, defaults to no private health
                                    ) {
  
  if (private.health ==0){
   rate <- mls_threshold_data %>%
      filter(year == yr
             , single == as.numeric(x.single()) # error hERE for some reason
             , threshold <= income.hecs.mls) %>%
      select(rate) %>%
      max()
    
    mls_payable <<- income.hecs.mls*rate
  
  } else {
    mls_payable <<- 0
  }
  return(mls_payable)
  
}

#medicare_levy_surcharge(211200, mls_threshold_data)




######################################################################################## tax offsets  #######
tax_offsets_data <- read_excel(here("taxes.xlsx")
                                 , sheet = "tax_offsets"
                                 , skip = 0) %>%
  select(c(1:5)) 

# not written
























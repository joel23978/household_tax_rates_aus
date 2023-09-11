# joelfindlay 2023.09.08

# binary encoding
# FALSE = 0
# TRUE = 1
# NA = 2

library(readxl)
library(here)
library(dplyr)
library(lubridate)


fbt_threshold_data <- read_excel(here("taxes.xlsx")
                                 , sheet = "fringe_benefits"
                                 , skip = 0) %>%
  select(c(1:6)) %>%
  slice(1:2)

for (i in 1:nrow(fbt_threshold_data)) {
  if (is.na(fbt_threshold_data$gross_up_rate[i])) {
    fbt_threshold_data$gross_up_rate[i] <- fbt_threshold_data$rate[i] / (1 - fbt_threshold_data$rate[i])
  }
}


fringe_benefits_tax <- function(rep.fringe.benefits.gst, rep.fringe.benefits.nogst, fbt_threshold_data, exempt, yr) {
  
  # Calculate FBT payable for each row in the data frame
  tmp <- fbt_threshold_data %>%
    filter(year == yr)
  
  threshold <- tmp %>% pull(threshold) %>% max()
  
  if (exempt == 1){
    exemption <- tmp %>% pull(exemption) %>% max()
  } else {
    exemption <- 0
  }
  
  type2_rate <- tmp %>% filter(type == "type_2") %>% pull(rate)
  type2_gross <- tmp %>% filter(type == "type_2") %>% pull(gross_up_rate)
  
  type1_rate <- tmp %>% filter(type == "type_1") %>% pull(rate)
  type1_gross <- tmp %>% filter(type == "type_1") %>% pull(gross_up_rate)
  
  rep.fringe.benefits <- rep.fringe.benefits.gst + rep.fringe.benefits.nogst
  fbt_payable <- 0
  
  ### calc type 2
  if (rep.fringe.benefits > threshold) {
    fbt_payable <- max(0
                       , fbt_payable + 
      rep.fringe.benefits.gst*type1_rate*type1_gross +
      rep.fringe.benefits.nogst*type2_rate*type2_gross -
      exemption
    )
  }
  
  return(fbt_payable)
}


#fringe_benefits_tax(rep.fringe.benefits.gst,rep.fringe.benefits.nogst, fbt_threshold_data, exempt, yr)









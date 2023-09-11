#TO DO == Age pension + Carers payment + DSP + Pharmaceutical Allowance + Energy Supplement

## List of functions and variables:


#Post-tax payments
# LITO <- function(taxable_income)
# LMTO <- function(taxable_income)
# 
# tax <- function (taxable_income)
# 
# medicare_levy <- function(taxable_income)
# 
# medicare_levy_surcharge <- function(hecs.mls_income)
# 
# hecs_repayment <- function(hecs.mls_income)
#   
#   
# ####### Payments taxable as income
#
# youth_allowance <- function(age, 
#                             scholarship, 
#                             assessable_income, 
#                             apprenticeship, 
#                             ft_study, 
#                             independent, 
#                             completed_y12, 
#                             away_from_home, 
#                             single, 
#                             children, 
#                             long_term_recipient, 
#                             principal_carer_exempt, 
#                             jobseeker) 












# 
# austudy <- function(single,
#                     age, 
#                     children, 
#                     long_term_recipient, 
#                     scholarship, 
#                     assessable_income)
# 
# job_seeker <- function(age, 
#                        single, 
#                        children, 
#                        nine_months, 
#                        principal_carer, 
#                        mutual_obligation, 
#                        partner_income, 
#                        partner_on_pension) 
#   
# rent_assistance <- function(rent, 
#                             shared_accom, 
#                             single, 
#                             separated_temp, 
#                             separated_illness,
#                             children, 
#                             children_no) 
#   
# adjusted_taxable_income <- taxable_income + investment_losses + reportable_fringe_benefits + reportable_super - deductions
#                 
# family_tax_benefit_A <- function(adjusted_taxable_income, 
#                                  single, 
#                                  children_ages)
# 
# family_tax_benefit_B <- function(children_ages, 
#                                  single, 
#                                  primary_earner_income, 
#                                  secondary_earner_income, 
#                                  adjusted_taxable_income) 
#                     
# parenting_payment <- function(principal_carer, 
#                               single, 
#                               children_ages, 
#                               separated_illness, 
#                               gross_income, 
#                               partner_income, 
#                               partner_centrelink_pension)

  
  
  
  
  
  
################################################################ Tax Offsets 

LITO <- function(taxable_income) {
  if (taxable_income <= 18200) {
    y <- 0
  } else if (taxable_income <= 37500) {
    y <- 700
  } else if (taxable_income <= 45000) {
    y <- (700 - 0.05*(taxable_income-37500))
  } else {
    y <- max(325 - 0.015*(taxable_income-45000), 0)
  }
  # print(y)
}
# LITO(taxable_income)+0



LMTO <- function(taxable_income) {
  if (taxable_income <= 18200) {
    y <- 0
  } else if (taxable_income <= 37000) {
    y <- 255
  } else if (taxable_income <= 48000) {
    y <- (255 + 0.075*(taxable_income-37000))
  } else if (taxable_income <= 90000) {
    y <- 1080
  } else {
    y <- max(1080 - 0.03*(taxable_income-90000), 0)
  }
  # print(y) 
}
# LMTO(taxable_income)+0




# ################################################################ Youth Allowance (At home 18+)
# Youth_Allowance <- function(assessable_income) {
#   max_payment <- 354.6*313/12
#   energy_supplement <- 4.60*313/12
#   step1 <- 437*313/12
#   step2 <- 524*313/12
#   cutoff <- 963.34*313/12
#   if (assessable_income < step1) {
#     reduction <- 0

#   } else if (assessable_income < step2) {
#     reduction <- 0.5*(assessable_income - step1)
#   } else if (assessable_income < cutoff) {
#     reduction <- 0.5*(step2 - step1) + 0.6*(assessable_income - step2)
#   } else {
#     reduction <- max_payment + energy_supplement
#   }
#   y <- assessable_income + energy_supplement + max_payment - reduction
# }
# Youth_Allowance(assessable_income) + 0



################################################################ Income Tax
tax <- function (taxable_income) {
  if (taxable_income <= 18200) {
    tax_paid = 0
    tax_paid_on = 0
    marginal_rate = 0
  } else if (taxable_income <= 45000) {
    tax_paid = 0
    tax_paid_on = 18200
    marginal_rate = 0.19
  } else if (taxable_income <= 120000) {
    tax_paid = 5092
    tax_paid_on = 45000
    marginal_rate = 0.325
  } else if (taxable_income <= 180000) {
    tax_paid = 29467
    tax_paid_on = 120000
    marginal_rate = 0.37
  } else {
    tax_paid = 51667
    tax_paid_on = 180000
    marginal_rate = 0.45
  } 
  y <- (tax_paid + marginal_rate*(taxable_income - tax_paid_on))*(-1)
  # print(y) 
}
# tax(taxable_income)+0

################################################################ Medicare Levy 
### There is an additional reductions for families
medicare_levy <- function(taxable_income) {
  if (taxable_income <= 22801) {
    y <- 0
  } else if (taxable_income <= 28501) {
    y <- (taxable_income-22801)*0.10*(-1)
  } else {
    y <- 0.02*taxable_income*(-1)
  }
}
# medicare_levy(taxable_income)+0



################################################################ Medicare Levy Surcharge
#income for mls == 
# taxable income + net investment losses + reportable super contributions
# reportable super contributions ==
# Employer contribution over 9.5% 
# + salary sacrifice contributions
# + deductible after-tax contributions
# FHSS withdrawal excluded
####

medicare_levy_surcharge <- function(hecs.mls_income, single) {
  mls_ST0 <- 90000
  mls_ST1 <- 105000
  mls_ST2 <- 140000
  if (hecs.mls_income < mls_ST0 | (hecs.mls_income < mls_ST0*2 & single == F)) {mls_rate = 0
  } else if (hecs.mls_income < mls_ST1 | (hecs.mls_income < mls_ST1*2 & single == F)) {mls_rate = 0.01
  } else if (hecs.mls_income < mls_ST2 | (hecs.mls_income < mls_ST2*2 & single == F)) {mls_rate = 0.015
  } else {mls_rate = 0.02
  }
  y <- mls_rate*hecs.mls_income*(-1)
  # print(y)
}
# medicare_levy_surcharge(hecs.mls_income)+0


################################################################ HECS repayment

hecs_repayment <- function(hecs.mls_income) {
  if (hecs.mls_income < 46620){hecs_rr=0
  } else if (hecs.mls_income <= 53826){hecs_rr=0.01
  } else if (hecs.mls_income <= 57055){hecs_rr=0.02
  } else if (hecs.mls_income <= 60479){hecs_rr=0.025
  } else if (hecs.mls_income <= 64108){hecs_rr=0.03
  } else if (hecs.mls_income <= 67954){hecs_rr=0.035
  } else if (hecs.mls_income <= 72031){hecs_rr=0.04
  } else if (hecs.mls_income <= 76534){hecs_rr=0.045
  } else if (hecs.mls_income <= 80935){hecs_rr=0.05
  } else if (hecs.mls_income <= 85972){hecs_rr=0.055
  } else if (hecs.mls_income <= 90939){hecs_rr=0.06
  } else if (hecs.mls_income <= 96396){hecs_rr=0.065
  } else if (hecs.mls_income <= 102179){hecs_rr=0.07
  } else if (hecs.mls_income <= 108309){hecs_rr=0.075
  } else if (hecs.mls_income <= 114707){hecs_rr=0.08
  } else if (hecs.mls_income <= 121698){hecs_rr=0.085
  } else if (hecs.mls_income <= 128999){hecs_rr=0.09
  } else if (hecs.mls_income <= 136739){hecs_rr=0.095
  } else {hecs_rr=0.1
  }
  y <- hecs_rr*hecs.mls_income*(-1)
  # print(y)
}
# hecs_repayment(hecs.mls_income)+0


### This is nearly identical to the actual result, 
# incorrect by $1 in a few spots due to iteratuve rounding for actuals
# x <- 53826
# n <- c(1:16)
# new <- function(x,n)(round(x*1.0600012^n))
# new(x,n)










# Partner income not assessed at this point in either YA or austudy
###################################################################### youth_allowance
## Variables
# apprenticeship <- F
# ft_study <- T
# independent <- T
# completed_y12 <- T
# away_from_home <- F
# single <- T
# children <- F
# long_term_recipient <- F
# jobseeker <- T
# principal_carer_exempt <- T 
# 
# age <- 27
# scholarship <- 0 ## $ Amount
# assessable_income <- 20000

#youth_allowance(22,0, 15000, F, T, T, T, F, T, F, F, F, F)+0


# 
# youth_allowance <- function(age, scholarship, assessable_income, apprenticeship, ft_study, independent
#                             , completed_y12, away_from_home, single, children, long_term_recipient
#                             , principal_carer_exempt, jobseeker
#                             ) {
# #### ELIGIBILITY
# if ((age >= 18 & age <=24)
#     | (age >= 16 & age < 18 & (apprenticeship == T 
#                                | (independent ==T & away_from_home ==T) 
#                                | (ft_study ==T & completed_y12 ==T)
#                                )
#        )
#     ) {
# ## Rates     
#       if (long_term_recipient == F) {
#         if (single == T & children == F & away_from_home == F) {
#               if (age < 18) {
#                 base_rate <- 303.2
#                 energy_supplement <- 3.9
#               } else {
#                 base_rate <- 354.6
#                 energy_supplement <- 4.6
#               }
#         } else if ((single == T & children == F & away_from_home == T)
#             | (single == F & children == F)
#             ) {
#           base_rate <- 512.50
#           energy_supplement <- 7.00
#         } else if (single == T & children == T) {
#           base_rate <- 656
#           energy_supplement <- 9.2
#         } else if (single == F & children == T) {
#           base_rate <- 557.9
#           energy_supplement <- 7.70
#         } else if (single == T & jobseeker == T & principal_carer_exempt ==T) {
#           base_rate <- 850.20
#           energy_supplement <- 12.00
#         }
#       } else if (long_term_recipient == T & age >= 22 & children == F ) { 
#           if (single == T & away_from_home == F) {base_rate <- 423.7
#           } else if (single == T & away_from_home == T) {base_rate <- 611.9
#           } else if (single == F) {base_rate <- 557.9
#           }
#         }
#       }
#   base_rate <- base_rate*313/12 ## Annualise
#   max_reduction <- 0.5*(step2 - step1) + 0.6*(assessable_income - step2)
#   
#   
#   if (jobseeker == F){
#       if (scholarship > 0){assessable_income <- assessable_income - min(scholarship,8355)}
#       
#       step1 <- 437*313/12
#       step2 <- 524*313/12
#       
#       if (assessable_income < step1) {
#         reduction <- 0
#       } else if (assessable_income < step2) {
#         reduction <- 0.5*(assessable_income - step1)
#       } else {
#         if(base_rate > max_reduction) {reduction <- max_reduction
#         } else {reduction <- base_rate + energy_supplement}
#       }
#   } else if (jobseeker == T) {
#     # 50 cents for each dollar of income you have between $150 and $250. 
#     # If your income is over $250, your payment will reduce by 60 cents for each dollar of income over $250.
#     step1 <- 150*313/12
#     step2 <- 250*313/12
#     
#     if (assessable_income < step1) {
#       reduction <- 0
#     } else if (assessable_income < step2) {
#       reduction <- 0.5*(assessable_income - step1)
#     } else {
#       if(base_rate > max_reduction) {reduction <- max_reduction
#       } else {reduction <- base_rate + energy_supplement}
#     }
#   }
#   
# assessable_income <- assessable_income + energy_supplement + base_rate - reduction
#       
# }
# 
# 

######################################################################################## austudy

## Variables
# single <- T
# age <- 27
# #away_from_home <- F
# children <- F
# long_term_recipient <- Fv
austudy <- function(single, age, children, long_term_recipient, scholarship, assessable_income) {
  if (age >= 25) {
    if (age >= 67) {pension_age <-T} else {pension_age <-F}
    
    if (long_term_recipient == F) {
      if (single == T & children ==F) {
        base_rate <- 512.50
        energy_supplement <- 7.00
      } else if (single == T & children == T) {
        base_rate <- 656
        energy_supplement <- 9.2
      } else if (single == F & children == F) {
        base_rate <- 512.50
        energy_supplement <- 7.00
      } else if (single == F & children == T) {
        base_rate <- 557.9
        energy_supplement <- 7.70
      } else { 
        if (single == T & children == F) {base_rate <- 611.9
        } else if (single == F & children == F) {base_rate <- 557.9
        }
      }
    }
    
    base_rate <- base_rate*313/12 ## Annualise
    if (scholarship > 0){assessable_income <- assessable_income - min(scholarship,8355)}
    
    step1 <- 437*313/12
    step2 <- 524*313/12
    max_reduction <- 0.5*(step2 - step1) + 0.6*(assessable_income - step2)
    
    if (assessable_income < step1) {
      reduction <- 0
    } else if (assessable_income < step2) {
      reduction <- 0.5*(assessable_income - step1)
    } else {
      if(base_rate > max_reduction) {reduction <- max_reduction
      } else {reduction <- base_rate + energy_supplement}
    }
    y <- assessable_income + energy_supplement + base_rate - reduction
  }
}
########################################################################################








######################################################################################## jobseeker
# age
# single
# children
# nine_months <- T
# principal_carer <- T
# mutual_obligation <- F
# partner_income <- 1150 ## fortnightly
# partner_on_pension <- F

job_seeker <- function(age, single, children, nine_months, principal_carer, mutual_obligation
                       , partner_income, partner_on_pension) {
  
if (age >= 22 & age < 67) {
  
  if (single == T & children == F & age < 60) {
    base_rate <- 620.8
  } else if ((single == T & age >= 60 & nine_months == T) 
             | (single == T & children = T)) {
    base_rate <- 667.5
  } else if (single == T & principal_carer == T & mutual_obligation == F) {
    base_rate <- 850.20
  } else if (single == F){
    base_rate <- 565.40
  }

  
  if ((single == T & principal_carer == F)
      |(single == F)){
        max_reduction <- 0.5*(step2 - step1) + 0.6*(assessable_income - step2)
        step1 <- 150*313/12
        step2 <- 250*313/12
        if (assessable_income < step1) {
          reduction <- 0
        } else if (assessable_income < step2) {
          reduction <- 0.5*(assessable_income - step1)
        } else {
          if(base_rate > max_reduction) {reduction <- max_reduction
          } else {reduction <- base_rate}
        }
  } else if (single == T & principal_carer == T) {
        max_reduction <- 0.4*(assessable_income - step1)
        step1 <- 150*313/12
        if (assessable_income < step1) {
          reduction <- 0
        } else {
          if(base_rate > max_reduction) {reduction <- max_reduction
          } else {reduction <- base_rate}
        }
  } 
  
  if (single == F & partner_on_pension == F){
    reduction <- reduction + max(0.4*(partner_income - 1124)*313/12, 0)
    if (reduction > base_rate){reduction <- base_rate}
  }}
}
  
 

  
  



  
  


######################################################################################## rent_assistance

# rent <- 200 #FORTNIGHT
# shared_accom <- F
# single <- T
# separated_illness <- F
# separated_temp <- F
# children <- F
# children_no <- F

rent_assistance <- function(rent, shared_accom, single, separated_temp, separated_illness,
                            children, children_no) {
if (children == F){
  
  min <- 125.8
  if (rent > min){
    if ((single ==T & shared_accom ==F)
        | (single ==F & separated_illness == T)) {
      rent_assistance <- min(0.75*(rent - min), 140.80)
    } else if (single ==T & shared_accom ==T){
      rent_assistance <- min(0.75*(rent - min), 93.87)
    } else if (single ==F & separated_temp == T){
      rent_assistance <- min(0.75*(rent - min), 140.80)
    } else if (single ==F){
      rent_assistance <- min(0.75*(rent - min), 132.80)
    }
  }
  
  min <- 203.60
  if (rent > min){
    if (single ==F & separated_illness == T){
      rent_assistance <- min(0.75*(rent - min), 132.80)
    }
  }

} else {
  
  min <- 244.16
  if (rent > min){
    if (single ==F & children_no <=2){
      rent_assistance <- min(0.75*(rent - min), 165.62)
    } else if (single ==F & children_no >=3){
      rent_assistance <- min(0.75*(rent - min), 187.04)
    }
  }
  
  min <- 165.06	
  if (rent > min){
    if (children_no <=2 & (separated_illness == T | separated_temp == T)){
      rent_assistance <- min(0.75*(rent - min), 165.62)
    } else if (children_no >=3 & (separated_illness == T | separated_temp == T)){
      rent_assistance <- min(0.75*(rent - min), 187.04)
    }
  }
    
}
  
}
  








######################################################################################## family_tax_benefit_A

### ignoring children in approved care atm
### ignoring the effect of child support
### ignoring kids from past relationships
### not applicable if less than 65% care
### ignoring foreign income
### ignores special treatment for triplets & quadruplets

### Variables:
# adjusted_taxable_income
# single 
# triplets
# quadruplets
# children_ages <- c(1,13,17) ###Input as a vector

#adjusted_taxable_income <- taxable_income + investment_losses + reportable_fringe_benefits + reportable_super - deductions
  
family_tax_benefit_A <- function(adjusted_taxable_income, single, children_ages){
    
  children_under_12 <- length(children_ages[children_ages <=12])
  children_13to15 <- length(children_ages[children_ages > 12 &  children_ages <=15])
  children_16to19 <- length(children_ages[children_ages > 15 &  children_ages <=19])

  max_rate <- (children_under_12*189.56 + children_13to15*246.54 + children_16to19*246.54)*313/12
  base_rate <- 60.9*no_children*313/12
  supplement_rate <- 781.10
  
# FTB at the base rate
  if ((children_no == 1 & (children_under_12 == 1 & adjusted_taxable_income >= 72398)
       | (children_under_12 == 0 & adjusted_taxable_income >= 79826))
      (children_no == 2 & (children_under_12 == 2 & adjusted_taxable_income >= 89170)
        | (children_under_12 <=1 & adjusted_taxable_income >= 96958))
  ) {
    rate <- base_rate
  } else {
    rate <- max_rate
  }
  
#FTB using deduction from the maximum rate
  if (adjusted_taxable_income <= 55626){
    ftb_a <- rate
  } else if (adjusted_taxable_income <= 98988){
    ftb_a <- rate - (adjusted_taxable_income - 55626)*0.20
  } else if (adjusted_taxable_income > 98988){
    ftb_a <- max(rate - (98988 - 55626)*0.20 - (adjusted_taxable_income - 98988)*0.3, 0)
  }
  

# income limits by ## of children
  if ((children_no == 1 & adjusted_taxable_income >= 104281)
      | (children_no == 2 & ((children_13to15 + children_16to19 == 2 & adjusted_taxable_income >= 112931)
                             | (adjusted_taxable_income >= 109573))
         )
      | (children_no == 3 & ((children_under_12 == 3 & adjusted_taxable_income >= 119501)
                             | (children_under_12 == 2 & adjusted_taxable_income >= 124453)
                             | (children_under_12 == 1 & adjusted_taxable_income >= 129405)
                             | (children_under_12 == 0 & adjusted_taxable_income >= 134357))
      )
      | (children_no == 4 & ((children_under_12 >= 3 & adjusted_taxable_income >= 140927)
                             | (children_under_12 == 2 & adjusted_taxable_income >= 145879)
                             | (children_under_12 <= 1 & adjusted_taxable_income >= 150831))
      )
      | (children_no == 5 & ((children_under_12 >= 3 & adjusted_taxable_income >= 162352)
                             | (children_under_12 <= 2 & adjusted_taxable_income >= 167304))
      )
      | (children_no == 6 & adjusted_taxable_income >= 183778)
  ){
    ftb_a <- 0
  }
      
## FTB A Supplement
  if (adjusted_taxable_income <= 80000){
    ftb_a_supplement <- 781.10*children_no
  } else {
    ftb_a_supplement <- 0
  }
   
  family_tax_benefit_A <- ftb_a + ftb_a_supplement
}
  
  
  
  
  

  
##################################################################### Family tax benefit part B  
## Analysis ignores grandparent carers

### Variables
# children_ages
# single <- F
# primary_earner_income <- 99000
# secondary_earner_income <- 22338
# adjusted_taxable_income <- 104000
  
family_tax_benefit_B <- function(children_ages, single, primary_earner_income, secondary_earner_income, adjusted_taxable_income) {
  
  
  if (min(children_ages) <=5) {
    ftb_b <- 161.14*313/12
  } else if (min(children_ages) <=18) {
    ftb_b <- 112.56*313/12
  }

##
  
### Secondary earner incomce test
if (single == F) {
  if (secondary_earner_income >= 5767) {
    ftb_b <- max(ftb_b - (secondary_earner_income - 5767)*0.2, 0)
  }
}

# 28,671 a year, if the youngest child is younger than 5
# $22,338 a year, if the youngest child is 5 to 13.


# Cutoff points
  if ((min(children_ages) <13) & ((single == T & adjusted_taxable_income > 100000)
                                  | (single == F & primary_earner_income > 100000))
  ) {
    ftb_b <- 0
  } 
  
  
## FTB B Supplement
  if (adjusted_taxable_income <= 80000){
    ftb_b_supplement <- 379.6
  } else {
    ftb_b_supplement <- 0
  }

  family_tax_benefit_B <- ftb_b + ftb_b_supplement
}




####################################################################### parenting payment (taxable)
#### Variables
# principal_carer
# single
# children_ages #### input vector
# separated_illness
# gross_income <- 1000
# partner_income <- 20000
# partner_centrelink_pension <- F

### not clear if all children have to be below cut-off age (or if just 1 child)
### not applicable if your partner gets Austudy or Youth Allowance as their fortnightly income limit is different.

parenting_payment <- function(principal_carer, single, children_ages, separated_illness
                              , gross_income, partner_income, partner_centrelink_pension){
if (principal_carer == T){

### maximum payment rates and reduction points
  if (single == T & min(children_ages) < 8){
    children_no <- count(children_ages[children_ages < 8])
    max_rate <- 850.20*313/12
    
    ### Income test
    if (children_no ==1){
      reduction_point <- 192.6*313/12
    } else if (children_no ==2){
      reduction_point <- 217.2*313/12
    } else if (children_no ==3){
      reduction_point <- 241.8*313/12
    } else if (children_no >=3){
      reduction_point <- (241.8 + (children_no-3)*24.6)*313/12
    }
    
  } else if (single == F & min(children_ages) < 6) {
    children_no <- count(children_ages[children_ages < 8])
    
    if (separated_illness == F){
      max_rate <- 565.40*313/12
    } else {
      max_rate <- 667.50*313/12
    }
    
  }
  
  
### actual rates (post-deductions)
  if (single == T){
  parenting_payment <- max(max_rate - (gross_income - reduction_point)*0.4, 0)
  
} else if (single == F & partner_centrelink_pension == F){
  step1 <- 150*313/12
  step2 <- 256*313/12
  step1_partner <- 1124*313/12
  
  ## Actual payment
  if (gross_income < step2) {
  parenting_payment <- max_rate - 
                             max((gross_income - step1)*0.5, 0) - 
                             max((partner_income - step1_partner)*0.6, 0)
  } else {
    parenting_payment <- max_rate - 
                               (step2 - step1)*0.5 - 
                               max((gross_income - step2)*0.6, 0) - 
                               max((partner_income - step1_partner)*0.6, 0)
  }
                             
} else if (single == F & partner_centrelink_pension == T){
  combined_income <- gross_income + partner_income
  step1 <- 300*313/12
  step2 <- 512*313/12
  
  ## Actual payment
  if (combined_income < step2) {
    reduction <- max(0.25*(combined_income - step1), 0)
  } else {
    reduction <- 0.25*(step2 - step1) + 0.3*(combined_income - step2)
  }
  parenting_payment <- max_rate - reduction
  
}
}
}







################################################################ Net income:  

Combined <- function(taxable_income, hecs.mls_income, single) {
  y <- taxable_income +
    tax(taxable_income) +
    medicare_levy(taxable_income) +
    medicare_levy_surcharge(hecs.mls_income, single) +
    hecs_repayment(hecs.mls_income) +
    LITO(taxable_income) +
    LMTO(taxable_income)
  # print(y)
}


################################################################ Take-home income:  
takehome_fortnight <- function(taxable_income) {
  y <- (taxable_income +
          tax(taxable_income) +
          medicare_levy(taxable_income) +
          hecs_repayment(taxable_income)
  )*12/313
}



################################################################ Total tax/offsets:  

Tax_due <- function(taxable_income, hecs.mls_income) {
  y <- tax(taxable_income) +
    medicare_levy(taxable_income) +
    medicare_levy_surcharge(hecs.mls_income, single) +
    hecs_repayment(hecs.mls_income) +
    LITO(taxable_income) +
    LMTO(taxable_income)
  # print(y)
}






############################################################### EMTR's

EMTR <- function(taxable_income, hecs.mls_income, single){
  z <- 1-(Combined(taxable_income+1, hecs.mls_income+1, single)-Combined(taxable_income, hecs.mls_income, single))
  # print(z)
}
# EMTR(taxable_income, hecs.mls_income)+0

EMTR_income_tax <- function(taxable_income){
  z <- tax(taxable_income)-tax(taxable_income+1)
}
# EMTR_income_tax(taxable_income)+0

EMTR_medicare_levy <- function(taxable_income){
  z <- medicare_levy(taxable_income)-medicare_levy(taxable_income+1)
}
# EMTR_medicare_levy(taxable_income)+0

EMTR_hecs_repayment <- function(hecs.mls_income){
  z <- hecs_repayment(hecs.mls_income)-hecs_repayment(hecs.mls_income+1)
}
# EMTR_hecs_repayment(hecs.mls_income)+0

EMTR_medicare_levy_surcharge <- function(hecs.mls_income, single){
  z <- medicare_levy_surcharge(hecs.mls_income, single)-medicare_levy_surcharge(hecs.mls_income+1, single)
}
# EMTR_medicare_levy_surcharge(hecs.mls_income)+0

EMTR_LITO <- function(taxable_income){
  z <- LITO(taxable_income)-LITO(taxable_income+1)
}
# EMTR_LITO(taxable_income)+0

EMTR_LMTO <- function(taxable_income){
  z <- LMTO(taxable_income)-LMTO(taxable_income+1)
}
# EMTR_LMTO(taxable_income)+0

EMTR_austudy <- function(single, age, children, long_term_recipient, scholarship, assessable_income){
  z <- austudy(single, age, children, long_term_recipient, scholarship, assessable_income) -
    austudy(single, age, children, long_term_recipient, scholarship, assessable_income+1)
}



EMTR_family_tax_benefit_A <- function(adjusted_taxable_income, single, children_ages){
  z <- family_tax_benefit_A(adjusted_taxable_income, single, children_ages) - 
    family_tax_benefit_A(adjusted_taxable_income+1, single, children_ages)
}

EMTR_family_tax_benefit_B <- function(children_ages, single, primary_earner_income
                                      , secondary_earner_income, adjusted_taxable_income){
  z <- family_tax_benefit_B(children_ages, single, primary_earner_income
                            , secondary_earner_income, adjusted_taxable_income) - 
    family_tax_benefit_B(children_ages, single, primary_earner_income
                         , secondary_earner_income, adjusted_taxable_income+1)
}

# EMTR_job_seeker <- function(age, single, children, nine_months, principal_carer
#                             , mutual_obligation, partner_income, partner_on_pension){
#   z <- job_seeker(age, single, children, nine_months, principal_carer
#                    , mutual_obligation, partner_income, partner_on_pension) - 
#     job_seeker(age, single, children, nine_months, principal_carer
#                , mutual_obligation, partner_income, partner_on_pension)
# }

# EMTR_rent_assistance <- function(rent, shared_accom, single, separated_temp, separated_illness
#                          ,children, children_no){
#   
# }

# EMTR_parenting_payment <- function(principal_carer, single, children_ages, separated_illness
#                                    , gross_income, partner_income, partner_centrelink_pension){
#   
# }

EMTR_youth_allowance <- function(age, scholarship, assessable_income, apprenticeship, ft_study, independent
                                 , completed_y12, away_from_home, single, children, long_term_recipient
                                 , principal_carer_exempt, jobseeker){
  z <- youth_allowance(age, scholarship, assessable_income, apprenticeship, ft_study, independent
                       , completed_y12, away_from_home, single, children, long_term_recipient
                       , principal_carer_exempt, jobseeker) - 
    youth_allowance(age, scholarship, assessable_income+1, apprenticeship, ft_study, independent
                    , completed_y12, away_from_home, single, children, long_term_recipient
                    , principal_carer_exempt, jobseeker)
  
}




################################################################ VECTORISE FUNCTIONS

Combined1 <- Vectorize(Combined)
EMTR1 <- Vectorize(EMTR)
tax1 <- Vectorize(tax)
medicare_levy1 <- Vectorize(medicare_levy)
hecs_repayment1 <- Vectorize(hecs_repayment)
medicare_levy_surcharge1 <- Vectorize(medicare_levy_surcharge)
LITO1 <- Vectorize(LITO)
LMTO1 <- Vectorize(LMTO)
#Youth_Allowance1 <- Vectorize(Youth_Allowance)
austudy1 <- Vectorize(austudy)

Tax_due1 <- Vectorize(Tax_due)

EMTR_income_tax1 <- Vectorize(EMTR_income_tax)
EMTR_medicare_levy1 <- Vectorize(EMTR_medicare_levy)
EMTR_hecs_repayment1 <- Vectorize(EMTR_hecs_repayment)
EMTR_medicare_levy_surcharge1 <- Vectorize(EMTR_medicare_levy_surcharge)
EMTR_LITO1 <- Vectorize(EMTR_LITO)
EMTR_LMTO1 <- Vectorize(EMTR_LMTO)
EMTR_austudy1 <- Vectorize(EMTR_austudy)





################################################################ Total tax/offsets:  

Tax_due1 <- function(taxable_income, hecs.mls_income, single) {
  y <- tax1(taxable_income) +
    medicare_levy1(taxable_income) +
    medicare_levy_surcharge1(hecs.mls_income, single) +
    hecs_repayment1(hecs.mls_income) +
    LITO1(taxable_income) +
    LMTO1(taxable_income)
}
# Tax_due1(taxable_income, hecs.mls_income)+0










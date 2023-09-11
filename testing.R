
# 
# youth_allowance_payment <- reactive({
#   # Check if "youth.allowance" is selected
#   if (input$youth.allowance == TRUE) {
#     sapply(
#       x_income_personal_fortnightly(),
#       youth_allowance,
#       youth_allowance_rate_data,
#       youth_allowance_personal_income_test_data,
#       youth_allowance_scholarship_exemption_data,
#       youth_allowance_deeming_rates_data,
#       youth_allowance_parental_income_test_data,
#       youth_allowance_partner_income_test_data,
#       x_income_partner_fortnightly(),
#       x_income_parental_annual(),
#       x_income_scholarship_annual(),
#       x_assets_personal(),
#       x_age(),
#       x_dependent(),
#       x_single(),
#       x_children(),
#       x_away_from_home(),
#       x_pension(),
#       x_homeowner(),
#       x_apprenticeship(),
#       x_completed_y12(),
#       x_ft_study()
#     )
#   } else {
#     rep(0, times = no_steps)
#   }
# })
# 
# # Check if "youth.allowance" is selected
# if (input$youth.allowance == T) {
#   youth_allowance_payment_value <- sapply(x_income_personal_fortnightly
#            , youth_allowance
#            , youth_allowance_rate_data
#            , youth_allowance_personal_income_test_data
#            , youth_allowance_scholarship_exemption_data
#            , youth_allowance_deeming_rates_data
#            , youth_allowance_parental_income_test_data
#            , youth_allowance_partner_income_test_data
#            
#            , x_income_partner_fortnightly
#            , x_income_parental_annual
#            , x_income_scholarship_annual
#            , x_assets_personal
#            , x_age
#            
#            , x_dependent
#            , x_single
#            , x_children
#            , x_away_from_home
#            , x_pension
#            , x_homeowner
#            , x_apprenticeship
#            , x_completed_y12
#            , x_ft_study)
#   
#   youth_allowance_payment(youth_allowance_payment_value) 
#   
# } else {
#   youth_allowance_payment <- rep(c(0),times=no_steps)
# }








# 
# observeEvent(input$youth.allowance, {
#   # Check if "youth.allowance" is selected
#   if (input$youth.allowance == T) {
#     youth_allowance_payment <- reactive({
#       sapply(x_income_personal_fortnightly
#              , youth_allowance
#              , youth_allowance_rate_data
#              , youth_allowance_personal_income_test_data
#              , youth_allowance_scholarship_exemption_data
#              , youth_allowance_deeming_rates_data
#              , youth_allowance_parental_income_test_data
#              , youth_allowance_partner_income_test_data
# 
#              , x_income_partner_fortnightly
#              , x_income_parental_annual
#              , x_income_scholarship_annual
#              , x_assets_personal
#              , x_age
# 
#              , x_dependent
#              , x_single
#              , x_children
#              , x_away_from_home
#              , x_pension
#              , x_homeowner
#              , x_apprenticeship
#              , x_completed_y12
#              , x_ft_study)
#     })
# 
#     if (all(class(youth_allowance_payment) == "numeric")) {
#       # All elements are numeric
#     } else {
#       # Not all elements are numeric
#     }
# 
#   } else {
#     youth_allowance_payment <- rep(c(0),times=no_steps)
#   }
# })

# 
# 
#     # Watch for changes in the "Youth Allowance" checkbox
#     youth_allowance_payment <- reactive({
#       if (input$youth.allowance == T) {
#         youth_allowance_payment <- sapply(x_income_personal_fortnightly
#                  , youth_allowance
#                  , youth_allowance_rate_data
#                  , youth_allowance_personal_income_test_data
#                  , youth_allowance_scholarship_exemption_data
#                  , youth_allowance_deeming_rates_data
#                  , youth_allowance_parental_income_test_data
#                  , youth_allowance_partner_income_test_data
#                  
#                  , x_income_partner_fortnightly
#                  , x_income_parental_annual
#                  , x_income_scholarship_annual
#                  , x_assets_personal
#                  , x_age
#                  
#                  , x_dependent
#                  , x_single
#                  , x_children
#                  , x_away_from_home
#                  , x_pension
#                  , x_homeowner
#                  , x_apprenticeship
#                  , x_completed_y12
#                  , x_ft_study)
#       } 
#     })
# 
#     




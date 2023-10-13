function(input, output, session){
  
  library(here)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(lubridate)
  
# issues with mls calcand hecs needs to be optional
  
  
  ################################################################################ DEFINED ELEMENTS ##########

  income_max <- 200000
  custom_step <- 1000  # You can adjust this to your desired step size
  income_min <- custom_step
  
  income_taxable <- seq(income_min, income_max, by = custom_step)
  x_income_personal_fortnightly <- income_taxable/365.25*14
  no_steps <- length(income_taxable)
  
  net_investment_loss <- 0
  rep_fringe_benefits <- 0 
  rep_super_contributions <- 0
  
  yr <- year(Sys.Date())
  resdent_status <- "resident"
  
  
  observe({
    source("youth_allowance.R")
    source("income_taxes.R")
    
    
    
    
    
    
    
    ################################################################################## REACTIVE ELEMENTS ####################
    # Create a reactive object to check the presence of variables in input$variables
    variable_check <- reactive({
      var_list <- c("x_dependent", "x_ft_study", "x_completed_y12", "x_apprenticeship", 
                    "x_children", "x_away_from_home", "x_homeowner", "x_pension")
      sapply(var_list, function(var) var %in% input$variables, USE.NAMES = FALSE)
    })
    
    # Use the variable_check object for calculations
    x_dependent <- reactive({ as.numeric(variable_check()[1]) })
    x_ft_study <- reactive({ as.numeric(variable_check()[2]) })
    x_completed_y12 <- reactive({ as.numeric(variable_check()[3]) })
    x_apprenticeship <- reactive({ as.numeric(variable_check()[4]) })
    x_children <- reactive({ as.numeric(variable_check()[5]) })
    x_away_from_home <- reactive({ as.numeric(variable_check()[6]) })
    x.homeowner <- reactive({ as.numeric(variable_check()[7]) })
    x.pension <- reactive({ as.numeric(variable_check()[8]) })
    
    x_single <- reactive(as.numeric(input$x_single))
    x_age <- reactive(as.numeric(input$x_age))
    x_income_scholarship_annual <- reactive(as.numeric(input$x_income_scholarship_annual))
    x_income_parental_annual <- reactive(as.numeric(input$x_income_parental_annual))
    x_income_partner_fortnightly <- reactive(as.numeric(input$x_income_partner_fortnightly))
    x_income_partner_annual <- reactive(as.numeric(input$x_income_partner_annual))
    x_assets_personal <- reactive(as.numeric(input$x_assets_personal))
    private_health <- reactive(as.numeric(as.logical(input$private_health)))
    hecs_debt <- reactive(as.numeric(as.logical(input$hecs_debt)))
    
    net_investment_loss <- reactive(as.numeric(input$net_investment_loss))
    rep_fringe_benefits <- reactive(as.numeric(input$rep_fringe_benefits))
    super_cont_vol <- reactive(as.numeric(input$super_cont_vol))
    

    
  
    
    
    
    ################################################################################ income tax #####
    income_hecs_mls <- reactive(income_taxable + net_investment_loss + rep_fringe_benefits + rep_super_contributions)
  
    income_tax_payable <- sapply(income_taxable, income.tax, tax_threshold_data, yr, resdent_status)
    hecs_payable <- sapply(income_taxable, hecs.repayment, hecs_threshold_data, yr, hecs_debt())
    mls_payable <- sapply(income_taxable, medicare.levy.surcharge
                          , mls_threshold_data, yr, x_single(), private_health(), x_income_partner_annual())

    df <- data.frame(income_taxable, income_tax_payable, hecs_payable, mls_payable) %>%
      mutate(income_net = income_taxable - income_tax_payable - hecs_payable - mls_payable
             , gross = income_taxable) %>%
      pivot_longer(!gross)
    
    
    
    
    
    
    
    
    ################################################################################ youth allowance #####
    # 
    # # Initialize youth_allowance_payment and emtr_youth_allowance
    # youth_allowance_payment <- reactiveVal(NULL)  # Initialize as NULL
    # 
    # observeEvent(input$youth_allowance, {
    #   if (input$youth_allowance) {
    #     result <- sapply(x_income_personal_fortnightly,
    #                      youth_allowance,
    #                      youth_allowance_rate_data,
    #                      youth_allowance_personal_income_test_data,
    #                      youth_allowance_scholarship_exemption_data,
    #                      youth_allowance_deeming_rates_data,
    #                      youth_allowance_parental_income_test_data,
    #                      youth_allowance_partner_income_test_data,
    #                      x_income_partner_fortnightly,
    #                      x_income_parental_annual,
    #                      x_income_scholarship_annual,
    #                      x_assets_personal,
    #                      x_age,
    #                      x_dependent,
    #                      x_single,
    #                      x_children,
    #                      x_away_from_home,
    #                      x_pension,
    #                      x_homeowner,
    #                      x_apprenticeship,
    #                      x_completed_y12,
    #                      x_ft_study
    #     )
    #   } else {
    #     result <- rep(0, times = no_steps)
    #   }
    #   
    #   youth_allowance_payment <- youth_allowance_payment(result)
    # 
    #   print(youth_allowance_payment())
    #   str(youth_allowance_payment())
    # })
    # 
    # 
    # 

    
   
    ################################################################################## PRETTY PLOTS ####################
    output$income_plot <- renderPlotly({ggplotly(
      df %>% 
        ggplot(aes(x=gross, y=value, colour = name)) +
        geom_line() +
        theme_minimal() +
        xlab("Gross Income") +
        ylab("Net Income") +
        xlim(income_min, income_max)
    )})
    
    
    

    
  })
}










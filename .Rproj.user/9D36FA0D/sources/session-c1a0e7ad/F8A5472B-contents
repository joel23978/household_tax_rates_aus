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
  
  net.investment.loss <- 0
  rep.fringe.benefits <- 0 
  rep.super.contributions <- 0
  
  yr <- year(Sys.Date())
  resdent.status <- "resident"
  
  
  observe({
    source("youth_allowance.R")
    source("income_taxes.R")
    
    
    
    
    
    
    
    ################################################################################## REACTIVE ELEMENTS ####################
    # Create a reactive object to check the presence of variables in input$variables
    variable_check <- reactive({
      var_list <- c("x.dependent", "x.ft.study", "x.completed.y12", "x.apprenticeship", 
                    "x.children", "x.away.from.home", "x.homeowner", "x.pension")
      sapply(var_list, function(var) var %in% input$variables, USE.NAMES = FALSE)
    })
    
    # Use the variable_check object for calculations
    x.dependent <- reactive({ as.numeric(variable_check()[1]) })
    x.ft.study <- reactive({ as.numeric(variable_check()[2]) })
    x.completed.y12 <- reactive({ as.numeric(variable_check()[3]) })
    x.apprenticeship <- reactive({ as.numeric(variable_check()[4]) })
    x.children <- reactive({ as.numeric(variable_check()[5]) })
    x.away.from.home <- reactive({ as.numeric(variable_check()[6]) })
    x.homeowner <- reactive({ as.numeric(variable_check()[7]) })
    x.pension <- reactive({ as.numeric(variable_check()[8]) })
    
    x.single <- reactive(as.numeric(input$x.single))
    x.age <- reactive(as.numeric(input$x.age))
    x.income.scholarship.annual <- reactive(as.numeric(input$x.income.scholarship.annual))
    x_income.parental.annual <- reactive(as.numeric(input$x_income.parental.annual))
    x.income.partner.fortnightly <- reactive(as.numeric(input$x.income.partner.fortnightly))
    x.assets.personal <- reactive(as.numeric(input$x.assets.personal))
    
    net.investment.loss <- reactive(as.numeric(input$net.investment.loss))
    rep.fringe.benefits <- reactive(as.numeric(input$rep.fringe.benefits))
    super.cont.vol <- reactive(as.numeric(input$super.cont.vol))
    

    
  
    
    
    
    ################################################################################ income tax #####
    income.hecs.mls <- reactive(income_taxable + net.investment.loss + rep.fringe.benefits + rep.super.contributions)
  
    income_tax_payable <- sapply(income_taxable, income_tax, tax_threshold_data, yr, resdent.status)
    hecs_payable <- sapply(income_taxable, hecs_repayment, hecs_threshold_data, yr)
    mls_payable <- sapply(income_taxable, medicare_levy_surcharge, mls_threshold_data, yr, x.single)
    
    df <- data.frame(income_taxable, income_tax_payable, hecs_payable, mls_payable) %>%
      mutate(income_net = income_taxable - income_tax_payable - hecs_payable - mls_payable
             , gross = income_taxable) %>%
      pivot_longer(!gross)
    
    
    
    
    
    
    
    
    ################################################################################ youth allowance #####
    # 
    # # Initialize youth_allowance_payment and emtr_youth_allowance
    # youth_allowance_payment <- reactiveVal(NULL)  # Initialize as NULL
    # 
    # observeEvent(input$youth.allowance, {
    #   if (input$youth.allowance) {
    #     result <- sapply(x_income_personal_fortnightly,
    #                      youth_allowance,
    #                      youth_allowance_rate_data,
    #                      youth_allowance_personal_income_test_data,
    #                      youth_allowance_scholarship_exemption_data,
    #                      youth_allowance_deeming_rates_data,
    #                      youth_allowance_parental_income_test_data,
    #                      youth_allowance_partner_income_test_data,
    #                      x.income.partner.fortnightly,
    #                      x.income.parental.annual,
    #                      x.income.scholarship.annual,
    #                      x.assets.personal,
    #                      x.age,
    #                      x.dependent,
    #                      x.single,
    #                      x.children,
    #                      x.away.from.home,
    #                      x.pension,
    #                      x.homeowner,
    #                      x.apprenticeship,
    #                      x.completed.y12,
    #                      x.ft.study
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










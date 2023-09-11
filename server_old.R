function(input, output, session){
  
  library(here)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  


    
    ############################################################ Payments ##########
    
    ##### DEFINED ELEMENTS
    # Define the start, end, and custom step
    max_income <- 200000
    custom_step <- 1000  # You can adjust this to your desired step size
    min_income <- custom_step
    
    taxable_income <- seq(min_income, max_income, by = custom_step)
    x_income_personal_fortnightly <- taxable_income/365.25*14
    no_steps <- length(taxable_income)
    
    
    
    observe({
      source("Functions.R")
      source("youth_allowance.R")
      
    ##### REACTIVE ELEMENTS
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
    x_homeowner <- reactive({ as.numeric(variable_check()[7]) })
    x_pension <- reactive({ as.numeric(variable_check()[8]) })
    x_single <- reactive(as.numeric({input$x_single}))
    
    age <- reactive({input$x_age})
    x_age <- reactive(as.numeric(input$x_age))
    x_income_scholarship_annual <- reactive(as.numeric(input$x_income_scholarship_annual))
    x_income_parental_annual <- reactive(as.numeric(input$x_income_parental_annual))
    x_income_partner_fortnightly <- reactive(as.numeric(input$x_income_partner_fortnightly))
    x_assets_personal <- reactive(as.numeric(input$x_assets_personal))
    
    investment_loss <- reactive(as.numeric(input$net_investment_loss))
    fringe_benefits <- reactive(as.numeric(input$rep_fringe_benefits))
    super <- reactive(as.numeric(input$rep_super))
    
    
    #### youth allowance #####
    
    # Initialize youth_allowance_payment and emtr_youth_allowance
    youth_allowance_payment <- reactiveVal(NULL)  # Initialize as NULL
    emtr_youth_allowance <- reactiveVal(NULL)
    
    
    observeEvent(input$youth.allowance, {
      if (input$youth.allowance) {
        result <- sapply(x_income_personal_fortnightly,
                         youth_allowance,
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
        )
      } else {
        result <- rep(0, times = no_steps)
      }
      
      youth_allowance_payment <- youth_allowance_payment(result)
      emtr_youth_allowance <- emtr_youth_allowance(result)
      
      print(youth_allowance_payment())
      str(youth_allowance_payment())
    })
    
    
    
    income_tax <- reactive({tax1(taxable_income+youth_allowance_payment())})
    lito <- reactive({LITO1(taxable_income+youth_allowance_payment())})
    
    
    medicare <- reactive({medicare_levy1(taxable_income+youth_allowance_payment())})
    lmito <- reactive({LMTO1(taxable_income+youth_allowance_payment())})
    
    hecs.mls_income <- reactive({taxable_income + 
        investment_loss() + 
        fringe_benefits() + 
        super() })
    
    mls <- reactive({medicare_levy_surcharge1(hecs.mls_income(), x_single())})
    hecs <- reactive({
      if (input$hecs.debt == T){
        hecs <- hecs_repayment1(hecs.mls_income())
      } else {
        hecs <- rep(c(0),times=no_steps)
      }
    })
    takehome.excl.hecs <- reactive({Combined1(taxable_income, hecs.mls_income(), x_single())})
    
    takehome <- reactive({takehome.excl.hecs() + hecs() + youth_allowance_payment()})
    
    
    
    
    df <- reactive({
      data.frame(taxable_income, takehome() 
      )})
    
    df_payments <- reactive({
      data.frame(taxable_income, lito(), lmito(), youth_allowance_payment()) %>%
        reshape2::melt(id.vars = "taxable_income")
    })
    
    df_taxes <- reactive({
      data.frame(taxable_income, income_tax(), medicare(), mls(), hecs()) %>%
        reshape2::melt(id.vars = "taxable_income")
    })
    
    
    output$payments_plot <- renderPlotly({
      ggplotly(
        df_payments() %>%
          ggplot(aes(x=taxable_income, y=value, colour = variable)) +
          geom_line() +
          theme_minimal() +
          xlab("Taxable Income") +
          ylab("Payments") +
          #ylim(-1,1) +
          xlim(min_income, max_income)
      )})
    
    
    output$taxes_plot <- renderPlotly({ ##
      ggplotly(
        df_taxes() %>%
          ggplot(aes(x=taxable_income, y=value, colour = variable)) +
          geom_line() +
          theme_minimal() +
          xlab("Taxable Income") +
          ylab("Taxes") +
          #ylim(-1,1) +
          xlim(min_income, max_income)
      )})
    
    
    output$takehome_plot <- renderPlotly({ggplotly(
      df() %>% 
        ggplot(aes(x=taxable_income, y=takehome())) +
        geom_line() +
        theme_minimal() +
        xlab("Taxable Income") +
        ylab("Net Income") +
        #ylim(-1,1) +
        xlim(min_income, max_income)
    )})
    
    #})
    
    
    
    
    
    
    
    
    
    
    
    
    
    ############################################################ EMTR's ##########
    
    ##### DEFINED ELEMENTS
    emtr_income_tax <- EMTR_income_tax1(taxable_income)
    emtr_medicare <- EMTR_medicare_levy1(taxable_income)
    emtr_lito <- EMTR_LITO1(taxable_income)
    emtr_lmito <- EMTR_LMTO1(taxable_income)
    
    
    
    
    
    
    #### REACTIVE ELEMENTS
    emtr_mls <- reactive(EMTR_medicare_levy_surcharge1(hecs.mls_income(), x_single()))
    emtr_hecs <- reactive({
      if (input$hecs.debt == F){
        emtr_hecs <- EMTR_hecs_repayment1(hecs.mls_income())
      } else {
        emtr_hecs <- rep(c(0),times=no_steps)
      }
    })
    

    

    emtr.excl.hecs <- reactive(EMTR1(taxable_income, hecs.mls_income(), x_single()))
    emtr <- reactive({emtr.excl.hecs() - emtr_hecs() - emtr_youth_allowance()})
    df_emtr <- reactive({data.frame(taxable_income, emtr())})
    
    df_emtr_payments <- reactive({
      data.frame(taxable_income, emtr_lito, emtr_lmito, emtr_youth_allowance()) %>%
        reshape2::melt(id.vars = "taxable_income")
    })
    
    df_emtr_taxes <- reactive({
      data.frame(taxable_income, emtr_income_tax, emtr_medicare, emtr_mls(), emtr_hecs()) %>%
        reshape2::melt(id.vars = "taxable_income")
    })
    
    output$emtr_payments_plot <- renderPlotly({
      ggplotly(
        df_emtr_payments() %>%
          ggplot(aes(x=taxable_income, y=value, colour = variable)) +
          geom_line() +
          theme_minimal() +
          xlab("Taxable Income") +
          ylab("Payments") +
          ylim(-0.2,0.2) +
          xlim(min_income, max_income)
      )})
    
    
    output$emtr_taxes_plot <- renderPlotly({
      ggplotly(
        df_emtr_taxes() %>%
          ggplot(aes(x=taxable_income, y=value, colour = variable)) +
          geom_line() +
          theme_minimal() +
          xlab("Taxable Income") +
          ylab("Taxes") +
          ylim(0,0.5) +
          xlim(min_income, max_income)
      )})
    
    
    output$emtr_takehome_plot <- renderPlotly({ggplotly(
      df_emtr() %>% 
        ggplot(aes(x=taxable_income, y=emtr())) +
        geom_line() +
        # ggplot() +
        # geom_line(aes(x=taxable_income, y=emtr_mod, color = "Effective Marginal Tax Rate")) +
        # geom_line(aes(x=taxable_income, y=cum_tax_rate, color = "Cumulative Tax Rate")) +
        # geom_text(aes(x=taxable_income, y=emtr_mod, label = emtr_mod2)
        #           , vjust = 10) +
        theme_minimal() +
        xlab("Taxable Income") +
        ylab("Net Income") +
        ylim(0,0.8) +
        xlim(min_income, max_income)
    )})
    
    
    output$youth_allowance_payment_data <- renderPrint({
      data.frame(youth_allowance_payment = youth_allowance_payment())
    })
    
    
  })
}



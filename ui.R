library(plotly)
navbarPage(
  "Taxes and Transfers",
  tabPanel(
    "Taxes",
    fluidPage(
      sidebarPanel(
        h3("Variables")
        , checkboxInput("hecs_debt"
                        , label = ("HECS-HELP Debt")
                        , FALSE
        )
        , checkboxInput("private_health"
                        , label = ("Private Health Cover")
                        , FALSE
        )
        , helpText("Private health cover affects Medicare Levy Surcharge")
        , selectInput("x_single"
                      , label = "Relationship Status"
                      , choices = list("Single" = 1
                                       , "Couple" = 0
                                       , "Separated temporarily" = 2
                                       , "Separated due to illness" = 3)
                      , selected = 1
        )
        , helpText("Note: Relationship status affects medicare levy surcharge and welfare payments")
        , conditionalPanel(
          condition = "input.x_single == '0'"
          , numericInput("x_income_partner_annual"
                         , label = "Partner Income (Annual)"
                         , value = 0)
          , helpText("Note: To determine Medicare levy surcharge")
          
        )
        , selectInput("resident_status"
                      , label = "Residency Status"
                      , choices = list("Resident" = 0
                                       , "Foreign Resident" = 1
                                       , "Working Holiday Maker" = 2)
                      , selected = 0)
        , helpText("Note: Residency status determines income tax rates")
        
        
        , h3("Superannuation")
        , numericInput("super_rate_employer"
                       , label = ("Employer Superannuation Rate (%)")
                       , value = 10.5)
        , helpText("Minimum = 10.5")
        , numericInput("super_cont_vol"
                       , label = ("Voluntary Super Contributions (Annual)")
                       , value = 0)
        , numericInput("super_cont_last5"
                       , label = ("Total Super Contributions (Last 5 yrs)")
                       , value = 0)
        , numericInput("super_balance"
                       , label = ("Superannuation Balance")
                       , value = 100000)
        , helpText("Note: The above includes post-tax contributions 
                                         which you claim a tax deduction on.")
        , numericInput("net_investment_loss"
                       , label = ("Net Investment Loss (Annual)")
                       , value = 0)
        , numericInput("rep_fringe_benefits"
                       , label = ("Reportable Fringe Benefits (Annual)")
                       , value = 0)
        , helpText("Note: Income for the purposes of HECS-HELP repayments and the medicare levy surchare include reportable super contributions and reportable superannuation contributions.")
      
        , h3("App Settings")
        , numericInput("income_step"
                       , label = ("Income Step for Calcs")
                       , value = 1000)
        , numericInput("income_max"
                       , label = ("Max Income Modelled")
                       , value = 200000)
        # , numericInput("yr"
        #                , label = ("Tax Year")
        #                , value = 2023)
        )
      
      
      , mainPanel(
        ## Basic Income and taxes chart
        h2("Income and Taxes")
        , plotlyOutput("income_plot")
        , helpText("Note: We model incomes in $1000 increments of gross income (excl. superannuation and fringe benefits) from $1,000 to $200,000.")
        
        
        ## EMTR faced at each $1000 incrememnt
        , h2("Effective Marginal Tax Rate")
        , plotlyOutput("emtr_plot")
        , helpText("Note: We model EMTRs in $1000 increments from $1,000 to $200,000 in the following manner (net income @ 72k - net income @ 71k)/1000.")
        
        
        ## super contributions
        , h2("Superannuation Contributions")
        , plotlyOutput("super_plot")
        
      )
      
      
      
      
      
    )
  )
  
  , tabPanel(
    "Offsets",
    fluidPage(
      
      column(3
             , h2("Offsets")
             , helpText("Note: Not functional at current**.")
             
             , checkboxInput("beneficiary_tax_ofset", label = "Beneficiary Offset", FALSE)
             , checkboxInput("senior_pensioners_tax_offset", label = "Senior and Pensioners Offset", FALSE)
             , checkboxInput("invalid_tax_offset", label = "Invalid Offset", FALSE)
             , checkboxInput("carer_tax_offset", label = "Carer Offset", FALSE)
                     
    )

    , column(3
             , h2("Criteria") #### this whole section wants to be dynamic
             , helpText("This section needs to be dynamic.")
             
             
             
    )
    
    , column(6
             , h2("Take-home Income")
             #, plotlyOutput("income_plot")
             
             , h2("Effective Marginal Tax Rate")
             #, plotlyOutput("income_plot")
    )
    
    )
  )
    
    
  , tabPanel(
    "Transfers",
    fluidPage(column(3
               , h2("Transfers")
               , helpText("Note: Currently functional for Youth Allowance only**.")
               
               , checkboxInput("youth_allowance", label = "Youth Allowance", FALSE)
               , checkboxInput("jobseeker", label = "Jobseeker", FALSE)
               , checkboxInput("aus_study", label = "Austudy", FALSE)
               , checkboxInput("ftb_a", label = "Family Tax Benefit A", FALSE)
               , checkboxInput("ftb_b", label = "Family Tax Benefit B", FALSE)
               , checkboxInput("parenting_payment", label = "Parenting Payment", FALSE)
               , checkboxInput("rent_assist", label = "Rent Assistance", FALSE)
               , checkboxInput("carers_payment", label = "Carers Payment", FALSE)
               , checkboxInput("age_pension", label = "Age Pension", FALSE)
               , checkboxInput("dsp", label = "Disability Support Pension", FALSE)
               
               
      )
      
      
      
      , column(3
               , h2("Eligibility/Rate Criteria") #### this whole section wants to be dynamic
               , helpText("This section needs to be dynamic.")
               
               , sliderInput("x_age"
                             , label = "Age"
                             , min = 0
                             , max = 100
                             , value = 22)
               # , sliderInput("children"
               #               , label = "Children"
               #               , min = 0
               #               , max = 10
               #               , value = 0)
               # , CHILDREN AGES (CONDITIONAL)
               , checkboxGroupInput("variables"
                                    , label = ("Variables")
                                    , choiceNames = list("On Jobseeker for 9+ months" 
                                                         , "Mutual Obligations" 
                                                         , "Partner recieving pension" 
                                                         , "Dependent (on parents)" 
                                                         , "Have children" 
                                                         , "Living away from home" 
                                                         , "Homeowner" 
                                                         , "Apprentice" 
                                                         , "Completed Year 12" 
                                                         , "Undertaking full-time study")
                                    , choiceValues = list("x_jobseeker"
                                                          , "x_mutual"
                                                          , "x_pension"
                                                          , "x_dependent"
                                                          , "x_children"
                                                          , "x_away_from_home"
                                                          , "x_homeowner"
                                                          , "x_apprenticeship"
                                                          , "x_completed_y12"
                                                          , "x_ft_study")
                                    , selected = "connect")
               
               , numericInput("x_income_scholarship_annual"
                              , label = ("Scholarship Income (Annual)")
                              , value = 0)
               , numericInput("x_income_parental_annual"
                              , label = ("Parental Income (Annual)")
                              , value = 0)
               , numericInput("x_income_partner_fortnightly"
                              , label = ("Partner Income (Fortnightly)")
                              , value = 0)
               , numericInput("x_assets_personal"
                              , label = ("Personal Financial Assets")
                              , value = 0)
               
               
      )
      
      , column(6
               , h2("Take-home Income")
               #, plotlyOutput("income_plot")

               , h2("Effective Marginal Tax Rate")
               #, plotlyOutput("income_plot")
      )

    )
  )
  
  
  # 
  # , tabPanel(
  #   "EMTR's",
  #   fluidPage(
  #     h2("Take-home EMTR")
  #     , plotlyOutput("emtr_takehome_plot")
  #     
  #     , h2("Taxes EMTR")
  #     , plotlyOutput("emtr_taxes_plot")
  #     
  #     , h2("Payments EMTR")
  #     , plotlyOutput("emtr_payments_plot")
  #   )
  # )
  , collapsible = TRUE

)

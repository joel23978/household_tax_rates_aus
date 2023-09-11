library(plotly)
navbarPage(
  "Taxes and Transfers",
  tabPanel(
    "Taxes",
    fluidPage(
      column(3
             , h2("Inputs")
             , checkboxInput("hecs.debt"
                             , label = ("HECS-HELP Debt")
                             , FALSE
             )
             , checkboxInput("private.health"
                             , label = ("Private Health Cover")
                             , FALSE
             )
             , helpText("Private health cover affects Medicare Levy Surcharge")
             , selectInput("x.single"
                           , label = "Relationship Status"
                           , choices = list("Single" = 0
                                            , "Couple" = 1
                                            , "Separated temporarily" = 2
                                            , "Separated due to illness" = 3)
                           , selected = 0)
             , helpText("Note: Relationship status affects medicare levy surcharge and welfare payments")
                          , selectInput("resident.status"
                           , label = "Residency Status"
                           , choices = list("Resident" = 0
                                            , "Foreign Resident" = 1
                                            , "Working Holiday Maker" = 2)
                           , selected = 0)
             , helpText("Note: Residency status determines income tax rates")
             
             , numericInput("super.rate.employer"
                            , label = ("Employer Superannuation Rate (%)")
                            , value = 11)
             , helpText("Minimum = 11")
             , numericInput("super.cont.vol"
                            , label = ("Voluntary Super Contributions (Annual)")
                            , value = 0)
             , numericInput("super.cont.last5"
                            , label = ("Total Super Contributions (Last 5 yrs)")
                            , value = 0)
             , numericInput("super.balance"
                            , label = ("Superannuation Balance")
                            , value = 100000)
             , helpText("Note: The above includes post-tax contributions 
                                         which you claim a tax deduction on.")
             , numericInput("net.investment.loss"
                            , label = ("Net Investment Loss (Annual)")
                            , value = 0)
             , numericInput("rep.fringe.benefits"
                            , label = ("Reportable Fringe Benefits (Annual)")
                            , value = 0)
             , helpText("Note: Income for the purposes of HECS-HELP repayments and the medicare levy surchare include reportable super contributions and reportable superannuation contributions.")
      )
      
      , column(9
               , h2("Income and Taxes")
               , plotlyOutput("income_plot")
               , helpText("Note: We model incomes in $1000 increments of gross income (excl. superannuation and fringe benefits) from $1,000 to $200,000.")
               
               , h2("Effective Marginal Tax Rate")
               
               , helpText("Note: We model EMTRs in $1000 increments from $1,000 to $200,000 in the following manner (net income @ 72k - net income @ 71k)/1000.")
               
               
               , h2("Superannuation Contributions")

               #, plotlyOutput("income_plot")
               
      )
      
      
      
      
      
    )
  )
  
  , tabPanel(
    "Offsets",
    fluidPage(
      
      column(3
             , h2("Offsets")
             , helpText("Note: Not functional at current**.")
             
             , checkboxInput("beneficiary.tax.ofset", label = "Beneficiary Offset", FALSE)
             , checkboxInput("senior.pensioners.tax.offset", label = "Senior and Pensioners Offset", FALSE)
             , checkboxInput("invalid.tax.offset", label = "Invalid Offset", FALSE)
             , checkboxInput("carer.tax.offset", label = "Carer Offset", FALSE)
                     
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
               
               , checkboxInput("youth.allowance", label = "Youth Allowance", FALSE)
               , checkboxInput("jobseeker", label = "Jobseeker", FALSE)
               , checkboxInput("aus.study", label = "Austudy", FALSE)
               , checkboxInput("ftb.a", label = "Family Tax Benefit A", FALSE)
               , checkboxInput("ftb.b", label = "Family Tax Benefit B", FALSE)
               , checkboxInput("parenting.payment", label = "Parenting Payment", FALSE)
               , checkboxInput("rent.assist", label = "Rent Assistance", FALSE)
               , checkboxInput("carers.payment", label = "Carers Payment", FALSE)
               , checkboxInput("age.pension", label = "Age Pension", FALSE)
               , checkboxInput("dsp", label = "Disability Support Pension", FALSE)
               
               
      )
      
      
      
      , column(3
               , h2("Eligibility/Rate Criteria") #### this whole section wants to be dynamic
               , helpText("This section needs to be dynamic.")
               
               , sliderInput("x.age"
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
                                    , choiceValues = list("x.jobseeker"
                                                          , "x.mutual"
                                                          , "x.pension"
                                                          , "x.dependent"
                                                          , "x.children"
                                                          , "x.away.from.home"
                                                          , "x.homeowner"
                                                          , "x.apprenticeship"
                                                          , "x.completed.y12"
                                                          , "x.ft.study")
                                    , selected = "connect")
               
               , numericInput("x.income.scholarship.annual"
                              , label = ("Scholarship Income (Annual)")
                              , value = 0)
               , numericInput("x.income.parental.annual"
                              , label = ("Parental Income (Annual)")
                              , value = 0)
               , numericInput("x.income.partner.fortnightly"
                              , label = ("Partner Income (Fortnightly)")
                              , value = 0)
               , numericInput("x.assets.personal"
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

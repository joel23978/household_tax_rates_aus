library(plotly)
navbarPage(
  "Australian Taxation System",
  tabPanel(
    "Eligibility",
    fluidPage(
      column(4
             , h2("Complusory Inputs")
             , checkboxInput("hecs.debt"
                                  , label = ("HECS-HELP Debt")
                                  , FALSE
                                  )
             , selectInput("x_single"
                           , label = "Relationship Status"
                           , choices = list("Single" = 0
                                            , "Couple" = 1
                                            , "Separated temporarily" = 2
                                            , "Separated due to illness" = 3)
                           , selected = 1)
             , helpText("Note: Relationship status affects medicare levy surcharge")
             , numericInput("net_investment_loss"
                            , label = ("Net Investment Loss (Annual)")
                            , value = 0)
             , numericInput("rep_fringe_benefits"
                            , label = ("Reportable Fringe Benefits (Annual)")
                            , value = 0)
             , numericInput("rep_super"
                            , label = ("Reportable Super Contributions (Annual)")
                            , value = 0)
             , helpText("Note: This includes post-tax contributions 
                                         which you claim a tax deduction on.")
             , helpText("Note: Other variables are needed to compute income for 
                        HECS-HELP repayments and the medicare levy surcharge")
             
             
      )
      
      , column(4
               , h2("Select Payments")
               , checkboxInput("jobseeker", label = "Jobseeker", FALSE)
               , checkboxInput("youth.allowance", label = "Youth Allowance", FALSE)
               , checkboxInput("aus.study", label = "Austudy", FALSE)
               , checkboxInput("ftb.a", label = "Family Tax Benefit A", FALSE)
               , checkboxInput("ftb.b", label = "Family Tax Benefit B", FALSE)
               , checkboxInput("parenting.payment", label = "Parenting Payment", FALSE)
               , checkboxInput("rent.assist", label = "Rent Assistance", FALSE)
               , checkboxInput("carers.payment", label = "Carers Payment", FALSE)
               , checkboxInput("age.pension", label = "Age Pension", FALSE)
               , checkboxInput("dsp", label = "Disability Support Pension", FALSE)
      )
      
      
      
      , column(4
               , h2("Eligibility")
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
        
        , helpText("Note: Currently functional for Youth Allowance only.")
 
      )
      
      
      
      
      
      
      
    
      
      # selectInput("single",
      #             , label = "Relationship Status"
      #             , choices = list("Single" = 1
      #                              , "Couple" = 2
      #                              , "Separated temporarily" = 3
      #                              , "Separated due to illness" = 4)
      #             , selected = 1)
      # , sliderInput("age"
      #             , label = "Age"
      #             , min = 0
      #             , max = 100
      #             , value = 22)
      # , sliderInput("children"
      #               , label = "Children"
      #               , min = 0
      #               , max = 10
      #               , value = 0)
      # 
      # , checkboxGroupInput("long_term_recipient"
      #                      , label = ("Long Term Recipient")
      #                      , choices = list("True" = T,
      #                                     "False" = F),
      #                      selected = 1)
      # # , checkboxGroupInput("variables"
      # #                      , label = ("Questions:")
      # #                      , choices = list("Long Term Recipient" = 1, 
      # #                                       "Children" = 2),
      # #                      selected = 1)
      # , numericInput("scholarship"
      #                , label = ("Scholarship (Annual)")
      #                , value = 0)

      )
  )
  , tabPanel(
    "Payments",
    fluidPage(
      h2("Take-home Income")
      , plotlyOutput("takehome_plot")
      
      , h2("Taxes")
      , plotlyOutput("taxes_plot")
      
      , h2("Payments")
      , plotlyOutput("payments_plot")
      
      , verbatimTextOutput("youth_allowance_payment_data") ######### temp

    )
  )
  
  
  
  , tabPanel(
    "EMTR's",
    fluidPage(
      h2("Take-home EMTR")
      , plotlyOutput("emtr_takehome_plot")
      
      , h2("Taxes EMTR")
      , plotlyOutput("emtr_taxes_plot")
      
      , h2("Payments EMTR")
      , plotlyOutput("emtr_payments_plot")
    )
  )
  , collapsible = TRUE
)

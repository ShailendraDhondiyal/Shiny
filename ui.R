fluidPage(
  titlePanel("Economic Freedom Index: 2014-2019"),
  
  navbarPage("Navbar",
 
   tabPanel("About",
            sidebarLayout(
              sidebarPanel(
                 fluidPage(
                  h4('Economic Freedom Index Parameters'),
                  strong('Rule of law'),
                  tags$ul(
                    tags$li('Property Rights'),
                    tags$li('Judicial Effectiveness'),
                    tags$li('Government Integrity')
                  ),
                  strong('Government size'),
                  tags$ul(
                    tags$li('Tax Burden'),
                    tags$li('Government Spending'),
                    tags$li('Fiscal Health')
                  ),                  
                  strong('Regulatory efficiency'),
                  tags$ul(
                    tags$li('Business Freedom'),
                    tags$li('Labor Freedom'),
                    tags$li('Monetary Freedom')
                  ),
                  strong('Market openness'),
                  tags$ul(
                    tags$li('Trade Freedom'),
                    tags$li('Investment Freedom'),
                    tags$li('Financial Freedom')
                  )
                 )
              ,width = 3),
              
              
              mainPanel(
                p('Economic freedom or economic liberty is the ability of people of a society to take economic actions.'),
                p('The free market viewpoint defines economic liberty as the freedom to produce, trade and consume any goods and services acquired without the use of force, fraud or theft. This is embodied in the rule of law, property rights and freedom of contract, and characterized by external and internal openness of the markets, the protection of property rights and freedom of economic initiative.'),
                p('There are several indices of economic freedom that attempt to measure free market economic freedom. Some of these are:- Index of Economic Freedom (Heritage Foundation and Wall Street Journal), Economic Freedom of the World (Fraser Institute) and World Survey of Economic Freedom (Freedom House).'),
                hr(),
                p('I wanted to observe how the economic freedom has changed in last 6 years in different countries and in the same period how has the economic growth moved. I also wanted to observe if the performance on different economic freedom parameters is good on its own, or if it is also linked to the economic status.'),
                p('For my study, I used the Index of Economic Freedom created by the Heritage Foundation and Wall Street Journal. The Index focuses on four key aspects of the economic and entrepreneurial
environment over which governments typically exercise policy control:- 1) Rule of law, 2) Government size, 3) Regulatory efficiency and 4) Market openness'),
                p('In assessing conditions in these four categories, the Index measures 12 specific components of economic freedom, each of which is graded on a scale from 0 to 100. Scores on these 12 components of economic freedom, which are calculated from a number of sub-variables, are equally weighted and averaged to produce an overall economic freedom score for each economy.'),
                hr()
                
              ) # mainPanel
              
            ) # sidebarLayout
   ),             
             
    tabPanel("Country Comparison",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = 'country1', label = 'Country 1',
                             choices = unique(df$Country.Name), selected = df$Country.Name[1]),
                 selectInput(inputId = 'country2', label = 'Country 2',
                             choices = unique(df$Country.Name), selected = df$Country.Name[1])
                 , width = 3),


               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel(title = "Rank/Score",
                                      
                                      fluidRow(
                                           column(6, plotOutput("rank1")),
                                           column(6, plotOutput("index11"))
                                       ),
                                      fluidRow(
                                           column(6, plotOutput("rank2")),
                                           column(6, plotOutput("index12"))
                                      )
                             ),
                             tabPanel(title = "Components of Index",
                                      fluidRow(
                                        column(8, plotOutput("components1", height = "450px")),
                                        column(4, plotOutput("categories1"))
                                      ),

                                      fluidRow(
                                        column(8, plotOutput("components2", height = "450px")),
                                        column(4, plotOutput("categories2"))
                                      )

                             ),
                             tabPanel(title = "Score/GDP(per Capita)",
                                      fluidRow(
                                        column(6, plotOutput("index31")),
                                        column(6, plotOutput("pcgdp1"))
                                      ),
                                      fluidRow(
                                        column(6, plotOutput("index32")),
                                        column(6, plotOutput("pcgdp2"))
                                     )

                             ) 
                 ) # tabsetPanel
               ) # mainPanel

             ) # sidebarLayout
    ), # tabPanel (upper)
    
    tabPanel("Income Quartile Comparison",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "radio", label = h3("Plot Type"),
                              choices = c("box-plot", "mosaic-plot"))
               , width = 3),
               
               
               mainPanel(
                 fluidRow(
                   column(6, plotOutput("r1c1")),
                   column(6, plotOutput("r1c2"))
                 ),
                 fluidRow(
                   column(6, plotOutput("r2c1")),
                   column(6, plotOutput("r2c2"))
                 )
                 
               ) # mainPanel
               
             ) # sidebarLayout
    ), # tabPanel (middle)
   
     
    tabPanel("Economic Impact",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = 'yvar', label = 'economic indicator', choices = resp_var),
                 selectInput(inputId = 'xvar', label = 'economic freedom parameter', choices = pred_var)
                 
                , width = 3), 

               mainPanel(
                   plotOutput("cormap", width = "95%"),
                   verbatimTextOutput("lmsummary")

               ) # mainPanel
               
             ) # sidebarLayout
    ) # tabPanel (upper)

    
  ) # navbarPage
) # fluidpage
  
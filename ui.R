dashboardPage(
  dashboardHeader(title = "Anova"),
  dashboardSidebar( sidebarMenu(
    menuItem("ANOVA", tabName = "anova", icon = icon("search dollar",lib = "font-awesome"))
  )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "anova",
              fluidPage(
                box(
                  fileInput("file1", "Choose CSV File",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  tags$hr(),
                  
                  checkboxInput("header", "Header", TRUE),
                  
                  radioButtons("sep", "Separator",
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = ","),
                  radioButtons("quote", "Quote",
                               choices = c(None = "",
                                           "Double Quote" = '"',
                                           "Single Quote" = "'"),
                               selected = '"'),
                  tags$hr(),
                  conditionalPanel(
                    condition = "output.contents",
                    selectInput("response", "Choose the response variable ",choices=names(df)),
                    conditionalPanel(
                      condition= "input.response!=NULL",
                      
                      selectInput("categorical", "Choose the categorical predictors",choices=names(df),multiple = T)
                    ),
                    selectInput("interacoes", "escolha o número de interações",choices = 1)
                    
                    
                  )
                ),
                box(
                  htmlOutput('anova')
                ),
                box(
                  dataTableOutput('contents')
                ),
                box(
                  plotOutput('interacao3')
                ),
                box(
                  plotOutput('interacao2')
                ),
                box(
                  plotOutput('autoplot')
                )
                
              )
      )
    )
  )
)



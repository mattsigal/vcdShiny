library(shiny)
library(shinydashboard)

dashboardPage(
    # THEME AND GLOBAL OPTIONS
    skin = "green",
    
    # DASHBOARD HEADER
    dashboardHeader(title = "shinyVCD"),
    
    # DASHBOARD SIDEBAR
    dashboardSidebar(
        sidebarMenu(
            menuItem("Initialization", tabName = "init", icon = icon("dashboard")),
            menuItem("Models", tabName = "models", icon = icon("check-square")),
            menuItem("Mosaic Plot", tabName = "mosaic", icon = icon("line-chart")),
            menuItem("Descriptive Statistics", tabName = "stats", icon = icon("table")),
            menuItem("About this Dashboard", tabName = "help", icon = icon("question-circle"))
        )
    ),
    
    # DASHBOARD BODY
    dashboardBody(
        tabItems(
            # INITIALIZATION TAB
            tabItem(tabName = "init",
                    fluidRow(
                        column(12, 
                               uiOutput("choose_dataset")),
                        column(12,
                               conditionalPanel(
                                   condition = "input.dataset == 'Upload a CSV data file'",
                                   uiOutput("upload_dataset")
                               )),
                        column(12,
                               checkboxInput("freq",
                                            label = "Is the dataset in frequency form?",
                                            value = FALSE)),
                        column(12,
                               conditionalPanel(
                                   condition = "input.freq == true",
                                   textInput("freqVar", 
                                             label = "Name of Frequency variable:",
                                             value = "Freq"))),
                        column(12,
                               uiOutput("choose_columns")),
                        column(12,
                               selectInput("type", 
                                           label = "Visualize expected or observed values?", 
                                           choices = c("observed", "expected"))),
                        column(12, 
                               checkboxInput("shading", 
                                             label = "Add shading?"))
                        )),
            
            # MOSAIC PLOT TAB
            tabItem(tabName = "mosaic",
                    h2("Mosaic Plot"),
                    fluidRow(
                        box(width = 12, 
                            plotOutput("mosaic", height = "600px")))
            ),
            
            # MODEL TAB
            tabItem(tabName = "models",
                    h2("Model Statistics"),
                    fluidRow(
                        box(width = 12, 
                            verbatimTextOutput("anova")))
            ),
            
            # STATISTICS TAB
            tabItem(tabName = "stats",
                    h2("Descriptive Statistics"),
                    fluidRow(
                        box(width = 12, 
                            verbatimTextOutput("xtabs"),
                            tableOutput("summary")))
            ),
            
            # HELP TAB
            tabItem(tabName = "help",
                    h2("About this App")
            )
        )
    )
)

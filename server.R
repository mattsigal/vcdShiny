library(shiny)
library(shinydashboard)
library(vcd)
library(vcdExtra)

data_sets <- c("Choose from the following options...",
               "Upload a CSV data file",
               "Abortion Opinion Data",
               "HairEyeSex Data",
               "Employment Status Data",
               "Pre-Marital Sex and Divorce Data",
               "Titanic Data",
               "Berkeley Admission Data",
               "Infection in Cesarean Births Data",
               "Suicide Data",
               "Heart Disease Data")

shinyServer(function(input, output, session) {
    
    output$choose_dataset <- renderUI({
        selectInput("dataset", "Choose data set:", as.list(data_sets))
    })
    
    output$upload_dataset <- renderUI({
        fileInput('file1', 'Choose file to upload',
                  multiple = FALSE,
                  accept = c('text/csv',
                             'text/comma-separated-values',
                             '.csv'))
    })
    
    output$choose_columns <- renderUI({
        if(is.null(input$dataset))
            return("\n")
        
        dname <- switch(input$dataset,
                        "Choose from the following options..." = NULL,
                        "Upload a CSV file" = "choose",
                        "Abortion Opinion Data" = "Abortion",
                        "HairEyeSex Data" = "HairEyeColor",
                        "Employment Status Data" = "Employment",
                        "Pre-Marital Sex and Divorce Data" = "PreSex",
                        "Titanic Data" = "Titanic",
                        "Berkeley Admission Data" = "UCBAdmissions",
                        "Infection in Cesarean Births Data" = "Caesar",
                        "Suicide Data" = "Suicide",
                        "Heart Disease Data" = "Heart")
        
        if(is.null(dname))
            return("\n")
        
        if (dname == "choose"){
            inFile <- input$file1
            if (is.null(inFile))
                return(NULL)
            dat <- read.csv(inFile$datapath)
        } else dat <- get(dname)
        
        if (class(dat) == "table") {
            dat <- expand.dft(dat)
        }
        colnames <- names(dat)
        
        checkboxGroupInput("columns", 
                           label = "Variables to Include:", 
                           choices  = colnames,
                           selected = colnames)
    })
    
    data_set <- reactive({
        if(is.null(input$dataset))
            return("\n")
        
        dname <- switch(input$dataset,
                        "Choose from the following options..." = NULL,
                        "Upload a CSV file" = "choose",
                        "Abortion Opinion Data" = "Abortion",
                        "HairEyeSex Data" = "HairEyeColor",
                        "Employment Status Data" = "Employment",
                        "Pre-Marital Sex and Divorce Data" = "PreSex",
                        "Titanic Data" = "Titanic",
                        "Berkeley Admission Data" = "UCBAdmissions",
                        "Infection in Cesarean Births Data" = "Caesar",
                        "Suicide Data" = "Suicide",
                        "Heart Disease Data" = "Heart")
        
        if(is.null(dname))
            return()
        
        if (dname == "choose"){
            inFile <- input$file1
            if (is.null(inFile))
                return(NULL)
            dat <- read.csv(inFile$datapath)
        } else dat <- get(dname)
        
        if (class(dat) == "table") {
            dat <- expand.dft(dat)
        }
        
        if (is.null(input$columns) || !(input$columns %in% names(dat)))
            return()
        
        dat <- dat[, input$columns, drop = FALSE]
    })

    output$xtabs <- renderPrint({
        if(is.null(input$dataset))
            return("\n")
        
        rs <- paste(input$columns, collapse = "+")
        
        if (input$freq == TRUE) {
            f <- as.formula(paste(input$freqVar, "~", rs))
        } else f <- as.formula(paste("~", rs))
        print(xtabs(f, data = data_set()))
    })
    
    output$summary <- renderTable({
        if(is.null(input$dataset))
            return("\n")
        summary(data_set())
    }, include.rownames = FALSE)
    
    output$mosaic <- renderPlot({
        if(is.null(input$dataset))
            return("\n")
        
        ls <- paste(input$columns, collapse = "+")
        if (input$freq == TRUE) {
            f <- as.formula(paste(input$freqVar, "~", ls))
        } else f <- as.formula(paste("~", ls))
        mosaic(f, data = data_set(), shade = input$shading)
    })
    
    output$anova <- renderPrint({
        if(is.null(input$dataset))
            return("\n")
        
        rs <- paste(input$columns, collapse = "+")
        f <- as.formula(paste("~", rs))
        ff1 <- xtabs(f, data = data_set())
        
        ff2 <- as.data.frame(ff1)
        f <- as.formula(paste("Freq ~", rs))
        mod <- glm(f, data = ff2, family = "poisson")
        print(anova(mod))
        print(coef(mod))
    })
    
#     output$mosaic <- renderPlot({
#         mod <- loglinear()
# #       f <- switch(input$model,
# #                   "(SPT)"= c("Presence*Period*Temperature.category"),
# #                   "(SP,ST,PT)" = c("Presence*Period", "Presence*Temperature.category", "Period*Temperature.category"), 
# #                   "(ST,PT)" = c("Presence*Temperature.category", "Period*Temperature.category"),
# #                   "(SP,PT)" = c("Presence*Period", "Period*Temperature.category"),
# #                   "(SP,ST)" = c("Presence*Period", "Presence*Temperature.category"))
#         
#         mod.glm <- glm(formula = reformulate(f, response = "Freq"), data = mod, family = poisson)
#         
#         mosaic.glm(mod.glm, 
#                    formula =  ~ Temperature.category + Period + Presence,
#                    gp = shading_hcl,
#                    spacing = spacing_highlighting,
#                    type = input$type,
#                    labeling_args = list(rot_labels = c(left = 0,  right = 0),
#                                        offset_varnames = c(left=1.5, right = 1), 
#                                        offset_labels = c(left=.5, right = .1),
#                                        set_varnames = c(Temperature.category="Temperature", Period="Period",
#                                                         Presence="Status")),
#                    set_labels=list(Presence = c("Ab","Pr")), 
#                    margins = c(right = 5, left = 4, bottom = 1, top =3 ))
#     })
#     
})

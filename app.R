## app.R ##

#to run this code, this must be placed inside the session working directory; install shiny, shinydashboard, and ggplot2; in the console: runApp()

#install.packages(c("devtools", "shiny"))
#devtools::install_github("rstudio/shinydashboard")
#install.packages("ggplot2"")

library(shiny)
library(shinydashboard)
library(ggplot2) 
data(mtcars)

#this is the data source

dataCars <- mtcars
dataCars$gear <- factor(mtcars$gear,levels=c(3,4,5), labels=c("3gears","4gears","5gears")) 
dataCars$am <- factor(mtcars$am,levels=c(0,1), labels=c("Automatic","Manual")) 
dataCars$cyl <- factor(mtcars$cyl,levels=c(4,6,8), labels=c("4cyl","6cyl","8cyl")) 


ui <- dashboardPage (
    dashboardHeader(title = "MTCars Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Cars Table", tabName = "tableView", icon = icon("tasks")),
            menuItem("Distribution of Mileage", tabName = "mileageByDistribution", icon = icon("star")),
            menuItem("Mileage Vs Horse Power", tabName = "mileageVsHp", icon = icon("star")),
            menuItem("Mileage On Weight", tabName = "mileageOnWt", icon = icon("star")),
            menuItem("Mileage By Gear", tabName = "mileageByGear", icon = icon("star")), 
            menuItem("Predict Mileage", tabName = "predictMileage", icon = icon("fire")),
            menuItem("Help/Documentation", tabName = "help", icon = icon("question"))
        )
    ),
    dashboardBody (
        width = 700,
        tabItems(
            tabItem(tabName = "tableView",
                    h2("MT Cars Table"), 
                    dataTableOutput("carsTable")
            ),
            tabItem(tabName = "mileageByDistribution",
                    box(
                        plotOutput("mpgDistribution"),
                        height = 500, width = 700
                    )
            ),
            tabItem(tabName = "mileageVsHp",
                    box(
                        plotOutput("scatterMPGvsHP"),
                        height = 500, width = 700
                    )
            ),
            tabItem(tabName = "mileageOnWt",
                    box(
                        plotOutput("mpgOnWeight"),
                        height = 500, width = 700
                    )
            ),
            tabItem(tabName = "mileageByGear",
                    box(
                        plotOutput("mileageByGear"),
                        height = 500, width = 700
                    )
            ),
            tabItem(tabName = "help", 
                    h1("About MTCars Dashboard"),
                    div(class = "about-class", p("I am a mobile apps developer who deeply desires to have the ability to create big data dashboards for the backend of my projects. I believe that an R-Shiny dashboard would be great because of R's capability to analyze big data."), 
                        p("This project made use of the following packages: shiny, shinydashboard, and ggplot to display, plot, and analyze the mtcars data."), 
                        p("The first menu on the left displays the mtcars data inside a table. The next four menus display different graphs that provide insight about the data."),
                        p("The Prediction menu is the most important one. It predicts the mileage from user-provided data based on a linear model fitting of the mtcars data where all columns were used as predictors. You can test this by using the input interfaces to submit new data to the server that results in the display of a new prediction value."),
                        p("Batchmate, if time comes that you will need a mobile apps developer to work with you on your big data projects, please do not forget me: rflauta@codemagnus.com"))
            ),
            tabItem(tabName = "predictMileage",
                    h2("Prediction Predictors and Variables"),
                    fluidRow(
                        column(width = 4,
                               selectInput("gears",
                                           label="Forward Gears",
                                           choices=c("3", "4", "5"),
                                           selected="3",
                                           width = 150
                               ),
                               selectInput("vs",
                                           label="V/S",
                                           choices=c("0", "1"),
                                           selected="0",
                                           width = 150
                               ),
                               radioButtons("transmission", label = "Transmission",
                                            choices = list("Automatic" = 0, "Manual" = 1),
                                            selected = 0),
                               radioButtons("cylinder", label = "Cylinder",
                                            choices = list("4" = 4, "6" = 6, "8" = 8),
                                            selected = 6)
                        ),
                        column(
                            width = 8,
                            
                            box(
                                title = "Horsepower",
                                sliderInput("sliderHorsepower", "Select the horsepower:", ceiling(min(mtcars$hp)), floor(max(mtcars$hp)), ceiling(min(mtcars$hp)))
                            ),
                            box(
                                title = "Weight",
                                sliderInput("sliderWeight", "Select the weight:", ceiling(min(mtcars$wt)), floor(max(mtcars$wt)), ceiling(min(mtcars$wt)))
                            ),
                            box(
                                title = "Displacement",
                                sliderInput("sliderDisplacement", "Select the displacement:", ceiling(min(mtcars$disp)), floor(max(mtcars$disp)), ceiling(min(mtcars$disp)))
                            )),
                        
                        height = 500, width = 12
                    ),
                    box(h1("The predicted MPG is :"), h1(textOutput("predictionValue")), width = 700)
            )
        )
    )
)

server <- function(input, output) {
    
    output$carsTable <- renderDataTable({ mtcars })
    
    output$mpgDistribution <- renderPlot({
        # Kernel density plots for mpg
        # grouped by number of gears (indicated by color)
        qplot(mpg, data=dataCars, geom="density", fill=gear, alpha=I(.5), 
              main="Distribution of Gas Milage", xlab="Miles Per Gallon", 
              ylab="Density")
    })
    
    output$scatterMPGvsHP <- renderPlot({
        # Scatterplot of mpg vs. hp for each combination of gears and cylinders
        # in each facet, transmittion type is represented by shape and color
        qplot(hp, mpg, data=dataCars, shape=am, color=am, 
              facets=gear~cyl, size=I(3),
              xlab="Horsepower", ylab="Miles per Gallon") 
    })
    
    output$mpgOnWeight <- renderPlot({
        # Separate regressions of mpg on weight for each number of cylinders
        qplot(wt, mpg, data=dataCars, geom=c("point", "smooth"), 
              method="lm", formula=y~x, color=cyl, 
              main="Regression of MPG on Weight", 
              xlab="Weight", ylab="Miles per Gallon")
    })
    
    output$mileageByGear <- renderPlot({
        
        # Boxplots of mpg by number of gears 
        # observations (points) are overlayed and jittered
        qplot(gear, mpg, data=dataCars, geom=c("boxplot", "jitter"), 
              fill=gear, main="Mileage by Gear Number",
              xlab="", ylab="Miles per Gallon")
    })
    
    output$predictionValue <- renderText({
        
        fit <- lm(mpg ~ ., mtcars)
        
        newData <- data.frame(wt = input$sliderWeight, hp = input$sliderHorsepower, am = as.numeric(input$transmission), cyl = as.numeric(input$cylinder), disp = as.numeric(input$sliderDisplacement), drat = mean(mtcars$drat), qsec=mean(mtcars$qsec), carb = mean(mtcars$carb), vs = as.numeric(input$vs), gear = as.numeric(input$gears))
        newData.pred <- predict(fit, newData) 
        
        newData.pred
    })
}

shinyApp(ui = ui, server = server)


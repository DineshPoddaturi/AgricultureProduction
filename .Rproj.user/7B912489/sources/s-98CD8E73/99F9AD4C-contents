#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    datasetInput <- reactive({
        
        # here we read the selected Indicator to use the appropriate data
        if(input$FoodType=="Dairy"){
            
            # data_tidy <- education_tidy
            data_StatesOnly <- education_StatesOnly
            dataTrends_US <- education_trends_US
            # data_US <- education_US
            
        } else if(input$FoodType=="Meat"){
            
            # data_tidy <- population_tidy
            data_StatesOnly <- population_StatesOnly
            dataTrends_US <- population_trends_US
            # data_US <- population_US
            
        }else if(input$FoodType=="Vegetables"){
            
            # data_tidy <- unemployment_tidy
            data_StatesOnly <- unemployment_StatesOnly
            dataTrends_US <- unemployment_trends_US
            # data_US <- unemployment_US
            
        } else if(input$FoodType=="Grain"){
            
            # data_tidy <- poverty_tidy
            data_StatesOnly <-poverty_StatesOnly
            dataTrends_US <- poverty_trends_US
            # data_US <- poverty_US
            
        } 
        
        data <- list(data_StatesOnly,dataTrends_US)
        data
        
    })
    
    output$type <- renderUI({
        
        if(input$FoodType=="Dairy"){
            # types <- sort(unique(dataSelected$Type))
            selectInput("types",label="Type",choices=c("Cheese", "Frozen Dairy"))
            
        }else if(input$FoodType=="Meat"){
            
            selectInput("types",label="Type",choices=c("Leading Meat (boneless)", "All Meat (boneless)"))
            
        }else if(input$FoodType=="Vegetables"){
            
            selectInput("types",label="Type",choices=c("Leafy Greens", "Cruciferous", "Root"))
            
        } else if(input$FoodType=="Grain"){
            
            selectInput("types",label="Type",choices=c("Corn Use"))
            
        } 
        
    })

})



##### Functions that return the data required for plots

cheeseData <- function(cheese){
    
}

frozenDairyData <- function(dairyFrozen){
    
}

leadingMeatData <- function(leadingMeat){
    
}

allMeatData <- function(allMeat){
    
}

leafyGreensData <- function(greens){
    
}

cruciferousData <- function(cruciferous){
    
}

rootsData <- function(roots){
    
}

cornUseData <- function(corn){
    
}








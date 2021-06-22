#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Food Availability/Usage in the United States"),

    # Sidebar with a dropdown selection 
    sidebarPanel(
        selectInput("FoodType",label="Food Group",choices=c("Dairy","Meat","Vegetables","Grain")),
        uiOutput("type")
    ),
    
    # Showing all the plots
    mainPanel(
        tabsetPanel(
            tabPanel("Availability/Usage",plotlyOutput("Trends"))
        )
    )
    
))

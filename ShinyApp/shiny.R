library(tidyverse)
library(readxl)
library(lubridate)
library(shiny)
library(shinythemes)
library(plotly)
library(crosstalk)
library(readr)
library(ggvis)
library(maps)
library(mapproj)
library(stringr)
library(rsconnect)


##Functions to clean the data using the tidy techniques

educationTidy <- function(education){
  
  #selecting the important variables
  education_tidy <- education %>% select (1,2,3,44,45,46,47) %>% data.frame()
  
  #here we fix the column names
  names(education_tidy) <- gsub("..","-",names(education_tidy),fixed=TRUE)
  
  #renaming some columns to common pattern
  education_tidy <- education_tidy %>% rename("FIPS"=`FIPS.Code`,"Area_Name"=`Area.name`)
  
  
  #here we gather the data and split to give us a tidy format of our data 
  education_tidy <- education_tidy %>% gather(key="TypeofEducation-Year",value="Value",4:7) %>% 
    separate(`TypeofEducation-Year`,into=c("Type","Year"),sep="-")
  
  education_tidy$Year <- gsub("2014.18",replacement = "2018",education_tidy$Year,fixed = TRUE)
  education_tidy$Type <- gsub("."," ",education_tidy$Type,fixed = TRUE)
  
  return (education_tidy)
}

educationTrendsTidy <- function(education){
  
  #selecting the important variables
  education_trends_tidy <- education %>% select (1,2,3,12,13,14,15,20,21,22,23,28,29,30,31,36,37,38,39,44,45,46,47) %>% data.frame()
  
  #here we fix the column names
  names(education_trends_tidy) <- gsub("..","-",names(education_trends_tidy),fixed=TRUE)
  
  #renaming some columns to common pattern
  education_trends_tidy <- education_trends_tidy %>% rename("FIPS"=`FIPS.Code`,"Area_Name"=`Area.name`,"Percent.of.adults.completing.some.college.or.associate.s.degree-1970"=`Percent.of.adults.completing.some.college-1.3.years-.1970`,"Percent.of.adults.completing.some.college.or.associate.s.degree-1980"=`Percent.of.adults.completing.some.college-1.3.years-.1980`)
  
  
  #here we gather the data and split to give us a tidy format of our data 
  education_trends_tidy <- education_trends_tidy %>% gather(key="Type-Year",value="Number",c(4:23)) %>% 
    separate(`Type-Year`,into=c("Type","Year"),sep="-")
  
  education_trends_tidy$Year <- gsub("2014.18","2018",education_trends_tidy$Year,fixed = TRUE) 
  
  education_trends_tidy$Type <- gsub("."," ",education_trends_tidy$Type,fixed = TRUE)
  
  return (education_trends_tidy)
}



populationTidy <- function(population){
  
  #selecting the important variables
  population_tidy <- population %>% select(1,2,3,20,120,129) %>% data.frame()
  
  #renaming some columns to common pattern
  names(population_tidy)[4:6] <- c("Population-2019","BirthRate-2019","DeathRate-2019")
  
  #here we gather the data and split to give us a tidy format of our data 
  population_tidy<- population_tidy %>% gather(key="Type-Year",value="Value",c(4:6)) %>% separate(`Type-Year`,into=c("Type","Year"))
  
  return(population_tidy)
  
}

populationTrendsTidy <- function(population){
  
  #selecting the important variables
  population_trends_tidy <- population %>% select(1,2,3,11,12,13,14,15,16,17,18,19,20,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129)%>%data.frame()
  
  #renaming some columns to common pattern
  names(population_trends_tidy) <- gsub("R_birth","BirthRate",names(population_trends_tidy),fixed=TRUE)
  names(population_trends_tidy) <- gsub("R_death","DeathRate",names(population_trends_tidy),fixed=TRUE)
  names(population_trends_tidy) <- gsub("POP_ESTIMATE","Population",names(population_trends_tidy),fixed=TRUE)
  
  
  #here we gather the data and split to give us a tidy format of our data 
  population_trends_tidy<- population_trends_tidy %>% gather(key="Type-Year",value="Number",c(4:31)) %>% separate(`Type-Year`,into=c("Type","Year"))
  
  
  return(population_trends_tidy)
  
}

povertyTidy <- function(poverty){
  
  #selecting the important variables
  poverty_tidy<- poverty %>% select(1,2,3,11,17,26)%>% data.frame()
  
  #renaming some columns to common pattern
  names(poverty_tidy) <- c("FIPS","State","Area_Name","Percent of people of all ages in poverty","Percent of people age 0-17 in poverty","Median_Household_Income")
  
  #here we gather the data us a tidy format of our data 
  poverty_tidy <- poverty_tidy %>% gather(key="Type",value="Value",c(4:6)) %>% mutate(Year="2018")
  
  return (poverty_tidy)
  
}

unemploymentTidy <- function(unemployment){
  
  unemployment_tidy <- unemployment %>% select(1,2,3,86) %>% data.frame()
  #renaming some columns to common pattern
  unemployment_tidy <- unemployment_tidy %>% rename("FIPS"=`FIPS`,"Area_Name"=`Area_name`,"Unemployment Rate-2019"=`Unemployment_rate_2019`) 
  
  # %>% 
  #   mutate(`Unemployment Rate-2015`=parse_number(`Unemployment Rate-2015`))
  
  #here we gather the data and split to give us a tidy format of our data 
  unemployment_tidy <- unemployment_tidy %>% gather(key="EmploymentType-year", value="Value",4) %>% 
    separate(`EmploymentType-year`,into=c("Type", "Year"),sep="-")
  
}

unemploymentTrendsTidy <- function(unemployment){
  
  unemployment_trends_tidy <- unemployment %>% select(1,2,3,38,42,46,50,54,58,62,66,70,74,78,82,86) %>% data.frame()
  
  #renaming some columns to common pattern
  unemployment_trends_tidy <- unemployment_trends_tidy %>% rename("FIPS"=`FIPS`,"Area_Name"=`Area_name`)
  
  columnNames <- colnames(unemployment_trends_tidy)[4:12]
  #here we parse the data into numeric since it was in character format
  # unemployment_trends_tidy[columnNames] <- sapply(unemployment_trends_tidy[columnNames],parse_number)
  
  #renaming some columns to common pattern
  names(unemployment_trends_tidy) <- gsub("Unemployment_rate","UnemploymentRate",names(unemployment_trends_tidy),fixed=TRUE)
  
  #here we gather the data and split to give us a tidy format of our data 
  unemployment_trends_tidy <- unemployment_trends_tidy %>% gather(key="Type-year", value="Number",c(4:16)) %>% 
    separate(`Type-year`,into=c("Type", "Year"),sep="_")
  
  
  return(unemployment_trends_tidy)
  
}

#this function return data of States only
stateData <- function(data){
  states <- state.name
  return(data[data$Area_Name %in% states,])
}

#merging map data to dataset
statesMapData <- function(data,type){
  all_states <- map_data("state")
  data$region <- tolower(data$Area_Name)
  dataFilter <- data %>% filter(Type==type)
  completeDataSet <- left_join(dataFilter,all_states,by="region")
  return(completeDataSet)
}

#ordering the data for top ten plot
reorderingData <- function(data,type){
  dataFilter <- data %>%filter(Type==type)
  topTen <- dataFilter %>% arrange(desc(Value))%>%select(Area_Name,Value)%>%head(10)
  topTen$Area_Name <- factor(topTen$Area_Name,levels = topTen$Area_Name[order(topTen$Value)])
  return(topTen)
}

#filtering data for states
stateWide <- function(data,indicator){
  
  if(indicator=="Education"){
    
    data$Type <- gsub("Percent of adults ",replacement = "",data$Type)
    data$Type <- gsub("with a ",replacement = "",data$Type)
    data$Type <- gsub("with ",replacement = "",data$Type)
    data$Type <- gsub("completing ",replacement = "",data$Type)
    data$Type <- gsub("completing some ",replacement = "",data$Type)
    data <- data
    
  }else if(indicator=="Population"){
    data <- data %>% 
    filter(Type=="BirthRate" | Type=="DeathRate")
  }else if(indicator=="Poverty"){
    data <- data %>% 
    filter(Type=="Percent of people of all ages in poverty" | Type=="Percent of people age 0-17 in poverty")
  }else if(indicator=="Unemployment"){
    data <- data
  }
  return(data)
}



#reading data
education <- read_xls(path="data/Education_2020.xls",sheet = 1)
population <- read_xls(path="data/PopulationEstimates_2020.xls",sheet = 1)
poverty <- read_xls(path="data/PovertyEstimates_2020.xls", sheet = 1)
unemployment <- read_xls(path="data/Unemployment_2020.xls", sheet=1)


education_tidy <- educationTidy(education)
population_tidy <- populationTidy(population)
poverty_tidy <- povertyTidy(poverty)
unemployment_tidy <- unemploymentTidy(unemployment)


education_StatesOnly <- stateData(education_tidy)
population_StatesOnly <- stateData(population_tidy)
poverty_StatesOnly <- stateData(poverty_tidy)
unemployment_StatesOnly <- stateData(unemployment_tidy)

education_US <- education_StatesOnly %>% group_by(Type) %>% summarize(Number = sum(Value)/length(unique(State)))
population_US <- population_StatesOnly %>% group_by(Type) %>% summarize(Number=sum(Value)/length(unique(State)))
poverty_US <- poverty_StatesOnly %>% group_by(Type) %>% summarize(Number=sum(Value)/length(unique(State)))
unemployment_US <- unemployment_StatesOnly %>% group_by(Type) %>% summarize(Number=sum(Value)/length(unique(State)))


education_trends_tidy <- educationTrendsTidy(education)
population_trends_tidy <- populationTrendsTidy(population)
poverty_trends_tidy <- povertyTidy(poverty)
unemployment_trends_tidy <- unemploymentTrendsTidy(unemployment)

education_trends_StatesOnly <- stateData(education_trends_tidy)
population_trends_StatesOnly <- stateData(population_trends_tidy)
poverty_trends_StatesOnly <- stateData(poverty_trends_tidy)
unemployment_trends_StatesOnly <- stateData(unemployment_trends_tidy)

education_trends_US <- education_trends_StatesOnly %>% group_by(Type,Year) %>% summarize(count = sum(Number)/length(unique(State))) %>%ungroup()
education_trends_US$Type <- gsub("Percent of adults ",replacement = "",education_trends_US$Type)
education_trends_US$Type <- gsub("with a ",replacement = "",education_trends_US$Type)
education_trends_US$Type <- gsub("with ",replacement = "",education_trends_US$Type)
education_trends_US$Type <- gsub("completing ",replacement = "",education_trends_US$Type)
education_trends_US$Type <- gsub("completing some ",replacement = "",education_trends_US$Type)

population_trends_US <- population_trends_StatesOnly %>% group_by(Type,Year) %>% summarize(count=sum(Number)/length(unique(State)))%>%ungroup()
poverty_trends_US <- poverty_trends_StatesOnly %>% group_by(Type) %>% summarize(count=sum(Value)/length(unique(State)))%>%ungroup()
unemployment_trends_US <- unemployment_trends_StatesOnly %>% group_by(Type,Year) %>% summarize(count=sum(Number)/length(unique(State)))%>%ungroup()




# Define UI for Socioeconomic Indicators - Unites States
ui <- fluidPage(
  # shinythemes::themeSelector(),
  
  # Application title
  titlePanel("Socioeconomic Indicators - United States"),
  
  sidebarPanel(
    selectInput("SocioeconomicIndicator",label="Indicator",choices=c("Education","Population","Unemployment","Poverty")),
    uiOutput("type"),
    uiOutput("state")
  ),
  
  # Showing all the plots
  mainPanel(
    tabsetPanel(
      tabPanel("Trends",plotlyOutput("Trends")),
      tabPanel("Nationwide",plotlyOutput("Nationwide")),
      tabPanel("TopTenStates",plotlyOutput("TopTenStates")),
      tabPanel("State",plotlyOutput("State"))
    )
  )
)



# Defining server logic 
server <- function(input, output) {
  
  datasetInput <- reactive({
    
    # here we read the selected Indicator to use the appropriate data
    if(input$SocioeconomicIndicator=="Education"){
      
      # data_tidy <- education_tidy
      data_StatesOnly <- education_StatesOnly
      dataTrends_US <- education_trends_US
      # data_US <- education_US
      
    } else if(input$SocioeconomicIndicator=="Population"){
      
      # data_tidy <- population_tidy
      data_StatesOnly <- population_StatesOnly
      dataTrends_US <- population_trends_US
      # data_US <- population_US
      
    }else if(input$SocioeconomicIndicator=="Unemployment"){
      
      # data_tidy <- unemployment_tidy
      data_StatesOnly <- unemployment_StatesOnly
      dataTrends_US <- unemployment_trends_US
      # data_US <- unemployment_US
      
    } else if(input$SocioeconomicIndicator=="Poverty"){
      
      # data_tidy <- poverty_tidy
      data_StatesOnly <-poverty_StatesOnly
      dataTrends_US <- poverty_trends_US
      # data_US <- poverty_US
      
    } 
    
    data <- list(data_StatesOnly,dataTrends_US)
    data
    
  })
  
  output$type <- renderUI({
    
    dataSelected <- datasetInput()[[1]]
    # types <- sort(unique(dataSelected$Type))
    selectInput("types",label="Type",choices=unique(dataSelected$Type),selected =dataSelected$Type[1])
    
  })
  
  output$state <- renderUI({
    
    dataSelected <- datasetInput()[[1]]
    # states <- unique(dataSelected$State)
    selectInput("states",label="State",choices = unique(dataSelected$Area_Name),selected=dataSelected$Area_Name[1])
    
  })
  
  output$Nationwide <- renderPlotly({
    
    dataSelected <- datasetInput()[[1]]
    selectedType <- input$types
    completeDataSet <- statesMapData(dataSelected,selectedType)
    plot <- completeDataSet %>% ggplot(aes(x = long, y = lat, group = region)) +
      geom_polygon(aes(fill = Value))  +
      scale_fill_gradient2(name=selectedType,mid = "white",high="purple") +
      ggthemes::theme_map() + theme(legend.position = c(0.8, 0))
    ggplotly(plot)
    
  })
  
  output$TopTenStates <- renderPlotly({
    
    dataSelected <- datasetInput()[[1]]
    selectedType <- input$types
    orderedData <- reorderingData(dataSelected,selectedType)
    plot <- orderedData %>% ggplot(aes(x=Area_Name,y=Value,fill=Area_Name))+
      geom_bar(stat="identity")+coord_flip()+labs(x="State")+theme_bw()
    ggplotly(plot)
    
  })
  
  output$State <- renderPlotly({
    
    dataSelected <- datasetInput()[[1]]
    selectedState <- input$states
    dataSelected <- stateWide(dataSelected,input$SocioeconomicIndicator)
    plot <- dataSelected %>% filter(Area_Name == selectedState) %>% ggplot(aes(x=Type,y=Value,fill=Type))+geom_bar(stat="identity")+
      theme_bw()+theme(legend.position="none")
    ggplotly(plot,height = 600)
    
  })
  
  output$Trends <- renderPlotly({
    
    dataSelected <- datasetInput()[[2]]
    if(input$SocioeconomicIndicator=="Education"){
      
      plot <- dataSelected %>% ggplot(aes(x=Year,y=count, color= Type))+geom_point(aes(colour=Type))+
        geom_path(aes(colour=Type,group=Type))+
        ggtitle("Trends of percentage of adults with different education types")+labs(y="Percentage")+theme_bw()
      
    } else if(input$SocioeconomicIndicator=="Population"){
      
      plot <- dataSelected %>% filter(Type %in% c("BirthRate","DeathRate")) %>% ggplot(aes(x=Year,y=count))+geom_point(aes(colour=Type))+
        geom_path(aes(colour=Type,group=Type))+ggtitle("Birth rate and Death rate trends")+labs(y="Rate")+theme_bw()
      
    } else if(input$SocioeconomicIndicator=="Unemployment"){
      
      plot <-  dataSelected %>% ggplot(aes(x=Year,y=count))+geom_point(aes(colour=Type))+geom_path(aes(group=Type,colour=Type))+
        ggtitle("Unemployment rate trends")+labs(y="Rate")+theme_bw()
      
    } else if(input$SocioeconomicIndicator=="Poverty"){
      
      plot <- dataSelected %>% filter(Type %in% c("Percent of people of all ages in poverty","Percent of people age 0-17 in poverty")) %>% ggplot(aes(x=Type,y=count,fill=Type))+
        geom_bar(stat="identity")+ggtitle("Poverty percentage 2015 only")+labs(y="Percentage")+theme_bw()
      
    }
    
    ggplotly(plot,height = 400)
    
  })
  
}

# Bind ui and server together
shinyApp(ui, server)






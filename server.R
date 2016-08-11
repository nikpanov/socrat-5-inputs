


# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# loading libraries to use in application
library(shiny)
library(ggplot2)
library(RSocrata)
library(maps)
library(mapproj)
library(dplyr)
library(sqldf)
library(utils)
library(extrafont)

# include code from helpers file
source("helpers.R")

# options switches
# options(shiny.error = browser) - for debugging
options(stringsAsFactors = FALSE) # to prevent default convertion vector to factor

# to take off limitation of 5k for data load
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

# #load data from Socrata just once per user and keep it in data_source dataframe; takes ~ 30 sec
# inURL = "https://chronicdata-stage.demo.socrata.com/Healthy-Aging/Healthy-Aging-Data/ufgt-sc29"
# data_source <- read.socrata(inURL)

# #load data from saved CSV file for speed
inURL = "Healthy_Aging_DataDB.csv"
data_source <- read.csv(inURL, header = TRUE)

#extract list of classes from data frame into named list "categories"
# we put it here because it is executed when app starts for every session
unique.class <- distinct(select(data_source, Class, ClassId))
categories  <- as.vector(unique.class$ClassId)
names(categories) <- unique.class$Class



shinyServer(function(input, output, session) {
  
  #=======================================================================================
  # build dynamic input controls 
  #=======================================================================================
  
  # render categories input
  output$CategoryChoices <-
    renderUI({
      selectInput("SelectClass", "Select Category", categories)
    })
  
  output$YearChoices <- renderUI({
    # extract list of Years for given classId into named list "years"
    unique.year <-
      distinct(select(
        filter(data_source[order(data_source$YearStart),], ClassId == input$SelectClass) ,
        YearStart
      ))
    sorted.year <- unique.year[order(-unique.year$YearStart),]
    years <- as.character(sorted.year)
    names(years) <- as.integer(sorted.year)
    selectInput("SelectYear", "Select Year", choices = years)
   
  })
  
  #
  output$StateChoices <- renderUI({
    # extract list of Locations for given classId and Year into named list "states"
    unique.state <-
      distinct(select(
        filter(
          data_source[order(data_source$LocationId),],
          ClassId == input$SelectClass,
          YearStart == input$SelectYear
        ) ,
        LocationDesc,
        LocationId
      ))
    states  <- as.integer(unique.state$LocationId)
    names(states) <- unique.state$LocationDesc
    
    selectInput("SelectState", "Select State", choices = states)
  })
  
  #
  output$IndicatorChoices <- renderUI({
    # extract list of Years for given classId, Year, LocationId into named list "indicators"
    unique.indicator <-
      distinct(select(
        filter(
          data_source[order(data_source$QuestionId),],
          ClassId == input$SelectClass,
          YearStart == input$SelectYear,
          LocationId == input$SelectState
        ) ,
        Question,
        QuestionId
      ))
    indicators  <- as.character(unique.indicator$QuestionId)
    names(indicators) <- unique.indicator$Question
    
    selectInput("SelectIndicator", "Select Question", choices = indicators)
  })
  
  # 
  output$StratCategoryChoices <- renderUI({
    # extract list of BreakOut Categories for given classId, Year, LocationId, IndicatorId  into named list "bocats"
    unique.bocat <-
      distinct(select(
        filter(
          data_source[order(data_source$StratificationCategoryId1),],
          ClassId == input$SelectClass,
          YearStart == input$SelectYear,
          LocationId == input$SelectState,
          QuestionId == input$SelectIndicator
        ) ,
        StratificationCategory1,
        StratificationCategoryId1
      ))
    bocats  <- as.character(unique.bocat$StratificationCategoryId1)
    names(bocats) <- unique.bocat$StratificationCategory1
    
    selectInput("SelectStratCat", "Select Strat Category", choices = bocats)
  })
  
  #
  output$StratChoices <- renderUI({
    # extract list of BreakOuts for given classId, Year, LocationId, IndicatorId, BOCId into named list "bouts"
    unique.bout <-
      distinct(select(
        filter(
          data_source[order(data_source$StratificationId1),],
          ClassId == input$SelectClass,
          YearStart == input$SelectYear,
          LocationId == input$SelectState,
          QuestionId == input$SelectIndicator,
          StratificationCategoryId1 == input$SelectStratCat
        ) ,
        Stratification1,
        StratificationId1
      ))
    bouts  <- as.character(unique.bout$StratificationId1)
    names(bouts) <- unique.bout$Stratification1

    selectInput("SelectStrat", "Select Stratification", choices = bouts)
  })

  #=========================================================================
  # Render output visualization 
  #=======================================================================
  
  # build map
  output$map <- renderPlot({
    
    # filter rows by user input into new data frame variable
    filtered_data <-
      filter(
        data_source,
        YearStart == input$SelectYear,
        ClassId == input$SelectClass,
        QuestionId == input$SelectIndicator,
        StratificationId1 == input$SelectStrat, 
        LocationId < 59 #to exclude nationwide data
      )
    
    # names(filtered_data)
    # select columns needed into map_data data frame
    map_data <-
      select(filtered_data, LocationAbbr, LocationDesc, Data_Value)
    
    # preparation
    data <- map_data$Data_Value
    
    color <- "darkgreen"
    
    #converting QUestion Code into Short QUestion text
    legend <- switch(
      input$SelectIndicator,
      "Q01" = "Eating 2 or more fruits daily",
      "Q02" = "Eating 3 or more vegetables daily",
      "Q03" = "Frequent mental distress",
      "Q04" = "Taking medications for high blood pressure",
      "Q05" = "Fall with injury within last year",
      "Q06" = "Disability status",
      "Q07" = "Oral health: tooth retention",
      "Q08" = "Physically unhealthy days (mean number of days)",
      "Q09" = "Ever had pneumococcal vaccine",
      "Q10" = "Up-to-date with recommended vaccines and screenings - Men",
      "Q11" = "Up-to-date with recommended vaccines and screenings - Women",
      "Q12" = "Mammogram within past 2 years",
      "Q13" = "Obesity",
      "Q14" = "Cholesterol checked in past 5 years",
      "Q15" = "Colorectal cancer screening",
      "Q16" = "No leisure-time physical activity within past month",
      "Q17" = "Current smoking",
      "Q18" = "Influenza vaccine within past year",
      "Q19" = "Diabetes screening within past 3 years",
      "Q20" = "Pap test within past 3 years",
      "Q21" = "Binge drinking within past 30 days",
      "Q22" = "High blood pressure ever",
      "Q23" = "Current depression",
      "Q24" = "Social and emotional support",
      "Q25" = "Life satisfaction",
      "Q26" = "Lifetime diagnosis of anxiety disorder",
      "Q27" = "Lifetime diagnosis of depression",
      "Q28" = "No osteoporosis screening",
      "Q29" = "No smoking cessation counseling",
      "Q30" = "Increased confusion or memory loss among older adults",
      "Q31" = "Functional difficulties associated with increased confusion or memory loss among older adults",
      "Q32" = "Self-rated health (fair to poor health)",
      "Q33" = "Self-rated health (good to excellent health)",
      "Q34" = "Prevalence of sufficient sleep",
      "Q35" = "Recent activity limitations in past month",
      "Q36" = "Provide care for an adult in past month",
      "Q37" = "Provide care for an older adult",
      "Q38" = "Duration of caregiving among older adults",
      "Q39" = "Intensity of caregiving",
      "Q40" = "Provide care for adult with cognitive changes in past year"
    )
    # The vectorised form of if is ifelse:
    #   test <- ifelse(test == "He is", 1,
    #                  ifelse(test == "She is", 1,
    #                         ifelse(test == "He has", 2,
    #                                2)))
    
   # legend = "% good to excellent health"
    
    # invoke functuion from helpers file
    percent_map(
      var = data,
      color = color,
      legend.title = legend,
      max = 100,
      min = 0
    )
    
  })
  
  # build table
  output$table <- renderTable({
    
    filtered_data <-
      filter(
        data_source,
        YearStart == input$SelectYear,
        ClassId == input$SelectClass,
        QuestionId == input$SelectIndicator,
        StratificationId1 == input$SelectStrat,
        LocationId < "59"
        # LocationId == input$SelectState
      )
    
    # select columns needed into table data frame
    table_data <- select(filtered_data,LocationDesc,Data_Value,Low_Confidence_Limit,High_Confidence_Limit)
    table_data$CI <- with(table_data,paste(Low_Confidence_Limit,High_Confidence_Limit, sep =" - "))
    
    table_data$Low_Confidence_Limit <- NULL
    table_data$High_Confidence_Limit <- NULL
    
    names(table_data) [1] <- "State"
    names(table_data) [2] <- "Value"
    
    table_data
  })
  
  # build chart of all states
  output$chart <- renderPlot({
    
    # filter data by inputs
    filtered_data <-
      filter(
        data_source,
        YearStart == input$SelectYear,
        ClassId == input$SelectClass,
        QuestionId == input$SelectIndicator,
        StratificationId1 == input$SelectStrat,
        LocationId < 59
      )
    #
    chart_data <- select(filtered_data,LocationId,LocationAbbr,LocationDesc,Data_Value)
   
    #building chart
    g<- ggplot(data = chart_data,
           aes(x = reorder(LocationDesc, -LocationId), y = Data_Value)) + geom_bar(stat = "identity", width = .7,color="blue") + coord_flip()
    
    g<-g+ggtitle('Healthy Aging Data By Location') +  theme(plot.title = element_text(size=20, face="bold",  margin = margin(10, 0, 10, 0)),
                                                            aspect.ratio = 0.9) + ylab("Value") + xlab("State")
    g
  })
  
  # build bar plot for one state/question
  output$bar <- renderPlot({
    
    # filter data by inputs
    filtered_data <-
      filter(
        data_source,
        YearStart == input$SelectYear,
        ClassId == input$SelectClass,
        QuestionId == input$SelectIndicator,
        LocationId == input$SelectState
      )
    
    bar_data <- select(filtered_data,Stratification1, Data_Value)
    
    ggplot(bar_data, aes(x =Stratification1, y=Data_Value, fill=Stratification1)) + geom_bar(stat="identity") + xlab("Age group") + ylab("Value") + ggtitle("Data by Age group")
    
    
    
  })
  
  
  
  
})
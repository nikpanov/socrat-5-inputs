



# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel(title = "Healthy Aging Data", windowTitle = "https://chronicdata-stage.demo.socrata.com/Healthy-Aging/Healthy-Aging-Data/ufgt-sc29"),
  
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      uiOutput("CategoryChoices"),
      uiOutput("YearChoices"),
      uiOutput("StateChoices") ,
      uiOutput("IndicatorChoices"),
      uiOutput("StratCategoryChoices"),
      uiOutput("StratChoices")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      # Show map
      tabPanel(
        "Map",
        textOutput("ClassId"),
        textOutput("Year"),
        textOutput("StateId"),
        textOutput("IndicatorId"),
        textOutput("AgeGroupId"),
        plotOutput("map")
      ),
      # Show Chart
      tabPanel("Chart", plotOutput("chart")),
      
      # Show table
      tabPanel("Table", tableOutput("table")),
      
      #show bar plot
      tabPanel("Bar plot", plotOutput("bar"))
    ))
  )
))

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(Lahman)

# Define UI for application that draws a histogram

fluidPage(
  theme = shinytheme("cerulean"),
  sidebarLayout(
    sidebarPanel(
    verticalLayout(
      h3("Player(s):"),
      textInput(
        "player",
        "If multiple players are desired, separate players with [comma + space]",
        "ex: 'Mark McGwire, Sammy Sosa'"
      ),
      radioButtons(
        "bp",
        label = "Batter or pitcher?",
        choices = c("batting", "pitching")
      ),
    ),
    h3("Player(s):"),
              selectInput("playersfound", label = "players found:", choices = c("NA", "NA")),
              selectInput("yearchosen", label = "season:", choices = c("NA", "NA")),
    fluidRow(
      actionButton("do", "Graph"),
      actionButton("clear", "reset"),
    ),
    ),
    
    mainPanel(tabsetPanel(
      tabPanel("tab1", "contents1"),
      tabPanel("tab2", "contents2")
    ),
    ),
    ),
    
)
  
  # Application title
  
  
  # Sidebar with a slider input for number of bins

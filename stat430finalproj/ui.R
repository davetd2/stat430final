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
library(shinyjs)
source("helper.R")

# Define UI for application that draws a histogram

fluidPage(
  useShinyjs(),
  theme = shinytheme("cerulean"),
  sidebarLayout(
    sidebarPanel(
    verticalLayout(
      h3("Player(s):"),
      textInput(
        "player",
        "Search players by name keyword:",
        placeholder = "ex: 'Mark McGwire, Sammy Sosa'"
      ),
    ),
    fluidRow(
      actionButton("do", "Search"),
      actionButton("clear", "Reset"),
    ),
    h3("Player(s):"),
              selectInput("playersfound", label = "players found:", choices = c("NA", "NA")),
              selectInput("yearchosen", label = "season:", choices = c("NA", "NA")),
    actionButton("graph", "Graph")
    ),
    
    mainPanel(tabsetPanel(
      tabPanel("similarity score", "This page graphs the similarity scores of the selected player and the 5 most similar players based on the chosen season",
               mainPanel(plotOutput("simplot"),)),
      tabPanel("WAR", "This page graphs the career WAR trajectories of the selected player and the three most similar players based on similarity score",
               mainPanel(plotOutput("warplot")))
    ),
    ),
    ),
    
)
  
  # Application title
  
  
  # Sidebar with a slider input for number of bins

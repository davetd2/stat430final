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
#source("helper.R")

# Define UI for application that draws a histogram

fluidPage(
  theme = shinytheme("cerulean"),
  sidebarLayout(
    sidebarPanel(
      verticalLayout(
        h3("Player(s):"),
        textInput(
          "player",
          "Type a player ",
          placeholder = "ex: 'Rodriguez'"
        ),
        radioButtons(
          "bp",
          label = "Batter or pitcher?",
          choices = c("batting", "pitching")
        ),
      ),
      h3("Player(s):"),
      selectInput("playersfound", label = "players found:", choices = c(NULL)),
      selectInput("yearchosen", label = "season:", choices = NULL),
      fluidRow(
        actionButton("do", "Graph"),
        actionButton("clear", "reset"),
      ),
    ),
    
    mainPanel(tabsetPanel(
      tabPanel("similarity score", "This page graphs the similarity scores of the selected player and the 10 most similar players based on the chosen season",
               mainPanel(plotOutput("simplot"),)),
      tabPanel("WAR", "This page graphs the career WAR trajectories of the selected player and the three most similar players based on similarity score",
               mainPanel(plotOutput("warplot")))
    ),
    ),
  ),
  
)

library(shiny)
library(shinythemes)
library(Lahman)
library(shinyjs)
source("helper.R")

fluidPage(
  useShinyjs(),
  theme = shinytheme("cerulean"),
  sidebarLayout(
    sidebarPanel(
    verticalLayout(
      h3("Player:"),
      textInput(
        "player",
        "Search player by name keyword:",
        placeholder = "ex: 'Sammy Sosa'"
      ),
    ),
    fluidRow(
      actionButton("do", "Search"),
      actionButton("clear", "Reset"),
    ),
    h3("Select Parameters:"),
              selectInput("playersfound", label = "players found:", choices = c("NA", "NA")),
              selectInput("yearchosen", label = "season:", choices = c("NA", "NA")),
    actionButton("graph", "Graph")
    ),
    
    mainPanel(tabsetPanel(
      tabPanel("similarity score", "This page graphs the similarity scores of the selected player and the 5 most similar players based on the chosen season",
               mainPanel(plotOutput("simplot"),)),
      tabPanel("WAR","This page graphs the career WAR trajectories of the selected player and the three most similar players based on similarity score",
               mainPanel(plotOutput("warplot")))
    ),
    ),
    ),
    
)
library(shiny)
library(shinyjs)
library(ggplot2)
source("helper.R")


function(input, output, session) {
  observeEvent(input$do, {
    req(input$player)
    updateSelectInput(inputId = "playersfound", choices = batting %>% filter(str_detect(name, input$player)) %>% select(name))
  })
  observeEvent(input$playersfound, {
    req(input$playersfound)
    if (input$playersfound == "NA") {
      
    } else{
      updateSelectInput(inputId = "yearchosen", choices = 1:max((batting %>% filter(name == input$playersfound))$szn))
    }
  })
  
  output$simplot = renderPlot({
    req(input$player, input$playersfound, input$yearchosen, input$graph)
    ggplot(data = simchartdata(getPYID(input$playersfound, input$yearchosen), 
                                        input$yearchosen,
                                        battersimscore(getPYID(input$playersfound, input$yearchosen), input$yearchosen)),
                    aes(x = szn, y = sim_score, color = name)) +
      geom_line() + 
      geom_vline(xintercept = as.integer(input$yearchosen)) +
      labs(x = "Season Number", y = "Similarity Score", title = "Similarity Score per season")
  })
  
  output$warplot = renderPlot({
    req(input$player, input$playersfound, input$yearchosen, input$graph)
    ggplot(data = simchartdata(getPYID(input$playersfound, input$yearchosen), 
                               input$yearchosen,
                               battersimscore(getPYID(input$playersfound, input$yearchosen), input$yearchosen)),
           aes(x = szn, y = totalWAR, color = name)) +
      geom_line() + 
      geom_vline(xintercept = as.integer(input$yearchosen)) +
      labs(x = "Season Number", y = "WAR per season", title = "WAR data per season")
  })
  
  observeEvent(input$clear, {
    refresh()
  })

}

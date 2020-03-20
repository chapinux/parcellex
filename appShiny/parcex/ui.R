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
  titlePanel("Parcex"),
  
  plotlyOutput('plotPrincipal'),
  
  fluidRow(
    column(4,
           sliderInput('nbpoints', "Number of points",
                       min=1000, max=20000, value=10000, step=1000)
           ),
    column(4,
           plotOutput('plotSHP')
           ),
    column(4,
           h3("Vignette photo")
           )
  
    
  )#fluidrow
 
  )#fluid page
)#ui

install.packages('Rcpp')
library(Rcpp)
library(shiny)
#include <Rcpp.h>
#using namespace Rcpp;

sourceCpp("/home/e20220008770/Documents/programmatioon_R/Projet_R/projetR.cpp")

ui <- fluidPage(
  actionButton("bouton","Cliquez ici")
)

server <- function(input, output, session) {
  observeEvent(input$bouton,{showModal(modalDialog("vous avez cliquez sur le bouton"))})
}

shinyApp(ui, server)


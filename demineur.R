# -*- coding: UTF-8 -*-
install.packages('Rcpp')
library(Rcpp)
library(shiny)
#include <Rcpp.h>
#using namespace Rcpp;

sourceCpp("/home/e20220008770/Documents/programmatioon_R/Projet_R/projetR.cpp")

ui <- fluidPage(
  titlePanel("Démineur"),
  
  sidebarPanel(
    sliderInput("n", "Taille de la grille", min = 5, max = 15, value = 10),
    sliderInput("p", "Difficulté", min = 1, max = 3, value = 1), #Faire un truc plus pratique
      actionButton("reset", "Nouvelle partie")
  ),
  
  mainPanel(
    renderTable("board")
  )
)

server <- function(input, output, session) {
  # Créé une grille de taille n par n avec la probabilité p d'avoir une mine
  createBoard <- function(n, nb_mines) {
    # Initialise une matrice vide
    board <- reactive({matrix(0, n, n)})
    
    # Remplit la matrice avec des mines aléatoires
    mine_coord <- sample(input$n*input$n, nb_mines)
    board[mine_coord] <- -1
    
    # Calcule les valeurs des cases non-mine
    for (i in 1:input$n) {
      for (j in 1:input$n) {
        if (board[i, j] != -1) {
          # Compte le nombre de mines adjacentes
          count <- sum(board[max(1, i - 1):min(input$n, i + 1), max(1, j - 1):min(input$n, j + 1)] == -1)
          board[i, j] <- count
        }
      }
    }
    
    return(board)
  }
  
  # Initialise une partie
  commencePartie <- function(n, p) {
    #On ajuste le nombre de mines à la difficulté
    if (p==1){
      nb_mines <- n
    }
    if (p==2){
      nb_mines <- 2*n
    }
    if (p==3){
      nb_mines <- 3*n
    }
    # Créé une nouvelle grille
    board <- createBoard(n, nb_mines)
    
    # Initialise le tableau de flags
    flags <- matrix(FALSE, n, n)
    
    # Initialise l'état de la grille
    state <- matrix("hidden", n, n)
    
    # Retourne les trois éléments
    list(board = board, flags = flags, state = state)
  }
}


shinyApp(ui, server)


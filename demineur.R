# -*- coding: UTF-8 -*-
install.packages('Rcpp')
install.packages('shiny')
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
  c <- reactive({
   if(input$n==5) return(n<-5)
   if(input$n==10) return(n<-10)
   if(input$n==15) return(n<-15)
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
  }})
  
  # Initialise une partie
   commencePartie<-reactive({ if (input$p == 1) return(nb_mines <-n)
    if (input$p==2) return(nb_mines <- n*2)
    if(input$p==3) return(nb_mines <-n*3)
  
    # Créé une nouvelle grille
    board <- c(n, nb_mines)
    
    # Initialise le tableau de flags
    flags <- matrix(FALSE, n, n)
    
    # Initialise l'état de la grille
    state <- matrix("hidden", n, n)
    
    # Retourne les trois éléments
    list(board = board, flags = flags, state = state)
  })
  
  # Initialise la partie
  game <- commencePartie(input$n, input$p)
  
  # Affiche la grille
  output$board <- renderMatrix({
    # Créé une matrice avec les valeurs affichées sur la grille
    display <- matrix("", input$n, input$n)
    for (i in 1:input$n) {
      for (j in 1:input$n) {
        if (game$state[i, j] == "hidden") {
          if (game$flags[i, j]) {
            display[i, j] <- "🚩"
          } else {
            display[i, j] <- " "
          }
        } else if (game$state[i, j] == "mine") {
          display[i, j] <- "💣"
        } else {
          display[i, j] <- game$board[i, j]
        }
      }
    }
    
    # Affiche la matrice
    matrix(display, input$n, input$n)
  })
  
  # Clic sur une case
  clickCase <- function(i, j) {
    if (game$flags[i, j]) {
      return()
    }
    
    # Vérifie si la case est une mine
    if (game$board[i, j] == -1) {
      # La partie est perdue
      game$state[i, j] <- "mine"
      showGameOver()
      return()
    }
    
    # Affiche la valeur de la case
    game$state[i, j] <- "visible"
    
    # Vérifie s'il n'y a pas de mine adjacente
    if (game$board[i, j] == 0) {
      # Clique sur les cases adjacentes
      for (k in max(1, i - 1):min(input$n, i + 1)) {
        for (l in max(1, j - 1):min(input$n, j + 1)) {
          if (game$state[k, l] == "hidden") {
            clickCase(k, l)
          }
        }
      }
    }
    
    # Vérifie si la partie est gagnée
    if (sum(game$state == "hidden") == sum(game$board != -1)) {
      showGameWon()
    }
  }
  
  # Place un drapeau
  flagCase <- function(i, j) {
    if (game$state[i, j] == "hidden") {
      game$flags[i, j] <- !game$flags[i, j]
    }
  }
  
  # Affiche l'écran de fin de partie en cas de victoire
  showGameWon <- function() {
    showModal(
      modalDialog(
        title = "Gagné !",
        "Vous avez gagné la partie.",
        footer = tagList(
          actionButton("reset", "Nouvelle partie"),
          modalButton("Fermer")
        )
      )
    )
  }
  
  # Affiche l'écran de fin de partie en cas de défaite
  showGameOver <- function() {
    showModal(
      modalDialog(
        title = "Perdu !",
        "Vous avez perdu la partie.",
        footer = tagList(
          actionButton("reset", "Nouvelle partie"),
          modalButton("Fermer")
        )
      )
    )
  }
  
  # Réinitialise la partie
  observeEvent(input$reset, {
    game <<- commencePartie(input$n, input$p)
  })
  
  # Clique sur une case lorsqu'elle est cliquée par l'utilisateur
  observeEvent(input$board_cell_clicked, {
    coords <- strsplit(input$board_cell_clicked, ",")[[1]]
    clickCase(as.numeric(coords[1]), as.numeric(coords[2]))
  })
  
  # Place un drapeau lorsqu'il la case est cliquée-droit par l'utilisateur
  observeEvent(input$board_cell_rightclicked, {
    coords <- strsplit(input$board_cell_rightclicked, ",")[[1]]
    flagCase(as.numeric(coords[1]), as.numeric(coords[2]))
  })
}

shinyApp(ui, server)

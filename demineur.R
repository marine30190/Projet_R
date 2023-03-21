# -*- coding: UTF-8 -*-
library(shiny)

ui <- fluidPage(
  titlePanel("Démineur"),
  
  sidebarPanel(
    sliderInput("taille", "Taille de la grille", min = 5, max = 15, value = 10),
    sliderInput("proba", "Difficulté", min = 1, max = 3, value = 1), #Faire un truc plus pratique
    actionButton("reset", "Nouvelle partie")
  ),
  
  mainPanel(
    tableOutput("board")
  )
)

server <- function(input, output, session) {
  n <- reactive({as.numeric(input$taille)})
  p <- reactive({as.numeric(input$proba)})
  
  # Créé une grille de taille n par n avec des mines
  createBoard <- function(n, nb_mines) { #Créé une matrice de n*n contenant -1 pour les cases minées, le nombre de mines pour les autres cases
    # Initialise une matrice vide
    board <- matrix(0, n, n)
    
    # Remplit la matrice avec des mines aléatoires
    mine_coord <- sample(n*n, nb_mines)
    board[mine_coord] <- -1
    
    # Calcule les valeurs des cases non-mine
    for (i in 1:n) {
      for (j in 1:n) {
        if (board[i, j] != -1) {
          # Compte le nombre de mines adjacentes
          count <- sum(board[max(1, i - 1):min(n, i + 1), max(1, j - 1):min(n, j + 1)] == -1)
          board[i, j] <- count
        }
      }
    }
    
    return(board)
  }
  
  # Initialise une partie
  commencePartie <- function(n, p) {
    #On ajuste le nombre de mines à la difficulté
    nb_mines <- p*n
    # Créé une nouvelle grille
    board <- createBoard(n, nb_mines)
    
    # Initialise le tableau de flags
    flags <- matrix(FALSE, n, n)
    
    # Initialise l'état de la grille
    state <- matrix("hidden", n, n)
    
    # Retourne les trois éléments
    list(board = board, flags = flags, state = state)
  }
  
  # Initialise la partie
  game <- reactive({commencePartie(input$taille, input$proba)})
  
  # Créé la matrice à afficher
  display <- reactive({matrix("", input$taille, inpuut$taille)})
  for (i in 1:n) {
    for (j in 1:n) {
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
  
  # Affiche la grille
  output$board <- renderTable(display)
  
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
      for (k in max(1, i - 1):min(n, i + 1)) {
        for (l in max(1, j - 1):min(n, j + 1)) {
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
    game <- commencePartie(n, p)
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

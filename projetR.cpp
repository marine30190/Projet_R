#include <Rcpp.h>
using namespace Rcpp;

#include <iostream>
#include <vector>
#include <cstdlib>
#include <ctime>

using namespace std;

// taille du tableau :

const int BOARD_taille = 20;

// nb de mines :

const int NB_mines = 10;

// représentation des grilles sur le plateau :

enum SquareState{
  UNREVEALED, REVEALED, FLAGGED
};

// représentation d'une case :

struct Square {
  bool hasMine;
  int numAdjacentMines; 
  SquareState State;
};

// plateau :

vector<vector<Square>> board;

// initialistation du plateau :

void initBoard(){
  board.
  resize(BOARD_taille,vector<Square>(BOARD_taille));
  
  // initialisation toutes les cases à l'état non révélées :
  
  for (int i =0; i< BOARD_taille;i++){
    for(int j=0; j<BOARD_taille;j++){
      board[i][j].state=UNREVEALED;
    }
  }
  
  // placer les mines : 
  
    srand;
    
    srand(time(0));
  
  int nbMinesPlacee =0;
  while(nbMinesPlacee< NB_mines){
    int x = rand() % BOARD_taille;
    int y = rand() % BOARD_taille;
    
    if (!board[x][y].hasMine){
      board[x][y].hasMine= true;
      nbMinesPlacee++;
    }
  }
  // initialisation mine adjencente :
  
  for(int i=0;i<BOARD_taille; i++){
    for (int j=0;j<BOARD_taille;j++){
      int count=0;
      for(int dx=-1; dx <=1; dx++){
        for(int dy=-1; dy<=1;dy++){
          int nx=i+dx;
          int ny=j+dy;
          if(nx>=0 && nx<BOARD_taille && ny >=0 && ny<BOARD_taille && board[nx][ny].hasMine){
            count++;
          }
        }
      }
      board[i][j].numAdjacentMines=count;
    }
  }
}

void printBoard(){
  cout <<"";
  for(int i=0; i<BOARD_taille;i++){
    cout<<""<<i<<endl;
  }
  cout <<endl;
  
  for(int i=0;i<BOARD_taille;i++){
    cout <<i<<"";
    for(int j=0;j< BOARD_taille;j++){
      switch(board[i][j].state){
      case UNREVEALED: cout<<"-";
        break;
      case REVEALED:
        if(board[i][j].hasMine){
          cout<<"*";
        }
        else{cout<<board[i][j].numAdjacentMines;}
        break;
      case FLAGGED: cout<<"F";
        break;
      }
      cout<<"";
    }
    cout<<endl;
  }
}
// révéler une case du plateau 

void revealSqare(int x, int y){
  if(x<0 || x>= BOARD_taille || y<0 || y>=BOARD_taille || board[x][y].state == REVEALED){
    return;
  }
}
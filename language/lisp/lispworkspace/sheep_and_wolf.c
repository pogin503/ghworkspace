#include <stdio.h>
#define LEFT 0
#define RIGHT 1

#define MAX 4

#define SHEEP 0
#define WOLF 1
#define PAPER 2
#define HUMAN 3

int main()
{
  int pos[MAX];

  initArray(pos);
  move(pos)
    }

void initArray(int pos[])
{
  int i;
  for(i = 0 ; i < MAX ; i++){
    pos[i] = LEFT ;
  }
}

int end_flag = 0;

//羊、狼、紙、人
void move(int pos[])  
{
  int i;

  for(i = 0 ; i < MAX : i++){
    if(pos[i] < 0)return;
  }
  if(pos[SHEEP] == pos[WOLF] || pos[SHEEP] == pos[PAPER])
    return;
  
  
}

int printState(int pos[])
{
  
}

#include <mpi.h>
#include <stdio.h>

#define CHUNK_LENGTH 100
#define MATRIX_N 8

int main(int argc, char **argv)
{
  MPI_Init(NULL, NULL);
  int world_size;
  MPI_Comm_size(MPI_COMM_WORLD, &world_size);
  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
  // int msg[1000];
  char msg[13];

  if (world_rank == 0) // левый верхний угол
  {
    char* s = "Hello world!";
    printf("%d\n", world_rank);
    MPI_Send(s, 13, MPI_BYTE, world_rank + 1, 0, MPI_COMM_WORLD);
  }
  else if (world_rank < MATRIX_N - 1) // верхняя строка
  {
    MPI_Recv(msg, 13, MPI_BYTE, world_rank - 1, 0, MPI_COMM_WORLD, NULL);
    printf("%d\n", world_rank);
    MPI_Send(msg, 13, MPI_BYTE, world_rank + 1, 0, MPI_COMM_WORLD);
  }
  else if (world_rank == MATRIX_N - 1) // правый верхний угол
  {
    MPI_Recv(msg, 13, MPI_BYTE, world_rank - 1, 0, MPI_COMM_WORLD, NULL);
    printf("%d\n", world_rank);
    MPI_Send(msg, 13, MPI_BYTE, world_rank + MATRIX_N, 0, MPI_COMM_WORLD);
  }
  else if (world_rank == MATRIX_N * MATRIX_N - 1) // правый нижний угол
  {
    MPI_Recv(msg, 13, MPI_BYTE, world_rank - MATRIX_N, 0, MPI_COMM_WORLD, NULL);
    printf("%d\n", world_rank);
    printf("%s\n", msg);
  }
  else if (world_rank % MATRIX_N == MATRIX_N - 1) // правый столбец
  {
    MPI_Recv(msg, 13, MPI_BYTE, world_rank - MATRIX_N, 0, MPI_COMM_WORLD, NULL);
    printf("%d\n", world_rank);
    MPI_Send(msg, 13, MPI_BYTE, world_rank + MATRIX_N, 0, MPI_COMM_WORLD);
  }
  MPI_Finalize();
}

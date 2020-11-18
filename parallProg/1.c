#include <mpi.h>
#include <stdio.h>

// #define MSG_LENGTH 10000
#define MSG_LENGTH 2000
#define CHUNK_LENGTH 1000
#define NUM_CHUNKS (MSG_LENGTH / CHUNK_LENGTH)

#define MATRIX_N 8
// #define MATRIX_N 4

int main(int argc, char **argv)
{
  MPI_Init(NULL, NULL);
  int world_size;
  MPI_Comm_size(MPI_COMM_WORLD, &world_size);
  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

  if (world_rank == 0) // левый верхний угол
  {
    char msg[MSG_LENGTH];
    for (int i = 0; i < MSG_LENGTH; i++)
    {
      msg[i] = 'a' + i % 26;
    }
    for (int i = 0; i < NUM_CHUNKS; i++)
    {
      MPI_Send(msg + i * CHUNK_LENGTH, CHUNK_LENGTH, MPI_BYTE, world_rank + 1, i, MPI_COMM_WORLD); ///
      printf("%d\n", world_rank);
    }
  }
  else if (world_rank == MATRIX_N * MATRIX_N - 1) // правый нижний угол
  {
    char msg[MSG_LENGTH];
    for (int i = 0; i < NUM_CHUNKS; i++)
    {
      MPI_Recv(msg + i * CHUNK_LENGTH, CHUNK_LENGTH, MPI_BYTE, world_rank - MATRIX_N, i, MPI_COMM_WORLD, NULL); ///
      printf("%d\n", world_rank);
    }
    printf("DONE!\n");
  }
  else
  {
    char chunk[2 * CHUNK_LENGTH]; ///
    int source;
    int destination;

    if (world_rank < MATRIX_N - 1)
    {
      source = world_rank - 1;
      destination = world_rank + 1;
    }
    else if (world_rank == MATRIX_N - 1)
    {
      source = world_rank - 1;
      destination = world_rank + MATRIX_N;
    }
    else if (world_rank % MATRIX_N == MATRIX_N - 1)
    {
      source = world_rank - MATRIX_N;
      destination = world_rank + MATRIX_N;
    }
    else
    {
      // printf("finalize: %d\n", world_rank);
      goto finalize;
    }

    for (int i = 0; i <= NUM_CHUNKS; i++)
    {
      if (i == 0)
      {
        MPI_Recv(chunk + CHUNK_LENGTH * (i % 2), CHUNK_LENGTH, MPI_BYTE, source, i, MPI_COMM_WORLD, NULL); ///
      }
      else if (i == NUM_CHUNKS)
      {
        MPI_Send(chunk + CHUNK_LENGTH * ((i - 1) % 2), CHUNK_LENGTH, MPI_BYTE, destination, i - 1, MPI_COMM_WORLD); ///
      }
      else
      {
        MPI_Request isendReq, irecvReq;                                                                                         ///
        MPI_Isend(chunk + CHUNK_LENGTH * ((i - 1) % 2), CHUNK_LENGTH, MPI_BYTE, destination, i - 1, MPI_COMM_WORLD, &isendReq); ///
        MPI_Irecv(chunk + CHUNK_LENGTH * (i % 2), CHUNK_LENGTH, MPI_BYTE, source, i, MPI_COMM_WORLD, &irecvReq);                ///
        MPI_Wait(&isendReq, NULL);                                                                                              ///
        MPI_Wait(&irecvReq, NULL);                                                                                              ///
      }
      printf("%d\n", world_rank);
    }
  }

finalize:
  MPI_Finalize();
}

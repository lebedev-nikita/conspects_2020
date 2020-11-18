#include <mpi.h>
#include <stdio.h>

#define MSG_LENGTH 2000
#define CHUNK_LENGTH 1000
#define NUM_CHUNKS (MSG_LENGTH / CHUNK_LENGTH)

#define MATRIX_N 8

int main(int argc, char **argv)
{
  MPI_Init(NULL, NULL);
  int world_size;
  MPI_Comm_size(MPI_COMM_WORLD, &world_size);
  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

  MPI_Request req[2];

  // левый верхний угол
  if (world_rank == 0)
  {
    char msg[MSG_LENGTH];
    for (int i = 0; i < MSG_LENGTH; i++)
    {
      msg[i] = 'a' + i % 26;
    }
    for (int i = 0; i < NUM_CHUNKS / 2; i++)
    {
      MPI_Isend(msg + i * CHUNK_LENGTH, CHUNK_LENGTH, MPI_BYTE, world_rank + 1, i, MPI_COMM_WORLD, &req[0]);
      MPI_Isend(msg + (i + NUM_CHUNKS / 2) * CHUNK_LENGTH, CHUNK_LENGTH, MPI_BYTE, world_rank + MATRIX_N, i + NUM_CHUNKS / 2, MPI_COMM_WORLD, &req[1]);
      MPI_Waitall(2, req, NULL);
      printf("%d\n", world_rank);
    }
  }
  // правый нижний угол
  else if (world_rank == MATRIX_N * MATRIX_N - 1)
  {
    char msg[MSG_LENGTH];
    for (int i = 0; i < NUM_CHUNKS / 2; i++)
    {
      MPI_Irecv(msg + i * CHUNK_LENGTH, CHUNK_LENGTH, MPI_BYTE, world_rank - MATRIX_N, i, MPI_COMM_WORLD, NULL, req[0]);
      MPI_Irecv(msg + (i + NUM_CHUNKS / 2) * CHUNK_LENGTH, CHUNK_LENGTH, MPI_BYTE, world_rank - 1, (i + NUM_CHUNKS / 2), MPI_COMM_WORLD, NULL, req[1]);
      MPI_Waitall(2, req, NULL);
      printf("%d\n", world_rank);
    }
    printf("DONE!\n");
  }
  else
  {
    char chunk[2 * CHUNK_LENGTH];
    int source;
    int destination;

    // правый верхний угол
    if (world_rank == MATRIX_N - 1)
    {
      source = world_rank - 1;
      destination = world_rank + MATRIX_N;
    }
    // левый нижний угол
    else if (world_rank % MATRIX_N == 0)
    {
      source = world_rank - MATRIX_N;
      destination = world_rank + 1;
    }
    // верхняя горизонталь
    if (world_rank < MATRIX_N - 1)
    {
      source = world_rank - 1;
      destination = world_rank + 1;
    }
    // нижняя горизонталь
    if (world_rank > MATRIX_N * (MATRIX_N - 1) && world_rank < MATRIX_N * MATRIX_N - 1)
    {
      source = world_rank - 1;
      destination = world_rank + 1;
    }
    // правая вертикаль
    else if (world_rank % MATRIX_N == MATRIX_N - 1)
    {
      source = world_rank - MATRIX_N;
      destination = world_rank + MATRIX_N;
    }
    // левая вертикаль
    else if (world_rank % MATRIX_N == 0)
    {
      source = world_rank - MATRIX_N;
      destination = world_rank + MATRIX_N;
    }
    // серединка
    else
    {
      goto finalize;
    }

    for (int i = 0; i <= NUM_CHUNKS; i++)
    {
      if (i == 0)
      {
        MPI_Recv(chunk + CHUNK_LENGTH * (i % 2), CHUNK_LENGTH, MPI_BYTE, source, i, MPI_COMM_WORLD, NULL);
      }
      else if (i == NUM_CHUNKS)
      {
        MPI_Send(chunk + CHUNK_LENGTH * ((i - 1) % 2), CHUNK_LENGTH, MPI_BYTE, destination, i - 1, MPI_COMM_WORLD);
      }
      else
      {
        MPI_Isend(chunk + CHUNK_LENGTH * ((i - 1) % 2), CHUNK_LENGTH, MPI_BYTE, destination, i - 1, MPI_COMM_WORLD, &req[0]);
        MPI_Irecv(chunk + CHUNK_LENGTH * (i % 2), CHUNK_LENGTH, MPI_BYTE, source, i, MPI_COMM_WORLD, &req[1]);
        MPI_Waitall(2, req, NULL);
      }
      printf("%d\n", world_rank);
    }
  }

finalize:
  MPI_Finalize();
}

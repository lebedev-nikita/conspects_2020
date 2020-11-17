#include <mpi.h>
#include <stdio.h>

int main(int argc, char **argv)
{
  MPI_Init(NULL, NULL);
  int world_size;
  MPI_Comm_size(MPI_COMM_WORLD, &world_size);
  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

  char processor_name[MPI_MAX_PROCESSOR_NAME];
  int name_len;
  MPI_Get_processor_name(processor_name, &name_len);
  int msg[3];

  if (world_rank == 0) {
    msg[0] = 0;
    msg[1] = 1;
    msg[2] = 2;
    MPI_Send( msg , 3 , MPI_INT , world_rank + 1 , 0 , MPI_COMM_WORLD);
  } else {
    MPI_Recv( msg , 3 , MPI_INT , world_rank - 1, 0 , MPI_COMM_WORLD , NULL);
    if (world_rank % 2) {
      printf("rank: %2d, msg: %d %d %d\n", world_rank, msg[0], msg[1], msg[2]);
    }
    if (world_rank != 63) {
      MPI_Send( msg , 3 , MPI_INT , world_rank + 1 , 0 , MPI_COMM_WORLD);
    }
  }

  MPI_Finalize();
}
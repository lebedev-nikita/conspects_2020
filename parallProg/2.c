#include "mpi_fdtd-2d.h"
#include <stdlib.h>
#define CHECKPOINT_ITERATIONS 30

double bench_t_start, bench_t_end;
int numtasks, rank;
int startrow, lastrow, nrows;
int *procsAlive, numProcsAlive;
int nextRank, prevRank, workRank;

// в этих матрицах хранится чекпоинт
float cp_ex[NX][NY];
float cp_ey[NX][NY];
float cp_hz[NX][NY];
float cp__fict_[TMAX];
int cp_t = 0;

static double rtclock() {
  struct timeval Tp;
  int stat;
  stat = gettimeofday(&Tp, NULL);
  if (stat != 0)
    printf("Error return from gettimeofday: %d", stat);
  return (Tp.tv_sec + Tp.tv_usec * 1.0e-6);
}

void bench_timer_start() { bench_t_start = rtclock(); }

void bench_timer_stop() { bench_t_end = rtclock(); }

void bench_timer_print() {
  printf("Time in seconds = %0.6lf\n", bench_t_end - bench_t_start);
}

void init_checkpoint() {
  int i, j;
  for (i = 0; i < TMAX; i++)
    cp__fict_[i] = (float)i;
  for (i = 0; i < NX; i++)
    for (j = 0; j < NY; j++) {
      cp_ex[i][j] = ((float)i * (j + 1)) / NX;
      cp_ey[i][j] = ((float)i * (j + 2)) / NY;
      cp_hz[i][j] = ((float)i * (j + 3)) / NX;
    }
}

void sync_checkpoint(int t,
                     float (**ex)[nrows][NY],
                     float (**ey)[nrows + 2][NY],
                     float (**hz)[nrows + 2][NY],
                     float (**_fict_)[TMAX]
){
  /*
    1. сначала собираем матрицу у одного процесса
    2. потом рассылаем ее всем живым процессам
  */
  cp_t = t;

  int masterProc;
  for (int i = 0; i < numProcsAlive; i++) {
    if (procsAlive[i]) {
      masterProc = i;
      break;
    }
  }

  if (rank == masterProc) {
    // копируем свои данные в матрицу
    memcpy(cp_ex, &(**ex)[0][0], nrows * NY * sizeof(float));
    memcpy(cp_ey, &(**ey)[1][0], nrows * NY * sizeof(float));
    memcpy(cp_hz, &(**hz)[1][0], nrows * NY * sizeof(float));
    // _fict_ не изменяется и имеется в чекпоинте с самой ее инициализации

    MPI_Request req[3 * (numProcsAlive - 1)];
    // Собираем контрольную точку в процессе-мастере
    int offset = 1;
    int reqI = 0;
    for (int i = masterProc + 1; i < numProcsAlive; i++) {
      if (procsAlive[i]) {
        MPI_Irecv(&cp_ex[nrows * offset][0], nrows * NY, MPI_FLOAT, i, 1, MPI_COMM_WORLD, &req[reqI++]);
        MPI_Irecv(&cp_ey[nrows * offset][0], nrows * NY, MPI_FLOAT, i, 2, MPI_COMM_WORLD, &req[reqI++]);
        MPI_Irecv(&cp_hz[nrows * offset][0], nrows * NY, MPI_FLOAT, i, 3, MPI_COMM_WORLD, &req[reqI++]);
        offset++;
      }
    }
    MPI_Waitall(3 * (numProcsAlive - 1), req, NULL);
    // Отправляем собранную контрольную точку всем процессам
    reqI = 0;
    for (int i = masterProc + 1; i < numProcsAlive; i++) {
      if (procsAlive[i]) {
        MPI_Isend(&cp_ex[0][0], NX * NY, MPI_FLOAT, i, 1, MPI_COMM_WORLD, &req[reqI++]);
        MPI_Isend(&cp_ey[0][0], NX * NY, MPI_FLOAT, i, 2, MPI_COMM_WORLD, &req[reqI++]);
        MPI_Isend(&cp_hz[0][0], NX * NY, MPI_FLOAT, i, 3, MPI_COMM_WORLD, &req[reqI++]);
      }
    }
    MPI_Waitall(3 * (numProcsAlive - 1), req, NULL);
  } else {
    // отправляем свою часть контрольной точки
    MPI_Request req[3];
    MPI_Isend(&(**ex)[0][0], nrows * NY, MPI_FLOAT, masterProc, 1, MPI_COMM_WORLD, &req[0]);
    MPI_Isend(&(**ey)[1][0], nrows * NY, MPI_FLOAT, masterProc, 2, MPI_COMM_WORLD, &req[1]);
    MPI_Isend(&(**hz)[1][0], nrows * NY, MPI_FLOAT, masterProc, 3, MPI_COMM_WORLD, &req[2]);
    MPI_Waitall(3, req, NULL);
    // получаем всю контрольную точку
    MPI_Irecv(&cp_ex[0][0], NX * NY, MPI_FLOAT, masterProc, 1, MPI_COMM_WORLD, &req[0]);
    MPI_Irecv(&cp_ey[0][0], NX * NY, MPI_FLOAT, masterProc, 2, MPI_COMM_WORLD, &req[1]);
    MPI_Irecv(&cp_hz[0][0], NX * NY, MPI_FLOAT, masterProc, 3, MPI_COMM_WORLD, &req[2]);
    MPI_Waitall(3, req, NULL);
  }
}

void checkpoint_restore(int *t,
                        float (**ex)[nrows][NY],
                        float (**ey)[nrows + 2][NY],
                        float (**hz)[nrows + 2][NY],
                        float (**_fict_)[TMAX]
){

}

/* Каждый процесс работает только со своей частью матрицы. */
void reinit(
  // TODO: копировать данные из чекпоинта, заполнять nrows;
  float (**ex)[nrows][NY], // ex не нужны теневые грани
  float (**ey)[nrows + 2][NY], // ey и hz нужны теневые грани
  float (**hz)[nrows + 2][NY], //
  float (**_fict_)[TMAX]
) {
  if (*ex != NULL) free(*ex);
  if (*ey != NULL) free(*ey);
  if (*hz != NULL) free(*hz);
  if (*_fict_ != NULL) free(*_fict_);

  // считаем живые процессы и устанавливаем prevRank, nextRank
  numProcsAlive = 0;
  int setPrevRank = 0;
  int setNextRank = 0;
  workRank = 0;

  for (int i = 0; i < numtasks; i++) {
    if (procsAlive[i]) { 
      numProcsAlive++;
      if (i < rank) {
        workRank++;
        prevRank = i;
        setPrevRank = 1;
      }
      if (i > rank && !setNextRank) {
        nextRank = i;
        setNextRank = 1;
      }
    }
  }
  if (!setPrevRank) prevRank = -1;
  if (!setNextRank) nextRank = -1;

  startrow = (workRank * NX) / numProcsAlive;
  lastrow = ((workRank + 1) * NX / numProcsAlive) - 1;
  nrows = lastrow - startrow + 1;

  *ex = malloc((nrows) * NY * sizeof(float));
  *ey = malloc((nrows + 2) * NY * sizeof(float));
  *hz = malloc((nrows + 2) * NY * sizeof(float));
  *_fict_  = malloc(TMAX * sizeof(float));

  // с этого момента мы должны копиросать чекпоинт в локальные матрицы
  int i, j;
  for (i = 0; i < TMAX; i++)
    (**_fict_)[i] = cp__fict_[i];

  for (i = 1; i <= nrows; i++)
    for (j = 0; j < NY; j++) {
      (**ex)[i - 1][j] = cp_ex[startrow + (i - 1)][j];
      (**ey)[i][j] = cp_ey[startrow + (i - 1)][j];
      (**hz)[i][j] = cp_hz[startrow + (i - 1)][j];
    }

  for (j = 0; j < NY; j++) // инициализируем теневые грани
  {
    (**ey)[0][j] = cp_ey[startrow - 1][j];
    (**hz)[0][j] = cp_hz[startrow - 1][j];
    (**ey)[nrows + 1][j] = cp_ey[lastrow + 1][j];
    (**hz)[nrows + 1][j] = cp_hz[lastrow + 1][j];
  }
}

// основные вычисления происходят здесь
static void kernel_fdtd_2d(float (**ex)[nrows][NY],
                           float (**ey)[nrows + 2][NY], 
                           float (**hz)[nrows + 2][NY],
                           float (**_fict_)[TMAX]
){
  MPI_Request req[4];
  MPI_Status status[4];
  int t, i, j;
  int nExchanges, n;
  int II, shift;
  int wasCheckpoint = 0;

  for (t = 0; t < TMAX; t++) {
    if (rank == 0) printf("%d\n", t);

    if (t % CHECKPOINT_ITERATIONS == 0) sync_checkpoint(t, ex, ey, hz, _fict_);
    if (t == 37 && !wasCheckpoint) {
      wasCheckpoint = 1;
      t = cp_t;
      reinit(ex, ey, hz, _fict_);
    }

    // начат участок ey
    if (workRank == 0)
      for (j = 0; j < NY; j++)
        (**ey)[1][j] = (**_fict_)[t];
    else
      for (j = 0; j < NY; j++)
        (**ey)[1][j] = (**ey)[1][j] - 0.5f * ((**hz)[1][j] - (**hz)[0][j]);

    for (i = 2; i <= nrows; i++) {
      for (j = 0; j < NY; j++)
        (**ey)[i][j] = (**ey)[i][j] - 0.5f * ((**hz)[i][j] - (**hz)[i - 1][j]);
    }
    // завершен участок ey
    // начат участок ex
    for (i = 0; i < nrows; i++) { // ex не имеет теневых граней
      for (j = 1; j < NY; j++)
        (**ex)[i][j] = (**ex)[i][j] - 0.5f * ((**hz)[i + 1][j] - (**hz)[i + 1][j - 1]);
    }
    // завершен участок ex

    // начинаем синхронизировать ey

    if (workRank != 0) {
      MPI_Irecv(&(**ey)[0][0], NY, MPI_FLOAT, prevRank, 'e' + 'y' + 1,
                MPI_COMM_WORLD, &req[0]);
    }


    if (workRank != 0) {
      MPI_Isend(&(**ey)[1][0], NY, MPI_FLOAT, prevRank, 'e' + 'y' + 2,
                MPI_COMM_WORLD, &req[1]);
    }

    if (workRank != numProcsAlive - 1) {
      MPI_Isend(&(**ey)[nrows][0], NY, MPI_FLOAT, nextRank, 'e' + 'y' + 1,
                MPI_COMM_WORLD, &req[2]);
    }

    if (workRank != numProcsAlive - 1) {
      MPI_Irecv(&(**ey)[nrows + 1][0], NY, MPI_FLOAT, nextRank, 'e' + 'y' + 2,
                MPI_COMM_WORLD, &req[3]);
    }

    II = 4;
    shift = 0;
    if (numProcsAlive - 1 == 0) {
      II = 0;
      shift = 0;
    } // если существует всего 1 MPI процесс
    else if (workRank == 0) {
      II = 2;
      shift = 2;
    } else if (workRank == numProcsAlive - 1) {
      II = 2;
      shift = 0;
    }

    MPI_Waitall(II, &req[shift], &status[0]);
    // закончили синхронизацию ey
    // начат участок hz
    for (i = 1; i <= nrows - 1; i++) {
      for (j = 0; j < NY - 1; j++)
        (**hz)[i][j] = (**hz)[i][j] - 0.7f * ((**ex)[i - 1][j + 1] 
                                    - (**ex)[i - 1][j] 
                                    + (**ey)[i + 1][j] 
                                    - (**ey)[i][j]);
    }

    if (workRank != numProcsAlive - 1) {
      for (j = 0; j < NY - 1; j++)
        (**hz)[nrows][j] = (**hz)[nrows][j] - 0.7f * ((**ex)[nrows - 1][j + 1] 
                                            - (**ex)[nrows - 1][j] 
                                            + (**ey)[nrows + 1][j] 
                                            - (**ey)[i][j]);
    }
    // завершен участок hz

    // начинаем синхронизацию hz
    if (workRank != 0) {
      MPI_Irecv(&(**hz)[0][0], NY, MPI_FLOAT, prevRank, 'h' + 'z' + 1,
                MPI_COMM_WORLD, &req[0]);
    }

    if (workRank != 0) {
      MPI_Isend(&(**hz)[1][0], NY, MPI_FLOAT, prevRank, 'h' + 'z' + 2,
                MPI_COMM_WORLD, &req[1]);
    }

    if (workRank != numProcsAlive - 1) {
      MPI_Isend(&(**hz)[nrows][0], NY, MPI_FLOAT, nextRank, 'h' + 'z' + 1,
                MPI_COMM_WORLD, &req[2]);
    }

    if (workRank != numProcsAlive - 1) {
      MPI_Irecv(&(**hz)[nrows + 1][0], NY, MPI_FLOAT, nextRank, 'h' + 'z' + 2,
                MPI_COMM_WORLD, &req[3]);
    }

    II = 2;
    shift = 0;
    if (numProcsAlive - 1 == 0) {
      II = 0;
      shift = 0;
    } // если существует всего 1 MPI процесс
    else if (workRank == 0) {
      II = 2;
      shift = 2;
    } else if (workRank == numProcsAlive - 1) {
      II = 2;
      shift = 0;
    }
    MPI_Waitall(II, &req[shift], &status[0]);
    // завершили синхронизацию hz
  }
}

int main(int argc, char **argv) {
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &numtasks);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Barrier(MPI_COMM_WORLD);
  int leader_rank = 0;

  procsAlive = malloc(numtasks * sizeof(int));
  for (int i = 0; i < numtasks; i++) procsAlive[i] = 1;

  float(*ex)[nrows][NY] = NULL;
  float(*ey)[nrows + 2][NY] = NULL;
  float(*hz)[nrows + 2][NY] = NULL;
  float(*_fict_)[TMAX] = NULL;

  init_checkpoint();
  reinit(&ex, &ey, &hz, &_fict_);
  if (rank == 0) printf("REINIT DONE\n");

  if (rank == leader_rank) bench_timer_start();

  kernel_fdtd_2d(&ex, &ey, &hz, &_fict_); // вычисления

  MPI_Barrier(MPI_COMM_WORLD); // ждем, чтобы измерения времени были максимально точными

  if (rank == leader_rank) {
    bench_timer_stop();
    bench_timer_print();
  }

  free((void *)ex);
  free((void *)ey);
  free((void *)hz);
  free((void *)_fict_);
  free(procsAlive);

  MPI_Finalize();
  return 0;
}

mpicc $1 && \
export OMPI_MCA_btl_vader_backing_directory=/tmp && \
mpirun --hostfile host.txt -np 64 a.out         
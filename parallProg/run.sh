mpicc $1 && \
echo 1 &&\
export OMPI_MCA_btl_vader_backing_directory=/tmp && \
echo 2 &&\
mpirun --hostfile host.txt -np $2 a.out &&\
echo 3 
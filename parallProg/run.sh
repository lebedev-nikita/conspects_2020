mpicc $1 && \
# echo 1 &&\
export OMPI_MCA_btl_vader_backing_directory=/tmp && \
echo _start_ &&\
mpirun --hostfile host.txt -np $2 a.out &&\
echo _finish_

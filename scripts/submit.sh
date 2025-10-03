module load python/3.8
module load R/4.2.2
dsq_path="/home/aguo28/dSQ-master"
##########################
# This is step1_dsq.sh
##########################

echo "Enter jobfile prefix"
read prefix

echo "Enter the number of jobs to run"
read n

##########################
# This is step3_submit.sh
##########################
for i in $(seq 1 ${n}); do
  sbatch ${prefix}_n$i.sh
done
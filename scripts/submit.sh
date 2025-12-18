module load python/3.8
module load R/4.2.2
dsq_path="/home/aguo28/dSQ-master"

##########################
# This is step1_dsq.sh
##########################

echo "Enter jobfile prefix. For example, prefix of joblist_n1.txt is joblist"
read prefix

echo "Enter the start number of jobs to run. Default is 1"
read n
n=${n:-1}

echo "Enter the end number of jobs to run"
read m

##########################
# This is step3_submit.sh
##########################
for i in $(seq ${n} ${m}); do
  sbatch ${prefix}_n$i.sh
done
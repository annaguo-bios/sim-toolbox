module load python/3.8
module load R/4.2.2
dsq_path="/home/aguo28/dSQ-master"
##########################
# This is step1_dsq.sh
##########################

# Prompt user for input
echo "Enter the name for dsq job:"
read sim_input

echo "Enter jobfile prefix"
read prefix

echo "Enter the number of jobs to run"
read n

for i in $(seq 1 ${n}); do
    python "$dsq_path/dSQ.py" --job-file ${prefix}_n$i.txt --batch-file ${prefix}_n$i.sh --job-name "${sim_input}-${prefix}-$i" --mail-type ALL -o dsq/dsq-jobfile-%A_%a-%N.out
done


##########################
# This is step2_modify.sh
##########################
dsq_path="/home/aguo28/dSQ-master"

# Default partition(s)
default_partition="day-long-cpu,short-cpu,empire"

# Ask user if default is ok
echo "Default partition is: $default_partition"
read -p "Is this partition ok? (y/n): " confirm

if [[ "$confirm" == "y" || "$confirm" == "Y" ]]; then
    partition=$default_partition
else
    read -p "Enter the partition you want to use: " partition
fi

for i in $(seq 1 ${n}); do
  # add "python" at the last line
  sed -i 's@'"$dsq_path"'/dSQBatch.py@python '"$dsq_path"'/dSQBatch.py@' "${prefix}_n$i.sh"

  # add email and packages
  sed -i '/#SBATCH --mail-type "ALL"/a #SBATCH --mail-user=anna.guo@emory.edu\n#SBATCH --mem 1G\nmodule load python/3.8\nmodule load R/4.2.2' ${prefix}_n$i.sh

  # specify partition name (user-confirmed or input)
  sed -i "1a #SBATCH --partition=${partition}" ${prefix}_n$i.sh
done


##########################
# This is step3_submit.sh
##########################
for i in $(seq 1 ${n}); do
  sbatch ${prefix}_n$i.sh
done
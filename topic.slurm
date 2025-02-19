#!/bin/bash

#SBATCH --job-name=trim_map             # Job name
#SBATCH --output=%j.out        # Output file
#SBATCH --error=%j.err         # Error file
#SBATCH --ntasks=1                      # Number of tasks
#SBATCH --cpus-per-task=4              # Number of CPU cores per task
#SBATCH --mem=15GB                       # Total memory limit
#SBATCH --time=08:10:00                 # Time limit hrs:min:sec
#SBATCH --account=lp_edu_comparativegenomics
#SBATCH --clusters=genius
#SBATCH --partition=batch

module load cluster/genius/batch
module load Python/3.11.5-GCCcore-13.2.0

source /data/leuven/359/vsc35904/KULdatathon2025/datathon_env/bin/activate
echo "before"
python /data/leuven/359/vsc35904/KULdatathon2025/topic_tagging.py
echo "after"
deactivate
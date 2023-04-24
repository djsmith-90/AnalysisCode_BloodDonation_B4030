#!/bin/bash    
                                                                
#SBATCH --job-name=NARMICE_mum_exposureMNAR
#SBATCH --partition=compute
#SBATCH --nodes=1 # This is the number of nodes required for the job
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --time=4-00:00:00 # (day-)hour:minute:second per job
#SBATCH --mem=5G
#SBATCH --array=1
#SBATCH --error=/user/home/ds16565/BloodDonation/Results_mum/Error_exposureMNAR.txt # Error files (if any)
#SBATCH --output=/user/home/ds16565/BloodDonation/Results_mum/Output_exposureMNAR.txt # SLURM output file
#SBATCH --account=sscm013902


# Change directory to save output in
cd "/user/home/ds16565/BloodDonation/Results_mum"

# Some other useful information
echo Running on hist "$(hostname)"
echo Time is "$(date)"
echo Directory is "$(pwd)"
echo Slurm job ID is "${SLURM_JOBID}"
echo This job runs on the following machines:
echo "${SLURM_JOB_NODELIST}"


# Load relevant software (no need to do this, as running local version of R-4.1.0)
#module load lang/r/4.1.2-gcc

# But do need to load the 'gcc' package to run the local R version
module load lang/gcc/9.1.0

# Get the array ID
i=${SLURM_ARRAY_TASK_ID}

# Run the R script
srun /user/home/ds16565/R-4.1.0/bin/Rscript /user/home/ds16565/BloodDonation/Scripts/mum_NARMICE_exposureMNAR.r ${i}


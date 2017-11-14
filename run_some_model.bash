#!/bin/bash
#
#SBATCH --job-name=lmer_model
#SBATCH --output=output/lmermod_%A.log
#SBATCH --workdir=./
#
#SBATCH --time=5-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=5000
#SBATCH --partition=long,longfat

echo "Running $1"

module load R 
srun Rscript --verbose -e "source('loadIndivData.R'); source('${1}');"

#!/bin/bash

echo "starting PC_ECA_run.sh"

# Submit the pipeline as a background process with ./run.sh
module load R # Uncomment if R is an environment module.
nohup nice -4 R CMD BATCH ./SCR_paper/Scripts/Master_scripts/PC_ECA_calculation_master.R &

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
 rm -f .RData

echo "ending PC_ECA_run.sh"
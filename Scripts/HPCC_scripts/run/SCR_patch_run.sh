#!/bin/bash

echo "starting SCR_patch_run.sh"

# Submit the pipeline as a background process with ./run.sh
module load R # Uncomment if R is an environment module.
nohup nice -4 R CMD BATCH ./SCR_paper/Scripts/Master_scripts/SCR_patch_level_master.R &

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
 rm -f .RData

echo "ending SCR_patch_run.sh"
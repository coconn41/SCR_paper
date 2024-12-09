#!/bin/bash

echo "starting SCR_landscape_run.sh"

# Submit the pipeline as a background process with ./run.sh
# module load R # Uncomment if R is an environment module.

Rscript ./SCR_paper/Scripts/Master_scripts/SCR_landscape_level_master.R

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
rm -f .RData

echo "ending SCR_landscape_run.sh"
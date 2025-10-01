#!/bin/bash

echo "starting graph4lg_run5.sh"

# Submit the pipeline as a background process with ./run.sh
# module load R # Uncomment if R is an environment module.

Rscript ./SCR_paper/Scripts/PC_graph4lg/Graphab_analysis_breakout_5.R

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
rm -f .RData

echo "ending graph4lg_run5.sh"
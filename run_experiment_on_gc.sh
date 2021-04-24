# create log file
export LOGFILE=shell.log

ts=$(date +%s)

# make directory for simulation results
echo "creating shell log file" >> $LOGFILE
echo "creating shell log file"
mkdir -p "simulation_results"


# invoke R script
echo "invoking R script, writing R log file" >> $LOGFILE
echo "invoking R script, writing R log file"
R CMD BATCH --slave master.R R-Output.Rout

# move log files into simulation results
echo "Rscript completed, retrieving lowest hierarchy folder for results" >> $LOGFILE
echo "Rscript completed, retrieving lowest hierarchy folder for results"
SIM=`ls -d simulation_results/*/`
echo "$SIM" >> $LOGFILE
echo "$SIM"

# move log files to results
echo "moving R-Log and copying shell log to simulation results" >> $LOGFILE
echo "moving R-Log and copying shell log to simulation results"
mv R-Output.Rout $SIM
cp $LOGFILE $SIM

# upload simulation results to bucket
echo "uploading to project bucket" >> $LOGFILE
echo "uploading to project bucket"
gsutil -m cp -r simulation_results/* gs://ai_collusion/$ts

# shutdown VM
# sudo poweroff
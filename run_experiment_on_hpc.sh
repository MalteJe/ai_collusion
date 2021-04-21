#!/bin/bash
#PBS -l select=1:ncpus=1:mem=5gb
#PBS -l walltime=01:00:00
#PBS -A "ai_collusion"


set -e

 
## Log-File definieren
export LOGFILE=$PBS_O_WORKDIR/$PBS_JOBNAME"."$PBS_JOBID".log"
 

##Scratch-Laufwerk definieren und erzeugen
SCRATCHDIR=/scratch_gs/$USER/$PBS_JOBID
mkdir -p "$SCRATCHDIR" 

 
##Information zum Start in das Log-File schreiben
cd $PBS_O_WORKDIR  
echo "$PBS_JOBID ($PBS_JOBNAME) @ `hostname` at `date` in "$RUNDIR" START" > $LOGFILE
echo "`date +"%d.%m.%Y-%T"`" >> $LOGFILE 

echo "working directory is" >> $LOGFILE
echo ~ >> $LOGFILE

echo "loading R module" >> $LOGFILE

##Software-Umgebung laden
module load R/4.0.3
 
echo "loaded R module, copying to scratch" >> $LOGFILE
 
##Daten vom Arbeitsverzeichnis auf das Scratch-Laufwerk kopieren
cp -r $PBS_O_WORKDIR/* $SCRATCHDIR/.
cd $SCRATCHDIR
rm $PBS_JOBNAME"."$PBS_JOBID".log"
 
echo "copied to scratch, R home directory is" >> $LOGFILE


##R-Aufruf

echo "invoking R script (master)" >> $LOGFILE
R CMD BATCH --slave master.R R-Output.Rout
 
echo "ran R script, copying from scratch to working directory (?)" >> $LOGFILE
 
##Daten zurück kopieren
cp -r "$SCRATCHDIR"/* $PBS_O_WORKDIR/.
cd $PBS_O_WORKDIR
 
echo "copied from scratch, writing echo in logfile"
echo "copied from scratch, writing echo in logfile" >> $LOGFILE

##Verfügbare Informationen zum Auftrag in das Log-File schreiben
echo >> $LOGFILE
qstat -f $PBS_JOBID >> $LOGFILE  
 
echo "$PBS_JOBID ($PBS_JOBNAME) @ `hostname` at `date` in "$RUNDIR" END" >> $LOGFILE
echo "`date +"%d.%m.%Y-%T"`" >> $LOGFILE
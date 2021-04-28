#!/bin/bash
#PBS -l select=1:ncpus=8:mem=6gb:arch=skylake
#PBS -l walltime=11:59:30
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
 
echo "copying to scratch" >> $LOGFILE
 
##Daten vom Arbeitsverzeichnis auf das Scratch-Laufwerk kopieren
cp -r $PBS_O_WORKDIR/* $SCRATCHDIR/.
cd $SCRATCHDIR
rm $PBS_JOBNAME"."$PBS_JOBID".log"

##R-Aufruf

echo "invoking R script (master)" >> $LOGFILE
R CMD BATCH --slave master.R R-Output.Rout
 
echo "ran R script" >> $LOGFILE
 
##Daten zurück kopieren
## cp -r "$SCRATCHDIR"/* $PBS_O_WORKDIR/.
## cd $PBS_O_WORKDIR

##Daten auf personal gpfs kopieren
echo "creating path to perssonal gpfs directory" >> $LOGFILE

GPFSDIR=/gpfs/project/majes102/$PBS_JOBID

echo "attempting to create directory: $GPFSDIR" >> $LOGFILE

mkdir -p "$GPFSDIR" 

echo "copying to newly created gpfs directory" >> $LOGFILE

cp -r "$SCRATCHDIR"/* $GPFSDIR/.
cd $GPFSDIR

##Verfügbare Informationen zum Auftrag in das Log-File schreiben
echo >> $LOGFILE
qstat -f $PBS_JOBID >> $LOGFILE  
 
echo "$PBS_JOBID ($PBS_JOBNAME) @ `hostname` at `date` in "$RUNDIR" END" >> $LOGFILE
echo "`date +"%d.%m.%Y-%T"`" >> $LOGFILE
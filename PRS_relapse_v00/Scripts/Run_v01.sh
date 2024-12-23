## Perform analysis on PRS and relapse
## Date: Nov 20, 2024

# Set working directory
dir=~/Downloads/PRS_relapse-main/PRS_relapse_v00                              # directory where scripts are located
cd $dir_path/$dir

# Create directories to save output
mkdir Data
mkdir -p results/Figures
mkdir -p results/Tables

# 1) Check dependencies
Rscript ./Scripts/dependencies.R

# 2) Prepare dataset
# For usage, see: Rscript ./Scripts/Format_Px_data_01.R --help
# NOTE: processed file will be saved as './Data/patient_data.Rdat' file

# Indicate file and variable column names
path_prefix=''                                                                # path to server, can be set to be empty
file="$path_prefix/GEN_TEC/Output/Dataset/PRS_pheno_clinical_20231025.dat"    # path to PRS-phenotype file
PRS=PRS_gwas
nPCs=3
stage=rmh
prim_tx=prim_beh
event=recpro
time=timerec
age=agediag
hist=tumortype
vasc=vasc

Rscript ./Scripts/Format_Px_data_01.R --file $file --PRS $PRS --nPCs $nPCs --stage $stage --prim_tx $prim_tx --event $event --time $time --age $age --hist $hist --vasc $vasc 

# 2) Plot PRS
Rscript ./Scripts/Plot_PRS_v01.R

# 3) Run analyses

## Kaplan-Meier survival plots by PRS(ses)
Rscript Scripts/KMplots_PRSgwas_Relapse_01.R

## Assoc. with risk of relapse
Rscript Scripts/ANL_PRSgwas_Relapse_01.R



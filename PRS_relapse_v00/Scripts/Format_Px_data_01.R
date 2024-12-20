#' ---
#' title: 'Format Px data: PRS, phenotype and clinical variables'
#' author: "Emilio Ugalde"
#' date: "15/Nov/2024"
#' output:
#'   html_document: default
#'   pdf_document: default
#' ---
#' 
suppressMessages(library("optparse"))
suppressMessages(library("data.table"))

option_list = list(make_option("--file", action="store", default=NA, type='character', help="Path/name of cohort file [required]"),
make_option("--PRS", action="store", default=NA, type='character', help="Column name: PRS score [required]"),
make_option("--nPCs", action="store", default=3, type='double', help="No. of PCs named as PC1, PC2, etc. (default=3) [required]"),
make_option("--stage", action="store", default=NA, type='character', help="Column name: Clinical stage (CS) ('CS I', ...) [optional]"),
make_option("--prim_tx", action="store", default=NA, type='character', help="Column name: Primary treatment ('Surveillance', ...) [optional]"),
make_option("--event", action="store", default=NA, type='character', help="Column name: Clinical relapse (0=No relpase, 1=Relapse) [required]"),
make_option("--time", action="store", default=NA, type='character', help="Column name: Time between orchiectomy and relapse, in years [required]"),
make_option("--age", action="store", default=NA, type='character', help="Column name: age at diagnosis, in years [required]"),
make_option("--hist", action="store", default=NA, type='character', help="Column name: histology (S=Seminoma, NS=non-seminoma) [required]"),
make_option("--vasc", action="store", default=NA, type='character', help="Column name: Vascular infiltration (1=yes, 0=no) [required]")
)

opt = parse_args(OptionParser(option_list=option_list))

#' 
#' # Working directory
## -----------------------------------------------------------------------------
path_prefix=""
wd="."
setwd(paste0(path_prefix, wd))

#' 
#' ## OUTPUT
## -----------------------------------------------------------------------------
file_out = "./Data/patient_data.Rdat"

#' 
#' 
#' ## INPUT
## -----------------------------------------------------------------------------

# read in data set
in_dat <- fread(opt$file)

# Check columns names are present in data
options=as.character(unlist(opt))
# Remove NAs
options=options[!is.na(options) & options!="NA"]
# skip 1, 3, and last: file and nPCs, "help" argument
colin <- options[c(2,4:(length(options)-1) ) ]

if(all(colin%in%names(in_dat) )==F ){
  colin_f <- colin[which(!colin%in%names(in_dat))]
  print(paste0("Variable names don't match file: ", paste(colin_f, collapse=", ")) )
  print(paste0("Argument(s) no. ", paste0(which(!colin%in%names(in_dat)), collapse= ", ")) )
  q()
}

# get variables
clin = data.frame(PRS_gwas=subset(in_dat, select=opt$PRS),
		  recpro=subset(in_dat, select=opt$event ),
		  timerec=subset(in_dat, select=opt$time),
		  agediag=subset(in_dat, select=opt$age),
		  tumortype=subset(in_dat, select=opt$hist),
		  vasc=subset(in_dat, select=opt$vasc)
		)
names(clin) = c("PRS_gwas", "recpro", "timerec", "agediag", "tumortype", "vasc")
print("Number of ind. in dataset:")
nrow(clin)

# add PCs	
PCs <- paste0("PC", 1:opt$nPCs)
pcs <- in_dat[, ..PCs]
print(paste0("Addin PCs: ", ncol(pcs) ))
clin <- cbind(clin, pcs)

# Stage variable
if(is.na(opt$stage)==F){ 
print("Adding 'stage' variable")
clin <- cbind(clin, subset(in_dat, select=opt$stage))
clin$rmh <- as.factor(clin[, opt$stage])
str(clin$rmh)
# Error if stage I not coded as "CS I" 
if(!"CS I"%in%levels(clin$rmh ) ) {
print("Clinical stage I should be coded as: CS I")
q()
}
}

# Surveillance variable (primary treatment)
if(is.na(opt$prim_tx)==F){ 
print("Adding 'primary tx' variable")
clin <- cbind(clin, subset(in_dat, select=opt$prim_tx))
clin$prim_beh <- as.factor(clin[, opt$prim_tx])
str(clin$prim_beh)
# Error if "Surveillance" is coded differently.
if(!"Surveillance"%in%levels(clin$prim_beh ) ) {
print("Primary treatment should include category named 'Surveillance' ")
q()
 }
}

#' 
#' ## Format PHENOTYPE variables
## -----------------------------------------------------------------------------

# Code clinical stage as binary (e.g. CI vs. other)
if(is.na(opt$stage)==F){ 
clin$rmh_bn <- ifelse(clin$rmh=="CS I", "No" , "Yes" )
# Mark missing 
clin$rmh_bn <- factor(ifelse(clin$rmh=="Missing", "Missing", clin$rmh_bn ), levels=c("No", "Yes", "Missing")  ) 
}
#' 
#' ### Format relapse as categorical
clin$x_recpro <- as.factor(clin$recpro)
levels(clin$x_recpro) <- c("No", "Yes")
# Mark missing 
clin$x_recpro <- factor(ifelse(is.na(clin$recpro)==T, "Missing", as.character(clin$x_recpro)), levels=c("No", "Yes", "Missing") )

#' ### Check tumortype levels
#' 
clin$tumortype <- as.factor(clin$tumortype)
hist_names=levels(clin$tumortype)
# To match coding in SWENOTECA coding
hist_levels=c("Seminom", "Nonseminom")
new_levels=hist_levels[]
# Rename
clin$tumortype <- as.factor(ifelse(clin$tumortype==grep("^sem", hist_names, ignore.case=T, value=T), hist_levels[1], 
                         ifelse(clin$tumortype==grep("^non", hist_names, ignore.case=T, value=T), hist_levels[2], as.character(clin$tumortype) )))

#' 
#' ### Format vascular invasion
clin$vasc_x <- as.factor(clin$vasc)
levels(clin$vasc_x) <- c("Absent", "Present")
# Mark missing 
clin$vasc_x <- factor(ifelse(is.na(clin$vasc_x)==T, "Missing", as.character(clin$vasc_x)), levels=c("Absent", "Present", "Missing") )


#' 
#' ## PRS
## -----------------------------------------------------------------------------

## Scale PRS using full cohort (case-only)
clin$PRS_gwas_SDcases <- as.numeric(scale(clin$PRS_gwas))

#' ### As binary: by median
gwas_median <- median(clin$PRS_gwas)
clin$PRS_gwas_median <- factor(ifelse(clin$PRS_gwas<gwas_median, "<median", ">=median"))

#' 
#' ### As binary: by mean
gwas_mean <- mean(clin$PRS_gwas)
clin$PRS_gwas_mean <- factor(ifelse(clin$PRS_gwas<gwas_mean, "<mean", ">=mean"))


#' ### As tertiles
tertiles <- quantile(clin$PRS_gwas, probs=0:3/3, na.rm=T)
clin$PRS_gwas_tert <- factor(cut(clin$PRS_gwas, tertiles, include.lowest=T)) 
# Re-name levels
clin$PRS_gwas_tert_c <- clin$PRS_gwas_tert
levels(clin$PRS_gwas_tert_c) <- c("Low", "Intermediate", "High")

#' ### Binary: Higher vs lowest tertiles 
clin$PRS_gwas_th <- as.factor(ifelse( clin$PRS_gwas_tert_c=="Low", "Low", "High") )
clin$PRS_gwas_th <- relevel(clin$PRS_gwas_th, ref="Low")


# ## Print data structure
print("Dataset structure: ")
str(clin)

#' ## Save data set
## -----------------------------------------------------------------------------
save(clin, file=file_out)



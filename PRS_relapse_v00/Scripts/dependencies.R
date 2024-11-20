# Rscript

# Devtool package
if(!"devtools"%in%installed.packages()[,"Package"]) install.packages("devtools")
## ggkm package
if(!"ggkm"%in%installed.packages()[,"Package"]) {
library(devtools)
install_github("michaelway/ggkm")
}

# List of other dependencies (R packages required)
list.of.packages <- c("data.table", "optparse","ggplot2", "dplyr",
			"survival", "survminer", "broom"
			)
			
# packages not installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Install any missing
if(length(new.packages)) install.packages(new.packages)

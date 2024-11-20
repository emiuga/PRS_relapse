load('./Data/patient_data.Rdat')

library(ggplot2)
library(dplyr)

clin <- clin[is.na(clin$tumortype)==F, ]

p <- clin %>%
  ggplot( aes(x=PRS_gwas, fill=tumortype )) + 
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080"))  

png(filename="./results/Figures/Plot.PRS_by_histology.png", width = 8, height = 6, units = "in", res = 400)
p
dev.off()

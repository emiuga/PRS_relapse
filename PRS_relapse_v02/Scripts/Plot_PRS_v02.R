load('./Data/patient_data.Rdat')

library(ggplot2)
library(dplyr)

clin <- clin[clin$tumortype%in%c("Seminom", "Nonseminom"), ]

p <- clin %>%
  ggplot( aes(x=PRS_gwas_SDcases, fill=tumortype )) + 
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080"))  

png(filename="./results/Figures/Plot.PRS_by_histology.png", width = 8, height = 6, units = "in", res = 400)
p
dev.off()

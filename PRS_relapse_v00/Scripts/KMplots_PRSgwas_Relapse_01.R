#' ---
#' title: 'Kaplan-Meier plots for the association btw testis PRS (gwas) and risk of relapse in CS I tumor patients under SURVEILLANCE'
#' author: Emilio Ugalde
#' Date: 2024-11-22
#' ---
#' 
## ----setup, include=FALSE, echo=F---------------------------------------------
suppressMessages(library("survival"))
suppressMessages(library("survminer"))

#' Working directory
## ----echo=T-------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
wd <- args[1]
if(is.na(wd)==T) wd=""

#' ## INPUT
# load dataset generated by "Format_Px_data_*.R" scripts
load(paste0(wd, './Data/patient_data.Rdat'))
date_a <- format(Sys.Date(), "%Y%m%d")

#' # Cox analysis: in stage I cases under surveillance
## ----echo=T-------------------------------------------------------------------

# Subset
if("rmh"%in%names(clin)) clin <- subset(clin, rmh=="CS I")
if("prim_beh"%in%names(clin)) clin <- subset(clin, prim_beh=="Surveillance")
data_s <- clin

#' Set OUTPUT
## ----echo=T-------------------------------------------------------------------
dir_out <- paste0(wd, "results/Figures/")

#' 
#' - Kaplan-Meier plots
## -----------------------------------------------------------------------------
# sufix (to add overall, seminom, and non-seminom)
KM_mean = "KM_plot_CSI_surveillance_gwasPRS_mean"          # above mean PRS
KM_median = "KM_plot_CSI_surveillance_gwasPRS_median"      # above mean PRS
KM_tertiles = "KM_plot_CSI_surveillance_gwasPRS_tertiles"  # by PRS tertiles
KM_high = "KM_plot_CSI_surveillance_gwasPRS_high"          # >1st PRS tertile


#' # Generate plots

# Pair PRS variable names to output files
fac <- c("PRS_gwas_mean", "PRS_gwas_median", "PRS_gwas_tert_c", "PRS_gwas_th" )
KM_list <- list(KM_mean, KM_median, KM_tertiles, KM_high)

#' ## Get plot Y-axis limits based on full cohort
fit=survfit(survival::Surv(timerec, recpro) ~ 1 , data = data_s)
# lowest surv. prob.
ymin = round(min(fit$surv), 2) - 0.10

# Follow-up time
medtime = round(median(fit$time), 1)
if(medtime<3) xmax=3
if(medtime>=3) xmax=3
if(medtime>=5) xmax=5
if(medtime>=10) xmax=10

# Set axis limits
ylims = c(ymin, 1)
xlims = c(0, xmax)

#' ## PRS: overall
## -----------------------------------------------------------------------------

plots_out <- as.list(paste0("plot", 1:length(fac)))

for(i in 1:length(fac)){
  #i=1
  fit <- survfit(formula(paste("Surv(timerec, recpro) ~", fac[i], collapse = "") ), data = data_s)
  plots_out[[i]] <- ggsurvplot(fit,
                               data = data_s,  # data used to fit survival curves. 
                               censor=T,
                               ncensor.plot=F,
                               ylab="Relapse-free survival",
                               xlab = "Time since diagnosis in years",
                               legend.title="PRS",
                               legend.labs = levels(data_s[, fac[i] ]),
                               risk.table = TRUE,  # show risk table.
                               pval = TRUE,  # show p-value of log-rank test.
                               pval.size=4,
                               pval.method = TRUE,
                               pval.coord = c(xmax-1,0.975),  # pvalue coordinates
                               pval.method.coord = c(xmax-1,0.99),
                               conf.int = F,  # show confidence intervals for point estimaes of survival curves.
                               ylim = ylims,  # present narrower Y axis, but not affect survival estimates.
                               xlim = xlims,
                               break.time.by = 1,  # break X axis in time intervals by 2.
                               ggtheme = theme_minimal(),  # customize plot and risk table with a theme.
                               risk.table.y.text.col = T,  # colour risk table text annotations.
                               risk.table.y.text = FALSE,  # show bars instead of names in text annotations in legend of risk table
                               fontsize=2
  )

  file=paste0(KM_list[i], ".Overall.", date_a, ".png")
  print(file)
  
  png(filename = paste0(dir_out, file), width = 8, height = 6, units = "in", res = 600 )
  print(plots_out[[i]])
  dev.off()
}

#' ## PRS: Seminoma
## -----------------------------------------------------------------------------
data_s <- subset(clin, tumortype=="Seminom")
plots_out <- as.list(paste0("plot", 1:length(fac)))

for(i in 1:length(fac)){
  #i=1
  fit <- survfit(formula(paste("Surv(timerec, recpro) ~", fac[i], collapse = "") ), data = data_s)
  plots_out[[i]] <- ggsurvplot(fit,
                               data = data_s,  # data used to fit survival curves. 
                               censor=T,
                               ncensor.plot=F,
                               ylab="Relapse-free survival",
                               xlab = "Time since diagnosis in years",
                               legend.title="PRS",
                               legend.labs = levels(data_s[, fac[i] ]),
                               risk.table = TRUE,  # show risk table.
                               pval = TRUE,  # show p-value of log-rank test.
                               pval.size=4,
                               pval.method = TRUE,
                               pval.coord = c(xmax-1,0.975),  # pvalue coordinates
                               pval.method.coord = c(xmax-1,0.99),
                               conf.int = F,  # show confidence intervals for point estimaes of survival curves.
                               ylim = ylims,  # present narrower Y axis, but not affect survival estimates.
                               xlim = xlims,
                               break.time.by = 1,  # break X axis in time intervals by 2.
                               ggtheme = theme_minimal(),  # customize plot and risk table with a theme.
                               risk.table.y.text.col = T,  # colour risk table text annotations.
                               risk.table.y.text = FALSE,  # show bars instead of names in text annotations in legend of risk table
                               fontsize=2
  )
  
  file=paste0(KM_list[i], ".Seminoma.", date_a, ".png")
  print(file)
  
  png(filename = paste0(dir_out, file), width = 8, height = 6, units = "in", res = 600 )
  print(plots_out[[i]])
  dev.off()
}

#' ## PRS: Non-Seminoma
## -----------------------------------------------------------------------------
data_s <- subset(clin, tumortype=="Nonseminom")
plots_out <- as.list(paste0("plot", 1:length(fac)))

for(i in 1:length(fac)){
  #i=1
  fit <- survfit(formula(paste("Surv(timerec, recpro) ~", fac[i], collapse = "") ), data = data_s)
  plots_out[[i]] <- ggsurvplot(fit,
                               data = data_s,  # data used to fit survival curves. 
                               censor=T,
                               ncensor.plot=F,
                               ylab="Relapse-free survival",
                               xlab = "Time since diagnosis in years",
                               legend.title="PRS",
                               legend.labs = levels(data_s[, fac[i] ]),
                               risk.table = TRUE,  # show risk table.
                               pval = TRUE,  # show p-value of log-rank test.
                               pval.size=4,
                               pval.method = TRUE,
                               pval.coord = c(xmax-1,0.975),  # pvalue coordinates
                               pval.method.coord = c(xmax-1,0.99),
                               conf.int = F,  # show confidence intervals for point estimaes of survival curves.
                               ylim = ylims,  # present narrower Y axis, but not affect survival estimates.
                               xlim = xlims,
                               break.time.by = 1,  # break X axis in time intervals by 2.
                               ggtheme = theme_minimal(),  # customize plot and risk table with a theme.
                               risk.table.y.text.col = T,  # colour risk table text annotations.
                               risk.table.y.text = FALSE,  # show bars instead of names in text annotations in legend of risk table
                               fontsize=2
  )
  
  file=paste0(KM_list[i], ".Non-Seminoma", date_a, ".png")
  print(file)
  
  png(filename = paste0(dir_out, file), width = 8, height = 6, units = "in", res = 600 )
  print(plots_out[[i]])
  dev.off()
}



rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script analyzes the income poverty over time. This is similar to
## RScript03. This includes analysis of income poverty by gender of
## head of household, marital status, etc. The data was created by RScript4.
########################################################################

########################################################################
## Run Path definition file                                           ##
########################################################################
RScriptPath <- '~/Project_Recession/RScripts_Recession/'
DataPath <- '~/Project_Recession/Data/'
RDataPath <- '~/Project_Recession/RData/'
PlotPath <- '~/Project_Recession/Plots/'
Filename.Header <- paste('~/RScripts/HeaderFile_lmcg.R', sep = '')
source(Filename.Header)
source(paste(RScriptPath, 'fn_Library_Recession.R', sep = ''))
########################################################################
Today <- Sys.Date()

########################################################################
## load income poverty data
########################################################################
#Filename <- paste0(RDataPath, 'Data_forIncPov_byRace.RData')
#load(file = Filename)

Filename <- paste0(RDataPath, 'Data_forIncPov.RData')
load(file = Filename)

ssuids <- unique(Data_forIncPov$ssuid)
#Data_Sub <- subset(Data_forIncPov, yearmon == 'Jun 2008')
Data_Sub <- subset(Data_forIncPov, ssuid %in% ssuids[1:40])
head(Data_forIncPov)

SplitByssuid <- split(x = Data_Sub, f = as.factor(Data_Sub$ssuid))
Data_Sub$FPL100_num_nobaseline <- do.call(what = c, args = lapply(X = SplitByssuid,
                                                      FUN = normalize_baseline, Colname = 'FPL100_num'))
########################################################################
## To plot longitudinal data for Yajuan
########################################################################
## Filename.plot <- paste0(PlotPath, 'Plot_40HH.pdf')
## pdf(file = Filename.plot, onefile = T)
## Plot1 <- qplot() + geom_line(aes(x = as.factor(yearmon), y = FPL100_num_Norm, group = ssuid, color = ssuid), data = Data_Sub) + 
##   facet_wrap(~disb_wrk_ageR2) + 
##   xlab(label = '') + ylab(label = '') +
##   theme(axis.text.x = element_text(angle=90, vjust=1), 
##         legend.position="none")

## Plot2 <- qplot() + geom_line(aes(x = as.factor(yearmon), y = FPL100_num, group = ssuid, color = ssuid), data = Data_Sub) + 
##   facet_wrap(~disb_wrk_ageR2) + 
##   geom_hline(yintercept = 1)+ xlab(label = '') + ylab(label = '') +
##   theme(axis.text.x = element_text(angle=90, vjust=1), 
##         legend.position="none")
## Plot1
## Plot2
## dev.off()
## try(dev.off())
########################################################################

Data <- Data_forIncPov
#
########################################################################
## Linear Model of FPL 100
########################################################################
Model1 <- lm(FPL100_num ~ race, data = Data_forIncPov)
summary(Model1)
Model2 <- lm(FPL100_num ~ race + gender_ms, data = Data_forIncPov)
summary(Model2)
anova(Model1, Model2)
Model3 <- lm(FPL100_num ~ race + gender_ms +  disb_wrk_ageR2, data = Data_forIncPov)
summary(Model3)
anova(Model2, Model3)

#######################################################################
## Linear Model of normalized FPL 100 
########################################################################
## Model1 <- lm(FPL100 ~ yearmon + gender_ms + race + disb_wrk_ageR2, data = Data_Sub)
Model0_FPL100 <- lm(FPL100_num_Norm ~ yearmon, data = Data)
summary(Model0_FPL100)

Model1_FPL100 <- lm(FPL100_num_Norm ~ race, data = Data_forIncPov)
summary(Model1_FPL100)
Model2_FPL100 <- lm(FPL100_num_Norm ~ race + gender_ms, data = Data_forIncPov)
summary(Model2_FPL100)
anova(Model1_FPL100, Model2_FPL100)
Model3_FPL100 <- lm(FPL100_num_Norm ~ race + gender_ms +  disb_wrk_ageR2, data = Data_forIncPov)
summary(Model3_FPL100)
anova(Model2_FPL100, Model3_FPL100)
Model4_FPL100 <- lm(FPL100_num_Norm ~ gender_ms + race + disb_wrk_ageR2*race , data = Data_forIncPov)
summary(Model4_FPL100)
Model5_FPL100 <- lm(FPL100_num_Norm ~ gender_ms + race + disb_wrk_ageR2*gender_ms, data = Data_forIncPov)
summary(Model5_FPL100)
anova(Model5_FPL100)

Model6_FPL100 <- lm(FPL100_num_Norm ~ yearmon + gender_ms + race + disb_wrk_ageR2*gender_ms, data = Data_forIncPov)
summary(Model6_FPL100)

Model7_FPL100 <- lm(FPL100_num_Norm ~ yearmon + gender_ms + race + disb_wrk_ageR2*gender_ms +
                    FPL100_num_Norm_Lag, data = Data_forIncPov)
summary(Model7_FPL100)

## Plots <- diagPlot(model = Model6_FPL100)
## Filename.plot <- paste0(PlotPath, 'ResidualPlots_Model6_FPL100.pdf')
## pdf(file = Filename.plot)
## Plots[[1]]
## Plots[[2]]
## Plots[[3]]
## Plots[[4]]
## Plots[[5]]
## Plots[[6]]
## dev.off()

#######################################################################
## Linear Model of normalized FPL 200 
########################################################################
## Model0_FPL200 <- lm(FPL200_num_Norm ~ yearmon, data = Data_forIncPov)
## summary(Model0_FPL200)
## #pacf(residuals(Model0_FPL200), 10)

## Model1_FPL200 <- lm(FPL200_num_Norm ~ race, data = Data_forIncPov)
## summary(Model1_FPL200)
## Model2_FPL200 <- lm(FPL200_num_Norm ~ race + gender_ms, data = Data_forIncPov)
## summary(Model2_FPL200)
## anova(Model1_FPL200, Model2_FPL200)
## Model3_FPL200 <- lm(FPL200_num_Norm ~ race + gender_ms +  disb_wrk_ageR2, data = Data_forIncPov)
## summary(Model3_FPL200)
## anova(Model2_FPL200, Model3_FPL200)
## Model4_FPL200 <- lm(FPL200_num_Norm ~ gender_ms + race + disb_wrk_ageR2*race , data = Data_forIncPov)
## summary(Model4_FPL200)
## Model5_FPL200 <- lm(FPL200_num_Norm ~ gender_ms + race + disb_wrk_ageR2*gender_ms, data = Data_forIncPov)
## summary(Model5_FPL200)






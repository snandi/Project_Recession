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
PathPrefix <- '~/'
PathPrefix <- '/Users/patron/Documents/snandi/'
RScriptPath <- paste0(PathPrefix, 'Project_Recession/RScripts_Recession/')
DataPath <- paste0(PathPrefix, 'Project_Recession/Data/data_2015Dec/')
RDataPath <- paste0(PathPrefix, 'Project_Recession/RData/data_2015Dec/')
PlotPath <- paste0(PathPrefix, 'Project_Recession/Plots/')
Filename.Header <- paste0(PathPrefix, 'RScripts/HeaderFile_lmcg.R')
source(Filename.Header)

source(paste(RScriptPath, 'fn_Library_Recession.R', sep=''))
try(source('../../RScripts/fn_Library_SN.R'))
try(source('~/RScripts/fn_Library_SN.R'))

########################################################################
Today <- Sys.Date()

########################################################################
## load income poverty data
########################################################################
#Filename <- paste0(RDataPath, 'Data_forIncPov_byRace.RData')
#load(file = Filename)

##Filename <- paste0(RDataPath, 'Data_forIncPov.RData')
##load(file = Filename)
Filename <- paste0(RDataPath, 'Data_forIncPov_v3.RData')
load(file = Filename)

Data <- Data_forIncPov
Data$year <- substr(x = Data$yearqtr, start = 1, stop = 4)
Data$year <- as.factor(Data$year)

rm(Data_forIncPov)
#View(Data[,c('hhid', 'yearqtr', 'thtotinc', 'rhpov', 'adult_disb', 'FPL100_num', 'FPL100_num_Lag')])

#######################################################################
## Mixed Effects Model (MEM) of Income Poverty Ratio
########################################################################
library(lsmeans)

Data$wt <- Data$wt/1000

Num1_FPL100_wt_log <- lmer(log(FPL100_num) ~ 1 + yearqtr + gender_ms + race + 
                             race*adult_disb + 
                             adult_disb + wt + adult_disb*gender_ms + adult_disb*gender_ms + 
                             adult_disb*yearqtr + (1 | hhid), 
                           data = Data, REML = TRUE)
summary(Num1_FPL100_wt_log)
anova(Num1_FPL100_wt_log) ## 
Resid1_log <- as.data.frame(cbind(yearqtr = Data$yearqtr, resid = residuals(Num1_FPL100_wt_log)))
Resid1_logPlot <- qplot() + geom_point(aes(x = yearqtr, y = resid), data = Resid1_log)
xtable(anova(Num1_FPL100_wt_log))

LS1 <- lsmeans(Num1_FPL100_wt_log, pairwise ~ race*adult_disb)
Num1_gender <- lsmeans(Num1_FPL100_wt_log, pairwise ~ adult_disb*gender)
Num1_disb_gender <- lsmeans(Num1_FPL100_wt_log, pairwise ~ adult_disb*gender_ms)

#######################################################################
## Mixed Effects Model (MEM) of normalized FPL 100 
#######################################################################
Data$FPL100_noBaseline[Data$FPL100_noBaseline == 0] <- 0.001

MEM1_FPL100_wt <- lmer(log(FPL100_noBaseline) ~ 1 + yearqtr + gender_ms + race + 
                         race*adult_disb + yearqtr*adult_disb +
                         adult_disb + wt + adult_disb*gender_ms + adult_disb*gender_ms + 
                         (1 | hhid), 
                       data = Data, REML = TRUE
)
anova(MEM1_FPL100_wt)
summary(MEM1_FPL100_wt)
## step(MEM2_FPL100_wt)
xtable(anova(MEM1_FPL100_wt))

library(lsmeans)

MEM2_FPL100_wt_log <- lmer(log(FPL100_noBaseline) ~ 1 + yearqtr + 
                             yearqtr*adult_disb +
                             erace + gender_ms + adult_disb + wt + 
                             adult_disb*gender_ms + (1 | hhid), 
                       data = Data, REML = TRUE)
summary(MEM2_FPL100_wt_log)

lsmeans(MEM2_FPL100_wt_log, pairwise ~ gender_ms)
lsmeans(MEM2_FPL100_wt_log, pairwise ~ erace)
lsmeans(MEM2_FPL100_wt_log, pairwise ~ adult_disb*gender_ms)

MEM3_FPL100_wt_log <- lmer(log(FPL100_noBaseline) ~ 1 + yearqtr + yearqtr*adult_disb +
                             race + adult_disb*race + 
                             gender_ms + adult_disb + wt + 
                             adult_disb*gender_ms + (1 | hhid), 
                           data = Data, REML = TRUE)
summary(MEM3_FPL100_wt_log)
anova(MEM3_FPL100_wt_log)

lsmeans(MEM3_FPL100_wt_log, pairwise ~ gender_ms)
lsmeans(MEM3_FPL100_wt_log, pairwise ~ race)
lsmeans(MEM3_FPL100_wt_log, pairwise ~ race*adult_disb)

lsmeans(MEM3_FPL100_wt_log, pairwise ~ adult_disb*gender_ms)


#######################################################################
## Residuals
#######################################################################
Data$Resid_Num1 <- residuals(Num1_FPL100_wt)
Data$Resid_Num2 <- residuals(Num2_FPL100_wt) 
Data$Resid_Num3 <- residuals(Num3_FPL100_wt)

Data$Resid_MEM1 <- residuals(MEM1_FPL100_wt)
Data$Resid_MEM2 <- residuals(MEM2_FPL100_wt)
Data$Resid_MEM3 <- residuals(MEM3_FPL100_wt)

Filename <- paste0(RDataPath, 'Data_forIncPov_v3_Residuals.RData')
save(Data, file = Filename)



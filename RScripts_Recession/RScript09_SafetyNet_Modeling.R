rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This analyzes participation ONLY, in different safety net programs, for 
## disabled and non-disabled populations
########################################################################

########################################################################
## Run Path definition file                                           ##
########################################################################
RScriptPath <- '~/Project_Recession/RScripts_Recession/'
DataPath <- '~/Project_Recession/Data/data_2015Dec/'
RDataPath <- '~/Project_Recession/RData/data_2015Dec/'
PlotPath <- '~/Project_Recession/Plots/'
Filename.Header <- paste('~/RScripts/HeaderFile_lmcg.R', sep='')
source(Filename.Header)
source(paste(RScriptPath, 'fn_Library_Recession.R', sep=''))
########################################################################
Today <- Sys.Date()

## Load weights
Filename <- paste0(RDataPath, 'Weights_Long.RData')
load(Filename)

########################################################################
## load the Participation dataset
########################################################################
Filename.RData <- paste0(RDataPath, 'Safetynet_participateOnly.RData')
load(Filename.RData)
summary(Data_Participation$Program_perhh)
table(Data_Participation$Program_factor)

########################################################################
## Model1: Participation vs time + program
########################################################################
Model1 <- lm(sqrt(Program_perhh) ~ yearmon + Program_factor, 
             data = Data_Participation)
summary(Model1)
plot(residuals(Model1))

## need to do weighted least square

########################################################################
## Model2: Participation vs time + program + gender_ms
########################################################################
Model2 <- lm(sqrt(Program_perhh) ~ yearmon + Program_factor + gender_ms, 
             data = Data_Participation)
summary(Model2)

########################################################################
## Model3: Participation vs time + program + gender_ms + erace
########################################################################
Model3 <- lm(sqrt(Program_perhh) ~ yearmon + Program_factor + gender_ms + erace, 
             data = Data_Participation)
summary(Model3)

########################################################################
## Model3: Participation vs time + program + gender_ms + erace + interaction
########################################################################
Model4 <- lm(sqrt(Program_perhh) ~ yearmon + Program_factor + gender_ms + erace
             + gender_ms:erace, 
             data = Data_Participation)
summary(Model4)
plot(residuals(Model4))

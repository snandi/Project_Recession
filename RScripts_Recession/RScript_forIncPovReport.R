rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script loads the data used for Income Poverty mixed effect model
## and produces the demographics for the Income Poverty report
########################################################################

########################################################################
## Run Path definition file                                           ##
########################################################################
PathPrefix <- '~/'
# PathPrefix <- '/Users/patron/Documents/snandi/'
RScriptPath <- paste0(PathPrefix, 'Project_Recession/RScripts_Recession/')
DataPath <- paste0(PathPrefix, 'Project_Recession/Data/data_2015Dec/')
RDataPath <- paste0(PathPrefix, 'Project_Recession/RData/data_2015Dec/')
PlotPath <- paste0(PathPrefix, 'Project_Recession/Plots/')
Filename.Header <- paste0(RScriptPath, 'HeaderFile_Recession.R')
source(Filename.Header)

source(paste(RScriptPath, 'fn_Library_Recession.R', sep=''))
source(paste(RScriptPath, 'plotLSMeans.R', sep=''))

########################################################################
Today <- Sys.Date()

########################################################################
## load income poverty data
########################################################################
#Filename <- paste0(RDataPath, 'Data_forIncPov_byRace.RData')
#load(file = Filename)

##Filename <- paste0(RDataPath, 'Data_forIncPov.RData')
##load(file = Filename)
Filename <- paste0(RDataPath, 'Data_forIncPov_v5_newWts.RData')
load(file = Filename)

Data <- Data_forIncPov
Data$year <- substr(x = Data$yearqtr, start = 1, stop = 4)
Data$year <- as.factor(Data$year)

rm(Data_forIncPov)
#View(Data[,c('hhid', 'yearqtr', 'thtotinc', 'rhpov', 'adult_disb', 'FPL100_num', 'FPL100_num_Lag')])

#######################################################################
#library(lsmeans)

Data$wt <- Data$whfnwgt_qtr/1000
Data$hhid <- as.factor( Data$hhid )
Data$yearqtrNum <- as.numeric( Data$yearqtr ) 
## This is necessary for post hoc test, otherwise it is throwing exception as the 
## object not being a matrix, when difflsmeans is called

demographicsVarList <- c('hhid', 'adult_disb', 'gender', 'ms', 'gender_ms', 'race_origin', 
                         'education')

DataDemographics <- unique( Data[, demographicsVarList ] )
View( DataDemographics )


rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script reads in and does basic understanding of the recession ##
## dataset
########################################################################

########################################################################
## Run Path definition file                                           ##
########################################################################
RScriptPath <- '~/Project_Recession/RScripts_Recession/'
DataPath <- '~/Project_Recession/Data/'
RDataPath <- '~/Project_Recession/RData/'
Filename.Header <- paste('~/RScripts/HeaderFile_lmcg.R', sep='')
source(Filename.Header)
source(paste(RScriptPath, 'fn_Library_Recession.R', sep=''))
########################################################################
Today <- Sys.Date()

## Load stata dataset
#Filename.dta <- paste(DataPath, 'sippl08puw6.dta', sep='')
#Data <- read.dta(file=Filename.dta)

## Save it as RData dataset
#Filename.rdata <- paste(DataPath, 'sippl08puw6.RData', sep='')
#save(Data, file=Filename.rdata)

## Load saved RData dataset
## Filename.rdata <- paste(DataPath, 'sippl08puw6.RData', sep='')
## load(file=Filename.rdata)
## str(Data)

## length(unique(as.numeric(Data$ssuid)))
## length(unique(as.numeric(Data$ssuseq)))

########################################################################
## use the dataset sipp08_MASTER.dta
## The above data set has all the outcome variables by year (rhcalyr)
## and month (rhcalmn)
########################################################################
Filepath1 <- paste(DataPath, 'Longitudinal08/sipp08_MASTER.dta', sep = '')
sipp08_master <- read.dta(file = Filepath1)
Filepath1 <- paste(RDataPath, 'sipp08_MASTER.RData', sep = '')
save(sipp08_master, file = Filepath1)

length(unique(as.numeric(Sipp08_master$ssuid)))
########################################################################
## The above data set needs to be merged with 2008_disability.dta 
########################################################################
Filepath2 <- paste(DataPath, 'Longitudinal08/2008_disability.dta', sep = '')
disability_2008 <- read.dta(file = Filepath2)
Filepath2 <- paste(RDataPath, '2008_disability.RData', sep = '')
save(disability_2008, file = Filepath2)

length(unique(as.numeric(disability_2008$ssuid)))

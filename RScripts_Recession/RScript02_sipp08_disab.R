rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script reads in sipp08_master & 2008_disability and merges them
## and conducts basic summary statistics
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

########################################################################
## load sipp08_MASTER.RData
########################################################################
Filepath1 <- paste(RDataPath, 'sipp08_MASTER.RData', sep = '')
load(Filepath1)

length(unique(as.numeric(sipp08_master$ssuid)))

sipp08_01 <- subset(sipp08_master, ssuid == "019128000276")
########################################################################
## 2008_disability.dta 
########################################################################
Filepath2 <- paste(RDataPath, '2008_disability.RData', sep = '')
load(Filepath2)

length(unique(as.numeric(disability_2008$ssuid)))


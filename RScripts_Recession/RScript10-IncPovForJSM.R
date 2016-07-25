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
source('../../RScripts/fn_Library_SN.R')

SlidePath <- paste0(PathPrefix, 'Project_Recession/Slides_Recession/')
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

ssuids <- unique(Data_forIncPov$ssuid)
#Data_Sub <- subset(Data_forIncPov, yearmon == 'Jun 2008')
Data_Sub <- subset(Data_forIncPov, ssuid %in% ssuids[1:40])
head(Data_forIncPov)
# View(subset(Data_forIncPov, ssuid == '019128000276'))

########################################################################
## save one user's data
########################################################################
FilenameUser <- paste0(SlidePath, 'SnapshotUser.csv')
SnapshotUserData <- subset(Data_forIncPov, ssuid == '019128000276')
# write.csv(SnapshotUserData, file = FilenameUser, quote = F, row.names = F)
str(SnapshotUserData)

########################################################################
## Plot snapshot of income poverty
########################################################################
ssuids <- c('019128000276', '019128038276', '019133469324', '019133717344')

fn_PlotSnapshot <- function(IncData = Data_forIncPov, ssID = ssuids){
  SnapshotUserData <- subset(IncData, ssuid %in% ssID)
  SnapshotUserData1 <- subset(IncData, ssuid %in% ssID[1])
  PlotSnapshot <- qplot() + 
    geom_line(aes(x = as.numeric(yearqtr), y = thtotinc, col = ssuid), 
                                      data = SnapshotUserData) + 
    theme(legend.position = 'none')
  PlotSnapshot <- PlotSnapshot + 
    geom_line(aes(x = as.numeric(yearqtr), y = rhpov), size = 1.1,
              data = SnapshotUserData1, col = 'black')
  PlotSnapshot <- PlotSnapshot + 
    geom_line(aes(x = as.numeric(yearqtr), y = rhpov2), size = 1.1,
              data = SnapshotUserData1, col = 'black', lty = 2)
  PlotSnapshot <- PlotSnapshot + xlab(label = '') + ylab(label = '') + 
    ggtitle(label = 'Monthly Household Income')
  PlotFilename <- paste0(SlidePath, 'PlotSnapshot', ssID[1], '.jpeg')
  ggsave(
    filename = PlotFilename, 
    plot = PlotSnapshot, 
    device = 'jpeg', 
    width = 30,
    height = 20,
    units = 'cm'
  )
  return(PlotSnapshot)
}

PlotSnapshot <- fn_PlotSnapshot(IncData = Data_forIncPov, ssID = ssuids)

########################################################################
## Demographics
########################################################################
fn_FirstRow <- function(Data){
  Baseline <- Data[1,]
  return(Baseline)
}

library(dplyr)

UniqeData <- ddply(
  .data = Data_forIncPov[,c('ssuid', 'shhadid', 'gender_ms', 
                            'race', 'erace', 'adult_disb')],
  .fun = fn_FirstRow,
  .variables = c('ssuid', 'shhadid')
)

GenderMS <- table(UniqeData$gender_ms)
chisq.test(GenderMS)

eRace <- table(UniqeData$erace)

table(UniqeData$adult_disb)

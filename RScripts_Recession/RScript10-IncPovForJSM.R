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

hhids <- unique(Data_forIncPov$hhid)
# Data_Sub <- subset(Data_forIncPov, yearmon == 'Jun 2008')
# Data_Sub <- subset(Data_forIncPov, ssuid %in% ssuids[1:40])
head(Data_forIncPov)
# View(subset(Data_forIncPov, ssuid == '019128000276'))

########################################################################
## save one user's data
########################################################################
# FilenameUser <- paste0(SlidePath, 'SnapshotUser.csv')
# SnapshotUserData <- subset(Data_forIncPov, ssuid == '019128000276')
# write.csv(SnapshotUserData, file = FilenameUser, quote = F, row.names = F)
# str(SnapshotUserData)

########################################################################
## Plot snapshot of income poverty
#######################################################################
hhids <- c('019128000276_11', '019128038276_11', '019133469324_11', 
           '019133717344_11')

fn_PlotSnapshot <- function(
  IncData = Data_forIncPov, 
  ssID = hhids
){
  SnapshotUserData <- subset(IncData, hhid %in% ssID)
  SnapshotUserData1 <- subset(IncData, hhid %in% ssID[1])
  PlotSnapshot <- qplot() + 
    geom_line(aes(x = as.numeric(yearqtr), y = thtotinc, col = hhid), 
              data = SnapshotUserData, size = 1.2) 
  PlotSnapshot <- PlotSnapshot + 
    geom_line(aes(x = as.numeric(yearqtr), y = rhpov), size = 1.1,
              data = SnapshotUserData1, col = 'black')
  PlotSnapshot <- PlotSnapshot + 
    geom_line(aes(x = as.numeric(yearqtr), y = rhpov2), size = 1.1,
              data = SnapshotUserData1, col = 'black', lty = 2)
  PlotSnapshot <- PlotSnapshot + xlab(label = '') + ylab(label = '') + 
    ggtitle(label = 'Monthly Household Income')
  PlotSnapshot <- PlotSnapshot + 
    theme(
      legend.position = 'none', 
      axis.text = element_text(size = 20, face = 'bold'), 
      plot.title = element_text(size = 32, face = 'bold')
    )
  PlotFilename <- paste0(SlidePath, 'PlotSnapshot', ssID[1], '.jpeg')
  ggsave(
    filename = PlotFilename, 
    plot = PlotSnapshot, 
    device = 'jpg', 
    width = 30,
    height = 20,
    units = 'cm'
  )
  return(PlotSnapshot)
}

PlotSnapshot <- fn_PlotSnapshot(IncData = Data_forIncPov, ssID = hhids)
PlotSnapshot
########################################################################
## Demographics
########################################################################
fn_FirstRow <- function(Data){
  Baseline <- Data[1,]
  return(Baseline)
}

library(dplyr)

UniqeData <- ddply(
  .data = Data_forIncPov[,c('hhid', 'gender_ms', 
                            'race', 'erace', 'adult_disb')],
  .fun = fn_FirstRow,
  .variables = c('hhid')
)

GenderMS <- table(UniqeData$gender_ms)
chisq.test(GenderMS)

eRace <- table(UniqeData$erace)

table(UniqeData$adult_disb)

########################################################################
## Disability Data
########################################################################
# FilenameDisab <- paste(RDataPath, '2008_W6_topical_disability_variables.RData', sep='')
# load(FilenameDisab)
# 
# UniqueDisb <- unique(Data_disab[,c('ssuid', 'shhadid', 'adult_disb')])
# table(UniqueDisb$adult_disb)
# sum(table(UniqueDisb$adult_disb))

summary(Data_forIncPov$FPL100_num)
qplot() + geom_histogram(aes(x = log(FPL100_num)), data = Data_forIncPov)


fn_PlotFPL100 <- function(
  IncData = Data_forIncPov, 
  ssID = hhids,
  ColToPlot = 'FPL100_num',
  Title = 'Ratio of Monthly Income to FPL 100',
  FilenameSuffix = 'FPL100'
){
  SnapshotUserData <- subset(IncData, hhid %in% ssID)
  SnapshotUserData1 <- subset(IncData, hhid %in% ssID[1])
  PlotSnapshot <- qplot() + 
    geom_line(aes(x = as.numeric(yearqtr), y = get(ColToPlot), col = hhid), 
              data = SnapshotUserData, size = 1.2) 
  PlotSnapshot <- PlotSnapshot + xlab(label = '') + ylab(label = '') + 
    ggtitle(label = Title)
  PlotSnapshot <- PlotSnapshot + 
    theme(
      legend.position = 'none', 
      axis.text = element_text(size = 20, face = 'bold'), 
      plot.title = element_text(size = 32, face = 'bold')
    )
  PlotFilename <- paste0(SlidePath, 'PlotFPL_', FilenameSuffix, '.jpeg')
  ggsave(
    filename = PlotFilename, 
    plot = PlotSnapshot, 
    device = 'jpg', 
    width = 50,
    height = 20,
    units = 'cm'
  )
  return(PlotSnapshot)
}

PlotFPL100 <- fn_PlotFPL100(
  IncData = Data, 
  ssID = hhids,
  ColToPlot = 'FPL100_num',
  Title = 'Ratio of Monthly Income to FPL 100',
  FilenameSuffix = 'FPL100'
)

PlotFPL100_noBaseline <- fn_PlotFPL100(
  IncData = Data, 
  ssID = hhids,
  ColToPlot = 'FPL100_noBaseline',
  Title = 'Normalized Ratio of Monthly Income to FPL 100',
  FilenameSuffix = 'FPL100_noBaseline'
)
library(stargazer)

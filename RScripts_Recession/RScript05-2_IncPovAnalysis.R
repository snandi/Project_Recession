rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script analyzes the income poverty over time. This is similar to
## RScript05, with the following differences
## 1. This uses Data_forIncPov_v5_newWts.RData
## 2. This does not fit log(FPL100)
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
## Mixed Effects Model (MEM) of Income Poverty Ratio
########################################################################
#library(lsmeans)

Data$wt <- Data$whfnwgt_qtr/1000
Data$hhid <- as.factor( Data$hhid )
Data$yearqtrNum <- as.numeric( Data$yearqtr ) 
## This is necessary for post hoc test, otherwise it is throwing exception as the 
## object not being a matrix, when difflsmeans is called

time1 <- Sys.time()
modelFPL100 <- lmerTest::lmer( FPL100_num ~ 1 + yearqtrNum + gender + ms + race_origin + adult_disb + education + 
                                 race_origin:gender + gender:ms + race_origin:ms + race_origin:adult_disb + gender:adult_disb + 
                                 ms:adult_disb + gender:ms:adult_disb + adult_disb:yearqtrNum + education:adult_disb + 
                                 (1 | hhid), data = Data, weights = wt 
)

lmerTest::summary( modelFPL100 )
lmerTest::anova( modelFPL100 )
time2 <- Sys.time()
print( time2 - time1 )

modelFPL100NoBaseline <- lmerTest::lmer( FPL100_noBaseline ~ 1 + yearqtrNum + gender + ms + race_origin + adult_disb + education + 
                                           race_origin*gender + gender*ms + race_origin*ms + race_origin*adult_disb + gender*adult_disb + 
                                           adult_disb*ms + gender*ms*adult_disb + adult_disb*yearqtrNum + education*adult_disb + 
                                           (1 | hhid), data = Data, weights = wt
)

lmerTest::summary( modelFPL100NoBaseline )
lmerTest::anova( modelFPL100NoBaseline )

time3 <- Sys.time()
print( time3 - time2 )

#######################################################################
## Post hoc tests
#######################################################################
postHocFactors <- c( 'race_origin', 'education', 'gender:ms', 'ms:race_origin', 
                     'gender:race_origin', 'race_origin:adult_disb', 'gender:adult_disb', 
                     'ms:adult_disb', 'gender:ms:adult_disb', 'adult_disb:education'
)

plotFilename <- paste0( PlotPath, 'PlotsPostHoc_RScript05-2.pdf' )
pdf( file = plotFilename, onefile = TRUE )

for( Factor in postHocFactors ){
  print( Factor )
  postHoc <- lmerTest::difflsmeans(
    model = modelFPL100, 
    test.effs = Factor
  )
  
  plotPostHoc <- try( plotLSMeans(
    response = postHoc$response,
    table = postHoc$diffs.lsmeans.table, 
    which.plot = 'DIFF of LSMEANS', 
    mult = TRUE
  ) )
  
  postHocNoBaseline <- lmerTest::difflsmeans(
    model = modelFPL100NoBaseline, 
    test.effs = Factor
  )
  
  plotPostHocNoBaseline <- try( plotLSMeans(
    response = postHocNoBaseline$response,
    table = postHocNoBaseline$diffs.lsmeans.table, 
    which.plot = 'DIFF of LSMEANS', 
    mult = TRUE
  ) )
  
  try( print( postHoc ) )
  try( print( postHocNoBaseline ) )
  
  try( plot( plotPostHoc ) )
  try( plot( plotPostHocNoBaseline ) )
  
  try( rm( postHoc, postHocNoBaseline, plotPostHoc, plotPostHocNoBaseline ) )
}

dev.off()

time4 <- Sys.time()
print( time4 - time3 )

# #######################################################################
# ## Post hoc: Race
# #######################################################################
postHoc <- lmerTest::difflsmeans(
  model = modelFPL100,
  test.effs = 'gender*ms*adult_disb'
)
# 
# plotRaceOrigin <- plotLSMeans(
#   response = postHocRaceOrigin$response,
#   table = postHocRaceOrigin$diffs.lsmeans.table, 
#   which.plot = 'DIFF of LSMEANS', 
#   mult = TRUE
# )
# plotRaceOrigin
# plotFilename <- paste0(PlotPath, 'PlotPostHoc_', 'race_origin', '.jpeg')
# ggsave(
#   filename = plotFilename, 
#   plot = plotRaceOrigin, 
#   device = 'jpg'
# )
# 

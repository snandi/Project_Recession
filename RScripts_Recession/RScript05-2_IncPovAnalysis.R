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

modelFPL100 <- lme4::lmer( FPL100_num ~ 1 + yearqtrNum + gender + ms + race_origin + adult_disb + 
                                     gender*ms + race_origin*ms + race_origin*adult_disb + gender*adult_disb + 
                                     adult_disb*ms + adult_disb*gender*ms + adult_disb*yearqtrNum + 
                                     (1 | hhid), data = Data, weights = wt 
)

summary( modelFPL100 )
# lmerTest::anova( modelFPL100 ) ## 
# anova( modelFPL100 ) ## 

# Resid1 <- as.data.frame( cbind( yearqtr = Data$yearqtr, resid = residuals( modelFPL100 ) ) )
# Resid1_Plot <- qplot() + geom_point( aes (x = yearqtr, y = resid ), data = Resid1 )
# xtable( anova( modelFPL100 ) )

#######################################################################
## Post hoc: Race
#######################################################################
postHocRaceOrigin <- lmerTest::difflsmeans(
  model = modelFPL100, 
  test.effs = 'race_origin'
)

plotRaceOrigin <- plotLSMeans(
  response = postHocRaceOrigin$response,
  table = postHocRaceOrigin$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
plotRaceOrigin
plotFilename <- paste0(PlotPath, 'PlotPostHoc_', 'race_origin', '.jpeg')
ggsave(
  filename = plotFilename, 
  plot = plotRaceOrigin, 
  device = 'jpg'
)

#######################################################################
## Post hoc: gender_ms
#######################################################################
postHocGenderMS <- lmerTest::difflsmeans(
  model =   modelFPL100, 
  test.effs = 'gender_ms'
)
plotGenderMS <- plotLSMeans(
  response = postHocGenderMS$response,
  table = postHocGenderMS$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
plotFilename <- paste0(PlotPath, 'PlotPostHoc_', 'gender_ms', '.jpeg')
ggsave(
  filename = plotFilename, 
  plot = plotGenderMS, 
  device = 'jpg'
)

#######################################################################
## Post hoc: Race & Disability
########################################################################
postHocRaceDisb <- lmerTest::difflsmeans(
  model =   modelFPL100, 
  test.effs = c('race_origin:adult_disb')
)
str(postHocRaceDisb)
# Plot_gender_ms <- plot(PostHoc_gender_ms)
plotRaceDisb <- plotLSMeans(
  response = postHocRaceDisb$response,
  table = postHocRaceDisb$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
plotRaceDisb
plotFilename <- paste0(PlotPath, 'PlotPostHoc_', 'race_disb', '.jpeg')
ggsave(
  filename = plotFilename, 
  plot = plotRaceDisb, 
  device = 'jpg'
)

#######################################################################
## Post hoc: Gender, MS & Disability
########################################################################
postHocGenderMSDisb <- lmerTest::difflsmeans(
  model =   modelFPL100, 
  test.effs = c('gender_ms:adult_disb')
)
# str(PostHoc_gender_ms_disb)
# str(PostHoc_gender_ms_disb$diffs.lsmeans.table)
# Plot_gender_ms_disb <- plot(PostHoc_gender_ms_disb, which.plot = 'DIFF of LSMEANS')
plotGenderMSDisb <- plotLSMeans(
  response = postHocGenderMSDisb$response,
  table = postHocGenderMSDisb$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
plotFilename <- paste0(PlotPath, 'PlotPostHoc_', 'gender_ms_disb', '.jpeg')
ggsave(
  filename = plotFilename, 
  plot = plotGenderMSDisb, 
  device = 'jpg'
)

#######################################################################
## Mixed Effects Model (MEM) of normalized FPL 100 
#######################################################################
# Data$FPL100_noBaseline[Data$FPL100_noBaseline == 0] <- 0.001

modelFPL100NoBaseline <- lmerTest::lmer( FPL100_noBaseline ~ 1 + yearqtrNum + gender + ms + race_origin + adult_disb + 
                                       gender*ms + race_origin*ms + race_origin*adult_disb + gender*adult_disb + 
                                       adult_disb*ms + adult_disb*gender*ms + adult_disb*yearqtrNum + 
                                       (1 | hhid), data = Data, weights = wt
)

summary( modelFPL100NoBaseline )
anova( modelFPL100NoBaseline, type = 1 )

#xtable(modelFPL100NoBaseline)

#######################################################################
## Post hoc: Race
#######################################################################
postHocRaceOriginNoBaseline <- lmerTest::difflsmeans(
  model = modelFPL100NoBaseline, 
  test.effs = 'race_origin'
)
postHocRaceOriginNoBaseline
plotRaceOriginNoBaseline <- plotLSMeans(
  response = postHocRaceOriginNoBaseline$response,
  table = postHocRaceOriginNoBaseline$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
plotFilename <- paste0(PlotPath, 'PlotPostHocNorm_', 'race', '.jpeg')
ggsave(
  filename = plotFilename, 
  plot = plotRaceOriginNoBaseline, 
  device = 'jpg'
)

#######################################################################
## Post hoc: gender_ms
#######################################################################
postHocGenderMSNoBaseline <- lmerTest::difflsmeans(
  model = modelFPL100NoBaseline, 
  test.effs = 'gender_ms'
)
postHocGenderMSNoBaseline
plotGenderMSNoBaseline <- plotLSMeans(
  response = postHocGenderMSNoBaseline$response,
  table = postHocGenderMSNoBaseline$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
plotFilename <- paste0(PlotPath, 'PlotPostHocNorm_', 'gender_ms', '.jpeg')
ggsave(
  filename = plotFilename, 
  plot = plotGenderMSNoBaseline, 
  device = 'jpg'
)

#######################################################################
## Post hoc: Race & Disability
#######################################################################
postHocRaceDisbNoBaseline <- lmerTest::difflsmeans(
  model = modelFPL100NoBaseline, 
  test.effs = c('adult_disb:race_origin')
)
str(postHocRaceDisbNoBaseline)
# Plot_gender_ms <- plot(PostHoc_gender_ms)
plotRaceDisbNoBaseline <- plotLSMeans(
  response = postHocRaceDisbNoBaseline$response,
  table = postHocRaceDisbNoBaseline$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
plotRaceDisbNoBaseline
plotFilename <- paste0(PlotPath, 'PlotPostHocNorm_', 'race_disb', '.jpeg')
ggsave(
  filename = plotFilename, 
  plot = plotRaceDisbNoBaseline, 
  device = 'jpg'
)

#######################################################################
## Post hoc: Gender, MS & Disability
########################################################################
postHocGenderMSDisbNoBaseline <- lmerTest::difflsmeans(
  model = modelFPL100NoBaseline, 
  test.effs = c('adult_disb:gender_ms')
)
# str(postHocGenderMSDisbNoBaseline)
# str(postHocGenderMSDisbNoBaseline$diffs.lsmeans.table)
# plotGenderMSDisbNoBaseline <- plot(postHocGenderMSDisbNoBaseline, which.plot = 'DIFF of LSMEANS')
plotGenderMSDisbNoBaseline <- plotLSMeans(
  response = postHocGenderMSDisbNoBaseline$response,
  table = postHocGenderMSDisbNoBaseline$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
plotFilename <- paste0(PlotPath, 'PlotPostHocNorm_', 'gender_ms_disb', '.jpeg')
ggsave(
  filename = plotFilename, 
  plot = plotGenderMSDisbNoBaseline, 
  device = 'jpg'
)


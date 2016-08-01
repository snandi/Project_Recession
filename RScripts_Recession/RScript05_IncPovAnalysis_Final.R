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
Filename.Header <- paste0(RScriptPath, 'HeaderFile_Recession.R')
source(Filename.Header)

source(paste(RScriptPath, 'fn_Library_Recession.R', sep=''))
try(source('../../RScripts/fn_Library_SN.R'))
try(source('~/RScripts/fn_Library_SN.R'))
source(paste(RScriptPath, 'plotLSMeans.R', sep=''))
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

Data <- Data_forIncPov
Data$year <- substr(x = Data$yearqtr, start = 1, stop = 4)
Data$year <- as.factor(Data$year)

rm(Data_forIncPov)
#View(Data[,c('hhid', 'yearqtr', 'thtotinc', 'rhpov', 'adult_disb', 'FPL100_num', 'FPL100_num_Lag')])

#######################################################################
## Mixed Effects Model (MEM) of Income Poverty Ratio
########################################################################
#library(lsmeans)

Data$wt <- Data$wt/1000

Num1_FPL100_wt_log <- lme4::lmer(log(FPL100_num) ~ 1 + yearqtr + gender_ms + race + 
                             race*adult_disb + 
                             adult_disb + wt + adult_disb*gender_ms + adult_disb*gender_ms + 
                             adult_disb*yearqtr + (1 | hhid), 
                           data = Data)

summary(Num1_FPL100_wt_log)
# lmerTest::anova(Num1_FPL100_wt_log) ## 
# anova(Num1_FPL100_wt_log) ## 

# Resid1_log <- as.data.frame(cbind(yearqtr = Data$yearqtr, resid = residuals(Num1_FPL100_wt_log)))
# Resid1_logPlot <- qplot() + geom_point(aes(x = yearqtr, y = resid), data = Resid1_log)
# xtable(anova(Num1_FPL100_wt_log))

#######################################################################
## Post hoc: Race
#######################################################################
PostHoc_race <- lmerTest::difflsmeans(
  model =   Num1_FPL100_wt_log, 
  test.effs = 'race'
)
Plot_race <- plotLSMeans(
  response = PostHoc_race$response,
  table = PostHoc_race$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
Plot_race
PlotFilename <- paste0(SlidePath, 'PlotPostHoc_', 'race', '.jpeg')
ggsave(
  filename = PlotFilename, 
  plot = Plot_race, 
  device = 'jpg', 
  width = 30,
  height = 20,
  units = 'cm'
)

#######################################################################
## Post hoc: gender_ms
#######################################################################
PostHoc_gender_ms <- lmerTest::difflsmeans(
  model =   Num1_FPL100_wt_log, 
  test.effs = 'gender_ms'
)
Plot_gender_ms <- plotLSMeans(
  response = PostHoc_gender_ms$response,
  table = PostHoc_gender_ms$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
PlotFilename <- paste0(SlidePath, 'PlotPostHoc_', 'gender_ms', '.jpeg')
ggsave(
  filename = PlotFilename, 
  plot = Plot_gender_ms, 
  device = 'jpg', 
  width = 40,
  height = 30,
  units = 'cm'
)

#######################################################################
## Post hoc: Race & Disability
########################################################################
PostHoc_race_disb <- lmerTest::difflsmeans(
  model =   Num1_FPL100_wt_log, 
  test.effs = c('race:adult_disb')
)
str(PostHoc_race_disb)
# Plot_gender_ms <- plot(PostHoc_gender_ms)
Plot_race_disb <- plotLSMeans(
  response = PostHoc_race_disb$response,
  table = PostHoc_race_disb$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
Plot_race_disb
PlotFilename <- paste0(SlidePath, 'PlotPostHoc_', 'race_disb', '.jpeg')
ggsave(
  filename = PlotFilename, 
  plot = Plot_race_disb, 
  device = 'jpg', 
  width = 40,
  height = 30,
  units = 'cm'
)

#######################################################################
## Post hoc: Gender, MS & Disability
########################################################################
PostHoc_gender_ms_disb <- lmerTest::difflsmeans(
  model =   Num1_FPL100_wt_log, 
  test.effs = c('gender_ms:adult_disb')
)
# str(PostHoc_gender_ms_disb)
# str(PostHoc_gender_ms_disb$diffs.lsmeans.table)
# Plot_gender_ms_disb <- plot(PostHoc_gender_ms_disb, which.plot = 'DIFF of LSMEANS')
Plot_gender_ms_disb <- plotLSMeans(
  response = PostHoc_gender_ms_disb$response,
  table = PostHoc_gender_ms_disb$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
PlotFilename <- paste0(SlidePath, 'PlotPostHoc_', 'gender_ms_disb', '.jpeg')
ggsave(
  filename = PlotFilename, 
  plot = Plot_gender_ms_disb, 
  device = 'jpg', 
  width = 40,
  height = 30,
  units = 'cm'
)

#######################################################################
## Mixed Effects Model (MEM) of normalized FPL 100 
#######################################################################
Data$FPL100_noBaseline[Data$FPL100_noBaseline == 0] <- 0.001

MEM2_FPL100_wt_log <- lme4::lmer(log(FPL100_noBaseline) ~ 1 + yearqtr + 
                             yearqtr*adult_disb + race*adult_disb +
                             race + gender_ms + adult_disb + wt + 
                             adult_disb*gender_ms + (1 | hhid), 
                       data = Data, REML = TRUE)
summary(MEM2_FPL100_wt_log)
#xtable(MEM2_FPL100_wt_log)

#######################################################################
## Post hoc: Race
#######################################################################
PostHoc_race <- lmerTest::difflsmeans(
  model = MEM2_FPL100_wt_log, 
  test.effs = 'race'
)
Plot_race_Norm <- plotLSMeans(
  response = PostHoc_race$response,
  table = PostHoc_race$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
PlotFilename <- paste0(SlidePath, 'PlotPostHocNorm_', 'race', '.jpeg')
ggsave(
  filename = PlotFilename, 
  plot = Plot_race_Norm, 
  device = 'jpg', 
  width = 40,
  height = 30,
  units = 'cm'
)

#######################################################################
## Post hoc: gender_ms
#######################################################################
PostHoc_gender_ms <- lmerTest::difflsmeans(
  model = MEM2_FPL100_wt_log, 
  test.effs = 'gender_ms'
)
Plot_gender_ms <- plotLSMeans(
  response = PostHoc_gender_ms$response,
  table = PostHoc_gender_ms$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
PlotFilename <- paste0(SlidePath, 'PlotPostHocNorm_', 'gender_ms', '.jpeg')
ggsave(
  filename = PlotFilename, 
  plot = Plot_gender_ms, 
  device = 'jpg', 
  width = 40,
  height = 30,
  units = 'cm'
)

#######################################################################
## Post hoc: Race & Disability
#######################################################################
PostHoc_race_disb <- lmerTest::difflsmeans(
  model = MEM2_FPL100_wt_log, 
  test.effs = c('adult_disb:race')
)
str(PostHoc_race_disb)
# Plot_gender_ms <- plot(PostHoc_gender_ms)
Plot_race_disb <- plotLSMeans(
  response = PostHoc_race_disb$response,
  table = PostHoc_race_disb$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
Plot_race_disb
PlotFilename <- paste0(SlidePath, 'PlotPostHocNorm_', 'race_disb', '.jpeg')
ggsave(
  filename = PlotFilename, 
  plot = Plot_race_disb, 
  device = 'jpg', 
  width = 40,
  height = 30,
  units = 'cm'
)

#######################################################################
## Post hoc: Gender, MS & Disability
########################################################################
PostHoc_gender_ms_disb <- lmerTest::difflsmeans(
  model = MEM2_FPL100_wt_log, 
  test.effs = c('adult_disb:gender_ms')
)
# str(PostHoc_gender_ms_disb)
# str(PostHoc_gender_ms_disb$diffs.lsmeans.table)
# Plot_gender_ms_disb <- plot(PostHoc_gender_ms_disb, which.plot = 'DIFF of LSMEANS')
Plot_gender_ms_disb <- plotLSMeans(
  response = PostHoc_gender_ms_disb$response,
  table = PostHoc_gender_ms_disb$diffs.lsmeans.table, 
  which.plot = 'DIFF of LSMEANS', 
  mult = TRUE
)
PlotFilename <- paste0(SlidePath, 'PlotPostHocNorm_', 'gender_ms_disb', '.jpeg')
ggsave(
  filename = PlotFilename, 
  plot = Plot_gender_ms_disb, 
  device = 'jpg', 
  width = 40,
  height = 30,
  units = 'cm'
)

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



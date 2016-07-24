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

#Data_Participation$Program_perhh[Data_Participation$Program_perhh == 0] 

Data_Participation <- Data_Participation[order(Data_Participation$yearmon),]

#Data_Participation$yearmon_Num <- as.numeric(Data_Participation$yearmon) - 2008

# ########################################################################
# ## Model1: Participation vs time + program
# ########################################################################
# Model1 <- lm(sqrt(Program_perhh) ~ yearmon + Program_factor, 
#              data = Data_Participation)
# summary(Model1)
# plot(residuals(Model1))
# 
# ## need to do weighted least square
# 
# ########################################################################
# ## Model2: Participation vs time + program + gender_ms
# ########################################################################
# Model2 <- lm(sqrt(Program_perhh) ~ yearmon + Program_factor + gender_ms, 
#              data = Data_Participation)
# summary(Model2)
# 
# ########################################################################
# ## Model3: Participation vs time + program + gender_ms + erace
# ########################################################################
# Model3 <- lm(sqrt(Program_perhh) ~ yearmon + Program_factor + gender_ms + erace, 
#              data = Data_Participation)
# summary(Model3)
# 
# ########################################################################
# ## Model4: Participation vs time + program + gender_ms + erace + interaction
# ########################################################################
# Model4 <- lm(sqrt(Program_perhh) ~ yearmon + Program_factor + gender_ms + erace
#              + gender_ms:erace, 
#              data = Data_Participation)
# summary(Model4)
# plot(residuals(Model4))

########################################################################
## Model5: Participation vs time + program + gender_ms + erace + interaction + disab
########################################################################
Model5 <- lm(sqrt(Program_perhh) ~ yearmon + Program_factor + erace + gender_ms + gender_ms*erace + 
               adult_disb + yearmon*adult_disb, 
             data = Data_Participation)
summary(Model5)
anova(Model5)
DiagPlots5 <- diagPlot(model = Model5)
names(DiagPlots5)
# grid.arrange(DiagPlots5$rvfPlot, DiagPlots5$qqPlot, DiagPlots5$sclLocPlot, DiagPlots5$cdPlot, 
#             DiagPlots5$rvlevPlot, DiagPlots5$cvlPlot, nrow = 2)

########################################################################
## Model5 - GLM: Participation vs time + program + gender_ms + erace + interaction + disab
########################################################################
Model5.glm <- glm(Program_perhh ~ yearmon + Program_factor + erace + gender_ms + 
                    adult_disb, 
                  family = binomial( link = logit ), 
                  data = Data_Participation)
summary(Model5.glm)
conf.intervals(Model5.glm)

# Model5.glm <- glm(Program_perhh ~ yearmon_Num + Program_factor + erace + gender_ms + 
#                     adult_disb, 
#                   family = binomial( link = logit ), 
#                   data = Data_Participation)
# summary(Model5.glm)
# conf.intervals(Model5.glm)
########################################################################
## Model6 - GLM: Participation vs time + program + gender_ms + erace + interaction + disab
########################################################################
Model6.glm <- glm(Program_perhh ~ yearmon + Program_factor + erace + gender_ms + 
                    adult_disb + yearmon*Program_factor, 
                  family = binomial( link = logit ), 
                  data = Data_Participation)
summary(Model6.glm)
conf.intervals(Model6.glm)

########################################################################
## Model for unemployment
########################################################################
Model_Unemp <- glm(Program_perhh ~ yearmon + I(yearmon^2) + erace + gender_ms + 
                    adult_disb, 
                  family = binomial( link = logit ), 
                  data = subset(Data_Participation, Program_factor == 'Unemp'))
summary(Model_Unemp)
Output_Unemp <- cbind(summary(Model_Unemp)$coeff, round(exp(coefficients(Model_Unemp)), 4), 
                      exp(conf.intervals(Model_Unemp)))
colnames(Output_Unemp) <- c('Beta', 'S.E.', 'z', 'p-value', 'Odds Ratio', '2.5%', '97.5%')
Output_Unemp <- round(Output_Unemp, 4)
xtable(Output_Unemp)

########################################################################
## Model for SSI
########################################################################
Model_SSI <- glm(Program_perhh ~ yearmon + erace + gender_ms + adult_disb, 
                   family = binomial( link = logit ), 
                   data = subset(Data_Participation, Program_factor == 'SSI'))
summary(Model_SSI)
Output_SSI <- cbind(summary(Model_SSI)$coeff, round(exp(coefficients(Model_SSI)), 4), 
                      exp(conf.intervals(Model_SSI)))
colnames(Output_SSI) <- c('Beta', 'S.E.', 'z', 'p-value', 'Odds Ratio', '2.5%', '97.5%')
Output_SSI[1,7] <- Inf
Output_SSI <- round(Output_SSI, 4)
xtable(Output_SSI, digits = c(0, rep(4, times = 7)))

########################################################################
## Model for foodstamp
########################################################################
Model_FdStp <- glm(Program_perhh ~ yearmon + erace + gender_ms + adult_disb, 
                 family = binomial( link = logit ), 
                 data = subset(Data_Participation, Program_factor == 'FdStp'))
summary(Model_FdStp)
Output_FdStp <- cbind(summary(Model_FdStp)$coeff,
                      round(exp(coefficients(Model_FdStp)), 4),
                      exp(conf.intervals(Model_FdStp)))
colnames(Output_FdStp) <- c('Beta', 'S.E.', 'z', 'p-value', 'Odds Ratio', '2.5%', '97.5%')
Output_FdStp <- round(Output_FdStp, 4)
xtable(Output_FdStp, digits = c(0, rep(4, times = 7)))


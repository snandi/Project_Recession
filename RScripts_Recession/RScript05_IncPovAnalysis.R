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
RScriptPath <- '~/Project_Recession/RScripts_Recession/'
DataPath <- '~/Project_Recession/Data/data_2015Dec/'
RDataPath <- '~/Project_Recession/RData/data_2015Dec/'
PlotPath <- '~/Project_Recession/Plots/'
Filename.Header <- paste('~/RScripts/HeaderFile_lmcg.R', sep = '')
source(Filename.Header)
source(paste(RScriptPath, 'fn_Library_Recession.R', sep = ''))
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

## SplitByssuid <- split(x = Data_Sub, f = as.factor(Data_Sub$ssuid))
## Data_Sub$FPL100_num_nobaseline <- do.call(what = c, args = lapply(X = SplitByssuid,
##                                                       FUN = normalize_baseline, Colname = 'FPL100_num'))
########################################################################
## To plot longitudinal data for Yajuan
########################################################################
## Filename.plot <- paste0(PlotPath, 'Plot_40HH.pdf')
## pdf(file = Filename.plot, onefile = T)
## Plot1 <- qplot() + geom_line(aes(x = as.factor(yearmon), y = FPL100_num_Norm, group = ssuid, color = ssuid), data = Data_Sub) + 
##   facet_wrap(~disb_wrk_ageR2) + 
##   xlab(label = '') + ylab(label = '') +
##   theme(axis.text.x = element_text(angle=90, vjust=1), 
##         legend.position="none")

## Plot2 <- qplot() + geom_line(aes(x = as.factor(yearmon), y = FPL100_num, group = ssuid, color = ssuid), data = Data_Sub) + 
##   facet_wrap(~disb_wrk_ageR2) + 
##   geom_hline(yintercept = 1)+ xlab(label = '') + ylab(label = '') +
##   theme(axis.text.x = element_text(angle=90, vjust=1), 
##         legend.position="none")
## Plot1
## Plot2
## dev.off()
## try(dev.off())
########################################################################

Data <- Data_forIncPov
Data$year <- substr(x = Data$yearqtr, start = 1, stop = 4)
Data$year <- as.factor(Data$year)

View(Data[,c('ssuid', 'yearqtr', 'thtotinc', 'rhpov', 'adult_disb', 'FPL100_num', 'FPL100_num_Lag')])

#######################################################################
## Linear Model of normalized FPL 100 
########################################################################
Num1_FPL100_wt <- lmer(FPL100_num ~ 1 + gender_ms + erace + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), 
                       data = Data, REML = TRUE)
summary(Num3_FPL100_wt)
anova(Num3_FPL100_wt)
step(Num3_FPL100_wt)

Num2_FPL100_wt <- lmer(FPL100_num ~ 1 + FPL100_num_Lag + gender_ms + erace + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), 
                       data = Data, REML = TRUE)
summary(Num4_FPL100_wt)
anova(Num4_FPL100_wt)



## Model1 <- lm(FPL100 ~ yearqtr + gender_ms + race + disb_wrk_ageR2, data = Data_Sub)
## Model0_FPL100 <- lm(FPL100_noBaseline ~ yearqtr, data = Data)
## summary(Model0_FPL100)

## Model1_FPL100 <- lm(FPL100_noBaseline ~ race, data = Data_forIncPov)
## summary(Model1_FPL100)

## Model2_FPL100 <- lm(FPL100_noBaseline ~ race + gender_ms, data = Data_forIncPov)
## summary(Model2_FPL100)
## anova(Model1_FPL100, Model2_FPL100)

Model3_FPL100 <- lm(FPL100_num ~ erace + adult_disb, data = Data)
summary(Model3_FPL100)
## anova(Model2_FPL100, Model3_FPL100)

## Model4_FPL100 <- lm(FPL100_noBaseline ~ gender_ms + race + disb_wrk_ageR2*race , data = Data_forIncPov)
## summary(Model4_FPL100)

## Model5_FPL100 <- lm(FPL100_noBaseline ~ gender_ms + race + disb_wrk_ageR2*gender_ms, data = Data_forIncPov)
## summary(Model5_FPL100)
## anova(Model5_FPL100)

## Model6_FPL100 <- lm(FPL100_noBaseline ~ yearqtr + gender_ms + race + disb_wrk_ageR2*gender_ms, data = Data_forIncPov)
## summary(Model6_FPL100)

## Model7_FPL100 <- lm(FPL100_noBaseline ~ yearqtr + gender_ms + race + disb_wrk_ageR2*gender_ms +
##                     FPL100_norm_Lag, data = Data_forIncPov)
## summary(Model7_FPL100)

str(Data$erace)
str(Data$race)

Data$wt <- Data$wt/1000
#######################################################################
## Mixed Effects Model (MEM) of normalized FPL 100 
########################################################################
Time1 <- Sys.time()
Model1 <- lmer(FPL100_noBaseline ~ 1 + race + (1 | ssuid), data=Data, REML=TRUE)
summary(Model1)

Time1 <- Sys.time()
MEM1_FPL100 <- lmer(FPL100_noBaseline ~ 1 + gender_ms + race + adult_disb + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM1_FPL100)
anova(MEM1_FPL100)
step(model = MEM1_FPL100, reduce.random = T)
Resid_MEM1 <- residuals(MEM1_FPL100)
ResidPlot_MEM1 <- qplot() + geom_point(aes(x = 1:length(Resid_MEM1), y = Resid_MEM1))

MEM2_FPL100 <- lmer(FPL100_noBaseline ~ 1 + gender_ms + race + adult_disb + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM2_FPL100)

MEM3_FPL100 <- lmer(FPL100_noBaseline ~ 1 + gender_ms + erace + adult_disb + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM3_FPL100)
anova(MEM3_FPL100)

MEM4_FPL100 <- lmer(FPL100_noBaseline ~ 1 + gender_ms + adult_disb + adult_disb*race + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM4_FPL100)

MEM1_FPL100_wt <- lmer(FPL100_noBaseline ~ 1 + gender_ms + erace + adult_disb + wt + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM1_FPL100_wt)
anova(MEM1_FPL100_wt)
step(model = MEM1_FPL100_wt, reduce.random = T)

MEM2_FPL100_wt <- lmer(FPL100_noBaseline ~ 1 + gender_ms + erace + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM2_FPL100_wt)

MEM3_FPL100_wt <- lmer(FPL100_noBaseline ~ 1 + gender_ms + erace + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM3_FPL100_wt)
anova(MEM3_FPL100_wt)

MEM4_FPL100_wt <- lmer(FPL100_noBaseline ~ 1 + gender_ms + adult_disb + adult_disb*race + wt + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM4_FPL100_wt)
anova(MEM4_FPL100_wt)
step(model = MEM4_FPL100_wt, reduce.random = T)
Resid_MEM4_wt <- residuals(MEM4_FPL100_wt)
ResidPlot_MEM4_wt <- qplot() + geom_point(aes(x = 1:length(Resid_MEM4_wt), y = Resid_MEM4_wt))
acf(x = Resid_MEM4_wt)

MEM5_FPL100_wt <- lmer(FPL100_noBaseline ~ 1 + FPL100_norm_Lag + gender_ms + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), 
                       data = Data, REML=TRUE)
summary(MEM5_FPL100_wt)
anova(MEM5_FPL100_wt)
step(model = MEM5_FPL100_wt, reduce.random = T)
Resid_MEM5_wt <- residuals(MEM5_FPL100_wt)
ResidPlot_MEM5_wt <- qplot() + geom_point(aes(x = 1:length(Resid_MEM5_wt), y = Resid_MEM5_wt))

Time2 <- Sys.time()
print(Time2 - Time1)





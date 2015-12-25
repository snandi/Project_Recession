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
Filename <- paste0(RDataPath, 'Data_forIncPov_v2.RData')
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
Data$Recession <- mapvalues(as.factor(Data$yearqtr),
				from = c("2008 Q2", "2008 Q3", "2008 Q4", 
				"2009 Q1", "2009 Q2", "2009 Q3", "2009 Q4",
				"2010 Q1", "2010 Q2", "2010 Q3", "2010 Q4", 
				"2011 Q1", "2011 Q2", "2011 Q3", "2011 Q4"),
 				to = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, 
					FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
)
Data$disab_Recession <- with(Data, interaction(adult_disb, Recession))

#######################################################################
## Linear Model of normalized FPL 100 
########################################################################
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
#######################################################################
## Mixed Effects Model (MEM) of normalized FPL 100 
########################################################################
Time1 <- Sys.time()
Model1 <- lmer(FPL100_noBaseline ~ 1 + race + (1 | ssuid), data=Data, REML=TRUE)
summary(Model1)

Time1 <- Sys.time()
MEM1_FPL100 <- lmer(FPL100_noBaseline ~ 1 + gender_ms + race + adult_disb + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM1_FPL100)

MEM2_FPL100 <- lmer(FPL100_noBaseline ~ 1 + gender_ms + race + adult_disb + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM2_FPL100)

MEM3_FPL100 <- lmer(FPL100_noBaseline ~ 1 + gender_ms + erace + adult_disb + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM3_FPL100)

MEM4_FPL100 <- lmer(FPL100_noBaseline ~ 1 + gender_ms + adult_disb + adult_disb*race + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM4_FPL100)

MEM1_FPL100_wt <- lmer(FPL100_noBaseline ~ 1 + gender_ms + race + adult_disb + wt + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM1_FPL100_wt)

MEM2_FPL100_wt <- lmer(FPL100_noBaseline ~ 1 + gender_ms + race + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM2_FPL100_wt)

MEM3_FPL100_wt <- lmer(FPL100_noBaseline ~ 1 + gender_ms + erace + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM3_FPL100)

MEM4_FPL100_wt <- lmer(FPL100_noBaseline ~ 1 + gender_ms + adult_disb + adult_disb*race + wt + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM4_FPL100_wt)

Time2 <- Sys.time()
print(Time2 - Time1)

Time1 <- Sys.time()
MEM1_FPL200 <- lmer(FPL200_noBaseline ~ 1 + gender_ms + race + adult_disb + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM1_FPL200)

MEM2_FPL200 <- lmer(FPL200_noBaseline ~ 1 + gender_ms + race + adult_disb + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM2_FPL200)

MEM3_FPL200 <- lmer(FPL200_noBaseline ~ 1 + gender_ms + erace + adult_disb + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM3_FPL200)

MEM4_FPL200 <- lmer(FPL200_noBaseline ~ 1 + gender_ms + adult_disb + adult_disb*race + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM4_FPL200)

MEM1_FPL200_wt <- lmer(FPL200_noBaseline ~ 1 + gender_ms + race + adult_disb + wt + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM1_FPL200_wt)

MEM2_FPL200_wt <- lmer(FPL200_noBaseline ~ 1 + gender_ms + race + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM2_FPL200_wt)

MEM3_FPL200_wt <- lmer(FPL200_noBaseline ~ 1 + gender_ms + erace + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM3_FPL200)

MEM4_FPL200_wt <- lmer(FPL200_noBaseline ~ 1 + gender_ms + adult_disb + adult_disb*race + wt + adult_disb*gender_ms + (1 | ssuid), data=Data, REML=TRUE)
summary(MEM4_FPL200_wt)

Time2 <- Sys.time()
print(Time2 - Time1)


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
## Mixed Effects Model (MEM) of Income Poverty Ratio
########################################################################
Data$wt <- Data$wt/1000

Num1_FPL100_wt <- lmer(FPL100_num ~ 1 + gender_ms + erace + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), 
                       data = Data, REML = TRUE)
summary(Num1_FPL100_wt)
anova(Num1_FPL100_wt)
step(Num1_FPL100_wt)

Num2_FPL100_wt <- lmer(FPL100_num ~ 1 + FPL100_num_Lag + gender_ms + erace + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), 
                       data = Data, REML = TRUE)
summary(Num2_FPL100_wt)
anova(Num2_FPL100_wt)
step(Num2_FPL100_wt)

anova(Num1_FPL100_wt, Num2_FPL100_wt)

Num3_FPL100_wt <- lmer(FPL100_num ~ 1 + FPL100_num_Lag + education + gender_ms + erace + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), 
                       data = Data, REML = TRUE)
summary(Num3_FPL100_wt)
anova(Num3_FPL100_wt)
step(Num3_FPL100_wt)

#anova(Num2_FPL100_wt, Num3_FPL100_wt)

#Num4_FPL100_wt <- lmer(FPL100_num ~ 1 + FPL100_num_Lag + year + gender_ms + erace + adult_disb + wt + adult_disb*gender_ms + adult_disb*year + (1 | ssuid), 
#                       data = Data, REML = TRUE)
#summary(Num4_FPL100_wt)
#anova(Num4_FPL100_wt)
#step(Num4_FPL100_wt)

#######################################################################
## Mixed Effects Model (MEM) of normalized FPL 100 
#######################################################################
MEM1_FPL100_wt <- lmer(FPL100_noBaseline ~ 1 + gender_ms + erace + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), 
                    data = Data, REML = TRUE)
summary(MEM1_FPL100_wt)
anova(MEM1_FPL100_wt)
step(MEM1_FPL100_wt)

MEM2_FPL100_wt <- lmer(FPL100_noBaseline ~ 1 + FPL100_norm_Lag + gender_ms + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), 
                    data = Data, REML = TRUE)
anova(MEM2_FPL100_wt)
summary(MEM2_FPL100_wt)
step(MEM2_FPL100_wt)

MEM3_FPL100_wt <- lmer(FPL100_noBaseline ~ 1 + FPL100_norm_Lag + education + gender_ms + adult_disb + wt + adult_disb*gender_ms + (1 | ssuid), 
                    data = Data, REML = TRUE)
anova(MEM3_FPL100_wt)
summary(MEM3_FPL100_wt)
step(MEM3_FPL100_wt)

#######################################################################
## Residuals
#######################################################################
Data$Resid_Num1 <- residuals(Num1_FPL100_wt)
Data$Resid_Num2 <- residuals(Num2_FPL100_wt) 
Data$Resid_Num3 <- residuals(Num3_FPL100_wt)
Data$Resid_Num4 <- residuals(Num4_FPL100_wt)

Data$Resid_MEM1 <- residuals(MEM1_FPL100_wt)
Data$Resid_MEM2 <- residuals(MEM2_FPL100_wt)
Data$Resid_MEM3 <- residuals(MEM3_FPL100_wt)

Filename <- paste0(RDataPath, 'Data_forIncPov_v3_Residuals.RData')
save(Data, file = Filename)



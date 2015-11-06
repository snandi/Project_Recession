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
DataPath <- '~/Project_Recession/Data/'
RDataPath <- '~/Project_Recession/RData/'
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

Filename <- paste0(RDataPath, 'Data_forIncPov.RData')
load(file = Filename)

Data_forIncPov <- subset(Data_forIncPov, yearmon != 'May 2008')
Data_forIncPov$FPL100_pct <- Data_forIncPov$thtotinc/Data_forIncPov$rhpov
Data_forIncPov$FPL200_pct <- Data_forIncPov$thtotinc/Data_forIncPov$rhpov2

#View(Data_forIncPov[1:50,])
Data_Sub <- subset(Data_forIncPov, yearmon %in% c('Jun 2008', 'Jul 2008', 'Aug 2008', 'Sep 2008',
                                                  'Oct 2008', 'Nov 2008', 'Dec 2008', 'Jan 2009',
                                                  'Feb 2009', 'Mar 2009', 'Apr 2009'))

########################################################################
## Model 1
########################################################################
## Model1 <- lm(FPL100_pct ~ yearmon + gender_ms + race + disb_wrk_ageR2, data = Data_Sub)

Model1 <- lm(FPL100_pct ~ race, data = Data_forIncPov)
summary(Model1)
Model2 <- lm(FPL100_pct ~ race + gender_ms, data = Data_forIncPov)
summary(Model2)
anova(Model1, Model2)
Model3 <- lm(FPL100_pct ~ race + gender_ms +  disb_wrk_ageR2, data = Data_forIncPov)
summary(Model3)
anova(Model2, Model3)
Model4 <- lm(FPL100_pct ~ yearmon + race + gender_ms +  disb_wrk_ageR2, data = Data_forIncPov)
summary(Model4)
anova(Model3, Model4)

Plots4 <- diagPlot(model = Model4)
Filename.plot <- paste0(PlotPath, 'ResidualPlots.pdf')
pdf(file = Filename.plot)
Plots4[[1]]
Plots4[[2]]
Plots4[[3]]
Plots4[[4]]
Plots4[[5]]
Plots4[[6]]
dev.off()


                                        





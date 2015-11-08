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

Filename <- paste0(RDataPath, 'Data_forIncPov_byGender.RData')
load(file = Filename)

ssuids <- unique(Data_forIncPov$ssuid)
#Data_Sub <- subset(Data_forIncPov, yearmon == 'Jun 2008')
Data_Sub <- subset(Data_forIncPov, ssuid %in% ssuids[1:10])
head(Data_forIncPov)

Data <- Data_forIncPov_byGender
########################################################################
## Model 1
########################################################################
## Model1 <- lm(FPL100 ~ yearmon + gender_ms + race + disb_wrk_ageR2, data = Data_Sub)
Model0 <- lm(FPL100_num_Norm ~ yearmon, data = Data)

Model1 <- lm(FPL100_num ~ race, data = Data_forIncPov)
summary(Model1)
Model2 <- lm(FPL100_num ~ race + gender_ms, data = Data_forIncPov)
summary(Model2)
anova(Model1, Model2)
Model3 <- lm(FPL100_num ~ race + gender_ms +  disb_wrk_ageR2, data = Data_forIncPov)
summary(Model3)
anova(Model2, Model3)
Model4 <- lm(FPL100_num ~ yearmon + race + gender_ms +  disb_wrk_ageR2, data = Data_forIncPov)
summary(Model4)
anova(Model3, Model4)

Plots4 <- diagPlot(model = Model4)
#Filename.plot <- paste0(PlotPath, 'ResidualPlots.pdf')
#pdf(file = Filename.plot)
#Plots4[[1]]
#Plots4[[2]]
#Plots4[[3]]
#Plots4[[4]]
#Plots4[[5]]
#Plots4[[6]]
dev.off()


                                        





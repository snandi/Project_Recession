rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script prepares the data for safety net program analysis 
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
SlidePath <- paste0(PathPrefix, 'Project_Recession/Slides_Recession/')
########################################################################
Today <- Sys.Date()

## Load weights
Filename <- paste0(RDataPath, 'Weights_Long.RData')
load(Filename)

########################################################################
## load the merged dataset
########################################################################
Filepath1 <- paste(RDataPath, 'Data15.RData', sep = '')
load(Filepath1)

length(unique(as.numeric(Data15$ssuid)))
nrow(unique(Data15[,c('ssuid', 'ehrefper')]))
nrow(unique(Data15[,c('ssuid', 'epppnum')]))
## 22002 unique ssuids, and 1 ehrefper per hh

Data15$yearmon <- as.yearmon(paste(Data15$rhcalmn, Data15$rhcalyr))
Data15$yearqtr <- as.yearqtr(Data15$yearmon)
Data15 <- Data15[order(Data15$ssuid, Data15$shhadid, Data15$yearmon),]

##Variable names for safety net:
#thsocsec: Total Household Social Security Income Recode 
#thssi: Total Household Supplemental Security Income Recode 
#thunemp: Total Household Unemployment Income Recode 
#thafdc: Total household public assistance payments 
#thfdstp:Total Household food stamps Received Recode  
#thnoncsh: Total Household Noncash Income Recode 

### Define a SafetyNet participation variable (categorical)
Data15$SafetyNetParticipate <- (rowSums(Data15[,c('thsocsec', 'thssi', 'thunemp', 'thafdc', 'thnoncsh')]) > 0)

DisabOnly <- subset(Data15, adult_disb == 1)
DisabOnly <- subset(DisabOnly, yearmon != 'Jun 2008')
DisabOnly <- subset(DisabOnly, yearmon != 'Jul 2008')
DisabOnly <- subset(DisabOnly, yearmon != 'May 2013')
DisabOnly <- subset(DisabOnly, yearmon != 'Jun 2013')

HH_Num <- length(unique(DisabOnly$ssuid))
SafetyNet_yrmon <- aggregate(SafetyNetParticipate ~ yearmon, data = DisabOnly, FUN = sum)
SafetyNet_yrmon$Pct <- SafetyNet_yrmon$SafetyNetParticipate/HH_Num

Plot1 <- qplot() + geom_line(aes(x = as.Date(yearmon), y = Pct), data = SafetyNet_yrmon, size = 1) +
  ggtitle(label = 'Safety net participation of households with disability') +
  xlab(label = '') + ylab(label = 'percentage')

SafetyNet_yrmon_race <- aggregate(SafetyNetParticipate ~ yearmon + erace, data = DisabOnly, FUN = sum)
Race_yrmon <- aggregate(ssuid ~ yearmon + erace, data = DisabOnly, FUN = length)
SafetyNet_yrmon_race <- merge(
  x = SafetyNet_yrmon_race,
  y = Race_yrmon,
  by = c('yearmon', 'erace')
)
SafetyNet_yrmon_race$Pct <- SafetyNet_yrmon_race$SafetyNetParticipate/SafetyNet_yrmon_race$ssuid

Plot2 <- qplot() + geom_line(aes(x = as.Date(yearmon), y = Pct, color = erace), 
                             data = SafetyNet_yrmon_race, size = 1) +
  ggtitle(label = 'Safety net participation of households with disability, by race') +
  xlab(label = '') + ylab(label = 'percentage')
Plot2


########################################################################
## Get Income Poverty by Gender & Marital status of head of household
########################################################################
DisabOnly$ms <- mapvalues(
  DisabOnly$ems,
  from = c("Married, spouse present", "Married, spouse absent",
           "Widowed", "Divorced", "Separated",
           "Never Married"),
  to = c("Married", "Married", rep("Not married", 4))
)
DisabOnly$gender_ms <- with(DisabOnly, interaction(esex, ms))

SafetyNet_yrmon_gender_ms <- aggregate(SafetyNetParticipate ~ yearmon + gender_ms, data = DisabOnly, FUN = sum)
gender_ms_yrmon <- aggregate(ssuid ~ yearmon + gender_ms, data = DisabOnly, FUN = length)
SafetyNet_yrmon_gender_ms <- merge(
  x = SafetyNet_yrmon_gender_ms,
  y = gender_ms_yrmon,
  by = c('yearmon', 'gender_ms')
)
SafetyNet_yrmon_gender_ms$Pct <- SafetyNet_yrmon_gender_ms$SafetyNetParticipate / SafetyNet_yrmon_gender_ms$ssuid

Plot3 <- qplot() + geom_line(aes(x = as.Date(yearmon), y = Pct, color = gender_ms), data = SafetyNet_yrmon_gender_ms, size = 1) +
  ggtitle(label = 'Safety net participation of households with disability, \n by gender and marital status') +
  xlab(label = '') + ylab(label = 'percentage') +
  theme(legend.position = 'top')
Plot3

Filename.plot <- paste0(PlotPath, 'Safetynet_Plots.pdf')
pdf(file = Filename.plot, onefile = T)
Plot1
Plot2
Plot3
dev.off()


Data_race <- SafetyNet_yrmon_race
Asian <- subset(Data_race, erace == 'Asian alone' & yearmon == 'Aug 2008')$Pct
Black <- subset(Data_race, erace == 'Black alone' & yearmon == 'Aug 2008')$Pct
White <- subset(Data_race, erace == 'White alone' & yearmon == 'Aug 2008')$Pct
Resid <- subset(Data_race, erace == 'Residual' & yearmon == 'Aug 2008')$Pct
Baseline <- as.data.frame(
  cbind(erace = c('Asian alone', 'Black alone', 'White alone', 'Residual'),
        baseline = c(Asian, Black, White, Resid)),
  stringsAsFactors = F
)
Baseline$baseline <- as.numeric(Baseline$baseline)

Data_race <- merge(
  x = Data_race,
  y = Baseline,
  by = 'erace'
)

Data_race$Pct_norm <- Data_race$Pct - Data_race$baseline

anova(lm(Pct_norm ~ yearmon + erace, data = Data_race))
summary(lm(Pct_norm ~ yearmon + erace, data = Data_race))

Plot2a <- qplot() + geom_line(aes(x = as.Date(yearmon), y = Pct_norm, color = erace), data = Data_race, size = 1) +
  ggtitle(label = 'Safety net participation of households with disability, by race') +
  xlab(label = '') + ylab(label = 'percentage')
Plot2a

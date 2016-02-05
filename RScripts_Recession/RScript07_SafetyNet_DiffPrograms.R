rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This analyzes participation in different safety net programs, for 
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
Data15 <- Data15[order(Data15$ssuid, Data15$yearmon),]

##Variable names for safety net:
#thsocsec: Total Household Social Security Income Recode 
#thssi: Total Household Supplemental Security Income Recode 
#thunemp: Total Household Unemployment Income Recode 
#thafdc: Total household public assistance payments 
#thfdstp:Total Household food stamps Received Recode  
#thnoncsh: Total Household Noncash Income Recode 

### Define a SafetyNet participation variable (categorical)
Data15$SafetyNetParticipate <- (rowSums(Data15[,c('thsocsec', 'thssi', 'thunemp', 'thafdc', 'thnoncsh')]) > 0)

Data15 <- subset(Data15, yearmon != 'May 2008')
Data15 <- subset(Data15, yearmon != 'Jun 2008')
Data15 <- subset(Data15, yearmon != 'Jul 2008')
Data15 <- subset(Data15, yearmon != 'May 2013')
Data15 <- subset(Data15, yearmon != 'Jun 2013')
Data15 <- subset(Data15, yearmon != 'Jul 2013')

Data <- Data15
########################################################################
## Define new factor: Gender and Marital status of head of household
########################################################################
Data$ms <- mapvalues(
  Data$ems,
  from = c("Married, spouse present", "Married, spouse absent",
           "Widowed", "Divorced", "Separated",
           "Never Married"),
  to = c("Married", "Married", rep("Not married", 4))
)
Data$gender_ms <- with(Data, interaction(esex, ms))

Data$year <- format(Data$yearqtr, "%Y")

########################################################################
## Define new factor: race
########################################################################
Data$race <- mapvalues(
  Data$erace,
  from = c("White alone", "Black alone", "Asian alone", "Residual"),
  to = c('White', 'Black', 'Others', 'Others')
)

Data$adult_disb <- as.factor(Data$adult_disb)
Data$adult_disb <- mapvalues(
  Data$adult_disb,
  from = c("0", "1"),
  to = c("Not Disabled", "Disabled")
)

########################################################################
# thsocsec: Total Household Social Security Income
########################################################################
thsocsec <- fn_separateSafetyNet(
  Data = Data, 
  ProgramVar = 'thsocsec', 
  Maintitle = 'Total Household Social Security Income, per household',
  ylabel = 'thsocsec per household'
)

# thsocsec_yrmon <- aggregate(thsocsec ~ yearmon + gender_ms + erace + adult_disb, data = Data, FUN = sum)
# HH_yrmon <- aggregate(ssuid ~ yearmon + gender_ms + erace + adult_disb, data = Data, FUN = length)
# thsocsec_yrmon <- merge(
#   x = thsocsec_yrmon,
#   y = HH_yrmon,
#   by = c('yearmon', 'erace', 'gender_ms', 'adult_disb')
# )
# thsocsec_yrmon$thsocsec_perhh <- thsocsec_yrmon$thsocsec/thsocsec_yrmon$ssuid
# 
# Plot1 <- qplot() + geom_line(aes(x = as.Date(yearmon), y = thsocsec_perhh, group = factor(adult_disb), col = factor(adult_disb)), 
#   data = thsocsec_yrmon, size = 1) +
#   ggtitle(label = 'Total Household Social Security Income, per household') +
#   xlab(label = '') + ylab(label = 'thsocsec per household') +
# #   facet_grid(race ~ gender_ms, scales = 'free_y') +
#   facet_grid(erace ~ gender_ms) +
#   theme(legend.position = 'top')
# 
# Plot1

########################################################################
# thfdstp: Total Household food stamps Received 
########################################################################
thfdstp <- fn_separateSafetyNet(
  Data = Data, 
  ProgramVar = 'thfdstp', 
  Maintitle = 'Total Household food stamps Received',
  ylabel = 'thfdstp per household'
)

# thfdstp_yrmon <- aggregate(thfdstp ~ yearmon + gender_ms + race + adult_disb, data = Data, FUN = sum)
# 
# Plot2 <- qplot() + geom_line(aes(x = as.Date(yearmon), y = thfdstp, group = factor(adult_disb), col = factor(adult_disb)), 
#   data = thfdstp_yrmon, size = 1) +
#   ggtitle(label = 'Total Household food stamps Received') +
#   xlab(label = '') + ylab(label = 'thfdstp') +
#   facet_grid(race ~ gender_ms, scales = 'free_y') +
#   theme(legend.position = 'top')
# 
# Plot2

########################################################################
# thunemp: Total Household Unemployment Income
########################################################################
thunemp <- fn_separateSafetyNet(
  Data = Data, 
  ProgramVar = 'thunemp', 
  Maintitle = 'Total Household Unemployment Income',
  ylabel = 'thunemp per household'
)

# thunemp_yrmon <- aggregate(thunemp ~ yearmon + gender_ms + race + adult_disb, data = Data, FUN = sum)
# 
# Plot3 <- qplot() + geom_line(aes(x = as.Date(yearmon), y = thunemp, group = factor(adult_disb), col = factor(adult_disb)), 
#   data = thunemp_yrmon, size = 1) +
#   ggtitle(label = 'Total Household Unemployment Income') +
#   xlab(label = '') + ylab(label = 'thunemp') +
#   facet_grid(race ~ gender_ms, scales = 'free_y') +
#   theme(legend.position = 'top')
# 
# Plot3

########################################################################
# thafdc: Total household public assistance payments
########################################################################
thafdc <- fn_separateSafetyNet(
  Data = Data, 
  ProgramVar = 'thafdc', 
  Maintitle = 'Total household public assistance payments',
  ylabel = 'thafdc per household'
)

# thafdc_yrmon <- aggregate(thafdc ~ yearmon + gender_ms + race + adult_disb, data = Data, FUN = sum)
# 
# Plot4 <- qplot() + geom_line(aes(x = as.Date(yearmon), y = thafdc, group = factor(adult_disb), col = factor(adult_disb)), 
#   data = thafdc_yrmon, size = 1) +
#   ggtitle(label = 'Total household public assistance payments') +
#   xlab(label = '') + ylab(label = 'thafdc') +
#   facet_grid(race ~ gender_ms, scales = 'free_y') +
#   theme(legend.position = 'top')
# 
# Plot4

########################################################################
# thssi: Total Household Supplemental Security Income
########################################################################
thssi <- fn_separateSafetyNet(
  Data = Data, 
  ProgramVar = 'thssi', 
  Maintitle = 'Total Household Supplemental Security Income',
  ylabel = 'thssi per household'
)

# thssi_yrmon <- aggregate(thssi ~ yearmon + gender_ms + race + adult_disb, data = Data, FUN = sum)
# 
# Plot5 <- qplot() + geom_line(aes(x = as.Date(yearmon), y = thssi, group = factor(adult_disb), col = factor(adult_disb)), 
#   data = thssi_yrmon, size = 1) +
#   ggtitle(label = 'Total Household Supplemental Security Income') +
#   xlab(label = '') + ylab(label = 'thssi') +
#   facet_grid(race ~ gender_ms, scales = 'free_y') +
#   theme(legend.position = 'top')
# 
# Plot5

Filename.plot <- paste0(PlotPath, 'Separate_Safetynet_Plots_perHH.pdf')
pdf(file = Filename.plot, onefile = T)
thsocsec$Plot
thfdstp$Plot
thunemp$Plot
thafdc$Plot
thssi$Plot
dev.off()


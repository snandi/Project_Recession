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
### Define SafetyNet participation variables (categorical)
########################################################################
Data$socsec_part <- (Data[,'thsocsec'] > 0)
Data$ssi_part <- (Data[,'thssi'] > 0)
Data$unemp_part <- (Data[,'thunemp'] > 0)
Data$afdc_part <- (Data[,'thafdc'] > 0)
Data$noncsh_part <- (Data[,'thnoncsh'] > 0)
Data$fdstp_part <- (Data[,'thfdstp'] > 0)

########################################################################
### Merge the weights
########################################################################

########################################################################
# thsocsec: Total Household Social Security Income
########################################################################
socsec <- fn_separateSafetyNet(
  Data = Data, 
  ProgramVar = 'socsec_part', 
  Maintitle = 'Social Security Income participation',
  ylabel = 'percentage of household'
)

########################################################################
# thfdstp: Total Household food stamps Received 
########################################################################
fdstp <- fn_separateSafetyNet(
  Data = Data, 
  ProgramVar = 'fdstp_part', 
  Maintitle = 'Food stamps participation',
  ylabel = 'percentage of household'
)

########################################################################
# thunemp: Total Household Unemployment Income
########################################################################
unemp <- fn_separateSafetyNet(
  Data = Data, 
  ProgramVar = 'unemp_part', 
  Maintitle = 'Unemployment Income participation',
  ylabel = 'percentage of household'
)
unemp$Plot

########################################################################
# thafdc: Total household public assistance payments
########################################################################
afdc <- fn_separateSafetyNet(
  Data = Data, 
  ProgramVar = 'afdc_part', 
  Maintitle = 'Public assistance payments participation',
  ylabel = 'percentage of household'
)
afdc$Plot

########################################################################
# thssi: Total Household Supplemental Security Income
########################################################################
ssi <- fn_separateSafetyNet(
  Data = Data, 
  ProgramVar = 'ssi_part', 
  Maintitle = 'Supplemental Security Income participation',
  ylabel = 'percentage of household'
)
ssi$Plot

########################################################################
# thnoncsh: Total Household Noncash Income 
########################################################################
noncsh <- fn_separateSafetyNet(
  Data = Data, 
  ProgramVar = 'noncsh_part', 
  Maintitle = 'Noncash Income participation',
  ylabel = 'percentage of household'
)

########################################################################
# Combine all participations
########################################################################
Participation <- rbind(cbind(socsec$Program_yrmon, Program_factor = 'SocSec'), 
                       cbind(fdstp$Program_yrmon, Program_factor = 'FdStp'), 
                       cbind(unemp$Program_yrmon, Program_factor = 'Unemp'), 
                       cbind(ssi$Program_yrmon, Program_factor = 'SSI'), 
#                        cbind(noncsh$Program_yrmon, Program_factor = 'NonCash'),
                       cbind(afdc$Program_yrmon, Program_factor = 'Afdc')
)
str(Participation)

Plot_Disabled <- qplot() + 
  geom_line(aes(x = as.Date(yearmon), y = Program_perhh, 
                group = Program_factor, col = Program_factor), 
            data = subset(Participation, adult_disb == 'Disabled'), size = 1) +
  ggtitle(label = 'Participation of disabled families') +
  xlab(label = '') + ylab(label = 'percentage of household') +
  #   facet_grid(race ~ gender_ms, scales = 'free_y') +
  facet_grid(erace ~ gender_ms) +
  theme(legend.position = 'top', 
        axis.text.x = element_text(angle = 90, hjust = 0))

Plot_Disabled_noSocsec <- qplot() + 
  geom_line(aes(x = as.Date(yearmon), y = Program_perhh, 
                group = Program_factor, col = Program_factor), 
            data = subset(Participation, adult_disb == 'Disabled' & Program_factor != 'SocSec'), size = 1) +
  ggtitle(label = 'Participation of disabled families, without SocSec') +
  xlab(label = '') + ylab(label = 'percentage of household') +
  #   facet_grid(race ~ gender_ms, scales = 'free_y') +
  facet_grid(erace ~ gender_ms) +
  theme(legend.position = 'top', 
        axis.text.x = element_text(angle = 90, hjust = 0))

Plot_NotDisabled <- qplot() + 
  geom_line(aes(x = as.Date(yearmon), y = Program_perhh, 
                group = Program_factor, col = Program_factor), 
            data = subset(Participation, adult_disb == 'Not Disabled'), size = 1) +
  ggtitle(label = 'Participation of non disabled families') +
  xlab(label = '') + ylab(label = 'percentage of household') +
  #   facet_grid(race ~ gender_ms, scales = 'free_y') +
  facet_grid(erace ~ gender_ms) +
  theme(legend.position = 'top', 
        axis.text.x = element_text(angle = 90, hjust = 0))

Plot_NotDisabled_noSocsec <- qplot() + 
  geom_line(aes(x = as.Date(yearmon), y = Program_perhh, 
                group = Program_factor, col = Program_factor), 
            data = subset(Participation, adult_disb == 'Not Disabled' & Program_factor != 'SocSec'), size = 1) +
  ggtitle(label = 'Participation of non disabled families, without SocSec') +
  xlab(label = '') + ylab(label = 'percentage of household') +
  #   facet_grid(race ~ gender_ms, scales = 'free_y') +
  facet_grid(erace ~ gender_ms) +
  theme(legend.position = 'top', 
        axis.text.x = element_text(angle = 90, hjust = 0))

Filename.plot <- paste0(PlotPath, 'Safetynet_Plots_participateOnly.pdf')
pdf(file = Filename.plot, onefile = T)
Plot_Disabled
Plot_NotDisabled
Plot_Disabled_noSocsec
Plot_NotDisabled_noSocsec
socsec$Plot
fdstp$Plot
unemp$Plot
afdc$Plot
ssi$Plot
#noncsh$Plot
dev.off()

Data_Participation <- rbind(cbind(fdstp$Program_yrmon, Program_factor = 'FdStp'), 
                       cbind(unemp$Program_yrmon, Program_factor = 'Unemp'), 
                       cbind(ssi$Program_yrmon, Program_factor = 'SSI')
)
str(Data_Participation)

Filename.RData <- paste0(RDataPath, 'Safetynet_participateOnly.RData')
save(Data_Participation, file = Filename.RData)


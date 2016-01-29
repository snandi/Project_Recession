rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script produces the data for income poverty analysis 
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
## load sipp08_MASTER.RData
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

Colnames_keep <- c('ssuid',
                   'shhadid',
                   'yearmon',
                   'yearqtr',
                   'ehrefper',
                   'epppnum',
                   'rhtype',
                   'whfnwgt',
                   'thtotinc',
                   'rhpov',
                   'erace',
                   'esex',
                   'ems',
                   'eeducate',
                   'adult_disb')
Data <- unique(Data15[, Colnames_keep])
Data <- subset(Data, yearmon != 'May 2008')

########################################################################
## Map education levels
########################################################################
Data$education <- mapvalues(
  Data$eeducate,
  from = c("Not in Universe", "Less Than 1st Grade", "1st, 2nd, 3rd or 4th grade", "5th Or 6th Grade", 
           "7th Or 8th Grade", "9th Grade", "10th Grade", "11th Grade", "12th grade, no diploma", 
           "High School Graduate - (diploma", "Some college, but no degree", "Diploma or certificate from a", 
           "Associate (2-yr) college degree", "Bachelor's degree (for example:", "Master's degree (For example: MA,", 
           "Professional School degree (for", "Doctorate degree (for example:"),
  to = c('High School or less', 'High School or less', 'High School or less', 'High School or less', 
         'High School or less', 'High School or less', 'High School or less', 'High School or less', 
         'High School or less', 'High School or less', 'Some college, diploma, assoc', 
         'Some college, diploma, assoc','Some college, diploma, assoc', 'Bachelors or higher', 'Bachelors or higher', 
         'Bachelors or higher', 'Bachelors or higher')
)

########################################################################
## Get Income Poverty by Race
########################################################################
Data$race <- mapvalues(
  Data$erace,
  from = c("White alone", "Black alone", "Asian alone", "Residual"),
  to = c('White', 'Black', 'Others', 'Others')
)

########################################################################
## Get Income Poverty by Gender & Marital status of head of household
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
## Add weights
########################################################################
Data <- merge(
  x = Data, 
  y = Weights_Long[,c('ssuid', 'epppnum', 'year', 'weight')], 
  by = c('ssuid', 'epppnum', 'year'), 
  all.x = T, 
  all.y = F
)
summary(Data$weight)
colnames(Data)[colnames(Data) == 'weight'] <- 'wt'
MedianWeights <- aggregate(weight ~ year, data = Weights_Long, FUN = median)
colnames(MedianWeights) <- c('year', 'median_wt')

Data <- merge(
  x = Data,
  y = MedianWeights,
  by = 'year'
)

Data$wt <- na.is.zero(Data$wt)

Data$wt[Data$wt == 0] <- Data$median_wt[Data$wt == 0]

## NoWeights <- unique(subset(Data, wt == 0)[,'ssuid'])
## length(unique(Data$ssuid))
########################################################################
## Get Income Poverty by Race, Gender & Marital status of head of household
########################################################################
## Temp <- aggregate(cbind(thtotinc, rhpov, disb_wrk_ageR2) ~ ssuid + shhadid + yearmon + gender_ms + race, 
##                   data = Data15, FUN = mean)
Temp <- aggregate(
  cbind(thtotinc, rhpov, adult_disb) ~ ssuid + shhadid + yearqtr + gender_ms + race + erace + education + wt, 
  data = Data,
  FUN = mean
)

## Data_forIncPov <- fn_DataforIncPov(Data = Temp)
## Filename <- paste0(RDataPath, 'Data_forIncPov.RData')
## save(Data_forIncPov, file = Filename)
## rm(Temp, Data_forIncPov)
## gc()

Data_forIncPov <- fn_DataforIncPov_v2(Data = Temp)
Filename <- paste0(RDataPath, 'Data_forIncPov_v3.RData')
save(Data_forIncPov, file = Filename)
rm(Temp, Data_forIncPov)
gc()

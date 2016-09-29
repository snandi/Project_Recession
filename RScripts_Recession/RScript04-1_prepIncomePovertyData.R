rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script produces the data for income poverty analysis 
## This script was last edited on 09/28, adding variable eorigin, to 
## include hispanics in the analysis
########################################################################

########################################################################
## Run Path definition file                                           ##
########################################################################
PathPrefix <- '/Users/patron/Documents/snandi/'
PathPrefix <- '~/'
RScriptPath <- paste0(PathPrefix, 'Project_Recession/RScripts_Recession/')
DataPath <- paste0(PathPrefix, 'Project_Recession/Data/data_2015Dec/')
RDataPath <- paste0(PathPrefix, 'Project_Recession/RData/data_2015Dec/')
PlotPath <- paste0(PathPrefix, 'Project_Recession/Plots/')
Filename.Header <- paste0(PathPrefix, 'RScripts/HeaderFile_lmcg.R')
source(Filename.Header)

source(paste(RScriptPath, 'fn_Library_Recession.R', sep=''))
source('~/RScripts/fn_Library_SN.R')

########################################################################
Today <- Sys.Date()

########################################################################
## STATIC VARIABLES
########################################################################
EDUCATION_MAP <- as.data.frame(cbind( 
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
), stringsAsFactors = FALSE)

RACE_MAP <- as.data.frame(cbind(
  from = c("White alone", "Black alone", "Asian alone", "Residual"),
  to = c('White', 'Black', 'Others', 'Others')
), stringsAsFactors = FALSE)

ORIGIN_MAP <- as.data.frame(cbind(
  from = c("Yes", "No"),
  to = c('Hispanic', 'Non-Hispanic')
), stringsAsFactors = FALSE)

MARITAL_STATUS_MAP <- as.data.frame(cbind(
  from = c("Married, spouse present", "Married, spouse absent",
           "Widowed", "Divorced", "Separated",
           "Never Married"),
  to = c("Married", "Married", rep("Not married", 4))
), stringsAsFactors = FALSE)

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
                   'eorigin',
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
  from = EDUCATION_MAP$from,
  to = EDUCATION_MAP$to
)

########################################################################
## Map Race variable
########################################################################
Data$race <- mapvalues(
  Data$erace,
  from = RACE_MAP$from,
  to = RACE_MAP$to
)

########################################################################
## Map Origin variable
########################################################################
Data$origin <- mapvalues(
  Data$eorigin,
  from = ORIGIN_MAP$from,
  to = ORIGIN_MAP$to
)

########################################################################
## Combine Race and Origin
########################################################################
Data$race_origin <- as.vector( Data$origin )
Data$race_origin[Data$origin == 'Non-Hispanic'] <- as.vector( Data$race[Data$origin == 'Non-Hispanic'] )
Data$race_origin <- as.factor( Data$race_origin )

########################################################################
## Get Income Poverty by Gender & Marital status of head of household
########################################################################
Data$ms <- mapvalues(
  Data$ems,
  from = MARITAL_STATUS_MAP$from,
  to = MARITAL_STATUS_MAP$to
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
  cbind(thtotinc, rhpov, adult_disb) ~ ssuid + shhadid + yearqtr + gender_ms + race_origin +
    education + wt, 
  data = Data,
  FUN = mean
)

## Data_forIncPov <- fn_DataforIncPov(Data = Temp)
## Filename <- paste0(RDataPath, 'Data_forIncPov.RData')
## save(Data_forIncPov, file = Filename)
## rm(Temp, Data_forIncPov)
## gc()

Data_forIncPov <- fn_DataforIncPov_v2(Data = Temp)

########################################################################
## create a household id
########################################################################
Data_forIncPov$hhid <- paste(Data_forIncPov$ssuid, Data_forIncPov$shhadid, sep = '_')

########################################################################
## Remove negative thtotinc 
########################################################################
hhids_negative <- unique(subset(Data_forIncPov, thtotinc < 0)[,'hhid'])
hhids_nonnegative <- unique(Data_forIncPov[,'hhid']) %w/o% hhids_negative

Data_forIncPov <- subset(Data_forIncPov, hhid %in% hhids_nonnegative)
## Around 450 households were dropped

########################################################################
## save the data
########################################################################
Filename <- paste0(RDataPath, 'Data_forIncPov_v3.RData')
save(Data_forIncPov, file = Filename)
rm(Temp, Data_forIncPov)
gc()


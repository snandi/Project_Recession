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

########################################################################
## load sipp08_MASTER.RData
########################################################################
Filepath1 <- paste(RDataPath, 'Data15.RData', sep = '')
load(Filepath1)

length(unique(as.numeric(Data15$ssuid)))
## 25123 unique ssuids

Data15$yearmon <- as.yearmon(paste(Data15$rhcalmn, Data15$rhcalyr))
Data15$yearqtr <- as.yearqtr(Data15$yearmon)
Data15 <- Data15[order(Data15$ssuid, Data15$yearmon),]

Colnames_keep <- c('ssuid',
                   'shhadid',
                   'yearmon',
                   'yearqtr',
                   'ehrefper',
                   'rhtype',
                   'whfnwgt',
                   'thtotinc',
                   'rhpov',
                   'erace',
                   'esex',
                   'ems',
                   'epppnum',
                   'adult_disb')
Data <- unique(Data15[, Colnames_keep])
Data <- subset(Data, yearmon != 'May 2008')

### Keep only when epppnum == ehrefper
Data$epppnum <- as.numeric(Data$epppnum)
Data <- Data[Data$ehrefper == Data$epppnum,]

########################################################################
## Get Income Poverty by Race
########################################################################
Data$race <- mapvalues(Data$erace,
                                 from = c("White alone", "Black alone", "Asian alone", "Residual"),
                                 to = c('White', 'Black', 'Others', 'Others')
                                 )

########################################################################
## Get Income Poverty by Gender & Marital status of head of household
########################################################################
Data$ms <- mapvalues(Data$ems,
                               from = c("Married, spouse present", "Married, spouse absent",
                                 "Widowed", "Divorced", "Separated",
                                 "Never Married"),
                               to = c("Married", "Married", rep("Not married", 4))
                               )
Data$gender_ms <- with(Data, interaction(esex, ms))

########################################################################
## Get Income Poverty by Race, Gender & Marital status of head of household
########################################################################
## Temp <- aggregate(cbind(thtotinc, rhpov, disb_wrk_ageR2) ~ ssuid + shhadid + yearmon + gender_ms + race, 
##                   data = Data15, FUN = mean)
Temp <- aggregate(cbind(thtotinc, rhpov, adult_disb) ~ ssuid + shhadid + yearqtr + gender_ms + race + erace, 
                  data = Data, FUN = mean)

## Data_forIncPov <- fn_DataforIncPov(Data = Temp)
## Filename <- paste0(RDataPath, 'Data_forIncPov.RData')
## save(Data_forIncPov, file = Filename)
## rm(Temp, Data_forIncPov)
## gc()

Data_forIncPov <- fn_DataforIncPov_v2(Data = Temp)
Filename <- paste0(RDataPath, 'Data_forIncPov_v2.RData')
save(Data_forIncPov, file = Filename)
rm(Temp, Data_forIncPov)
gc()

rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script reads in sipp08_master & 2008_disability and merges them
## to produce data for income poverty analysis. This is similaryt to 
##
########################################################################

########################################################################
## Run Path definition file                                           ##
########################################################################
RScriptPath <- '~/Project_Recession/RScripts_Recession/'
DataPath <- '~/Project_Recession/Data/'
RDataPath <- '~/Project_Recession/RData/'
PlotPath <- '~/Project_Recession/Plots/'
Filename.Header <- paste('~/RScripts/HeaderFile_lmcg.R', sep='')
source(Filename.Header)
source(paste(RScriptPath, 'fn_Library_Recession.R', sep=''))
########################################################################
Today <- Sys.Date()

########################################################################
## load sipp08_MASTER.RData
########################################################################
Filepath1 <- paste(RDataPath, 'sipp08_MASTER.RData', sep = '')
load(Filepath1)
sipp08_master <- within(data = sipp08_master,{
  epppnum <- as.integer(epppnum)
})

length(unique(as.numeric(sipp08_master$ssuid)))

# sipp08_01 <- subset(sipp08_master, ssuid == "019133398883")
# sipp08_02 <- subset(sipp08_master, ssuid %in% ssuids[1:20])
# View(sipp08_01)

########################################################################
## 2008_disability.dta 
########################################################################
Filepath2 <- paste(RDataPath, '2008_disability.RData', sep = '')
load(Filepath2)
ssuids <- unique(as.vector(disability_2008$ssuid))
length(unique(as.numeric(disability_2008$ssuid)))

Colnames_Keep_disab <- c('ssuid', 'shhadid', 'epppnum', 'disb_wrk_ageR2', 'gas', 'rent', 'meet_expenses', 
                         'phone', 'medical')

########################################################################
## Get disability information, with only 1 type of disability entry 
## per ssuid, in the disability_2008 data
########################################################################
Disab <- fn_returnDisb_ssuid(disbData = disability_2008)
names(Disab)
disability_2008_1 <- unique(Disab[['disability_2008_1']][,Colnames_Keep_disab])
ssuid_disb_1 <- Disab[['ssuid_disb_1']]
length(unique(as.numeric(ssuid_disb_1)))
#Disab[['ssuid_disb_2_ORmore']]

########################################################################
## Merge sipp_2008 & 2008_disability.dta 
########################################################################
sipp08_master_disab <- merge(x = sipp08_master, y = disability_2008_1, 
                             by = c('ssuid', 'shhadid', 'epppnum'), all.x = T)
sipp08_master_disab$yearmon <- as.yearmon(paste(sipp08_master_disab$rhcalmn, sipp08_master_disab$rhcalyr))
sipp08_master_disab <- sipp08_master_disab[order(sipp08_master_disab$ssuid, sipp08_master_disab$yearmon),]

# Colnames_Keep_merged <- c('ssuid', 'shhadid', 'yearmon', 'ehrefper', 'rhtype', 'whfnwgt', 'thtotinc', 'rhpov', 'epppnum',
#                           'disb_wrk_ageR2')
# sipp08_master_disab <- sipp08_master_disab[, Colnames_Keep_merged]

# disab_01 <- subset(disability_2008, ssuid == "730925701502")[,Colnames_Keep_disab]
#sipp08_master_disab <- subset(sipp08_master_disab, ssuid %in% ssuid_disb_1[1:500])
sipp08_master_disab <- na.omit(sipp08_master_disab)

#View(sipp08_master_disab[,Colnames_Keep_merged])
#str(sipp08_master_disab)

########################################################################
## Get Income Poverty by Gender
########################################################################
## Temp <- aggregate(cbind(thtotinc, rhpov, disb_wrk_ageR2) ~ ssuid + shhadid + yearmon + esex, 
##                   data = sipp08_master_disab, FUN = mean)
## Data_forIncPov_byGender <- fn_DataforIncPov(Data = Temp)
## Filename <- paste0(RDataPath, 'Data_forIncPov_byGender.RData')
## save(Data_forIncPov_byGender, file = Filename)
## rm(Temp, Data_forIncPov_byGender)
## gc()

########################################################################
## Get Income Poverty by Race
########################################################################
sipp08_master_disab$race <- mapvalues(sipp08_master_disab$erace,
                                      from = c("White alone", "Black alone", "Asian alone", "Residual"),
                                      to = c('White', 'Black', 'Others', 'Others')
                                      )
## Temp <- aggregate(cbind(thtotinc, rhpov, disb_wrk_ageR2) ~ ssuid + shhadid + yearmon + race, 
##                   data = sipp08_master_disab, FUN = mean)
## Data_forIncPov_byRace <- fn_DataforIncPov(Data = Temp)
## Filename <- paste0(RDataPath, 'Data_forIncPov_byRace.RData')
## save(Data_forIncPov_byRace, file = Filename)
## rm(Temp, Data_forIncPov_byRace)
## gc()

########################################################################
## Get Income Poverty by Gender & Marital status of head of household
########################################################################
sipp08_master_disab$ms <- mapvalues(sipp08_master_disab$ems,
                                    from = c("Married, spouse present", "Married, spouse absent",
                                      "Widowed", "Divorced", "Separated",
                                      "Never Married"),
                                    to = c("Married", "Married", rep("Not married", 4))
                                    )
sipp08_master_disab$gender_ms <- with(sipp08_master_disab,
                                      interaction(esex, ms))

## Temp <- aggregate(cbind(thtotinc, rhpov, disb_wrk_ageR2) ~ ssuid + shhadid + yearmon + gender_ms, 
##                   data = sipp08_master_disab, FUN = mean)
## Data_forIncPov_byGenderMS <- fn_DataforIncPov(Data = Temp)
## Filename <- paste0(RDataPath, 'Data_forIncPov_byGenderMS.RData')
## save(Data_forIncPov_byGenderMS, file = Filename)
## rm(Temp, Data_forIncPov_byGenderMS)
## gc()

########################################################################
## Get year quarter instead of year month
########################################################################
sipp08_master_disab <- subset(sipp08_master_disab, yearmon != 'May 2008')
sipp08_master_disab$yearqtr <- as.yearqtr(sipp08_master_disab$yearmon)

########################################################################
## Get Income Poverty by Race, Gender & Marital status of head of household
########################################################################
## Temp <- aggregate(cbind(thtotinc, rhpov, disb_wrk_ageR2) ~ ssuid + shhadid + yearmon + gender_ms + race, 
##                   data = sipp08_master_disab, FUN = mean)
Temp <- aggregate(cbind(thtotinc, rhpov, disb_wrk_ageR2) ~ ssuid + shhadid + yearqtr + gender_ms + race, 
                  data = sipp08_master_disab, FUN = mean)

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

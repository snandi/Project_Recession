rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script reads in sipp08_master & 2008_disability and merges them
## and conducts basic summary statistics
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
Disab[['ssuid_disb_2_ORmore']]

########################################################################
## Merge sipp_2008 & 2008_disability.dta 
########################################################################
sipp08_master_disab <- merge(x = sipp08_master, y = disability_2008_1, 
                             by = c('ssuid', 'shhadid', 'epppnum'), all.x = T)
sipp08_master_disab$yearmon <- as.yearmon(paste(sipp08_master_disab$rhcalmn, sipp08_master_disab$rhcalyr))
sipp08_master_disab <- sipp08_master_disab[order(sipp08_master_disab$ssuid, sipp08_master_disab$yearmon),]

Colnames_Keep_merged <- c('ssuid', 'shhadid', 'yearmon', 'ehrefper', 'rhtype', 'whfnwgt', 'thtotinc', 'rhpov', 'epppnum',
                          'disb_wrk_ageR2')
sipp08_master_disab <- sipp08_master_disab[, Colnames_Keep_merged]

# disab_01 <- subset(disability_2008, ssuid == "730925701502")[,Colnames_Keep_disab]
#sipp08_master_disab <- subset(sipp08_master_disab, ssuid %in% ssuid_disb_1[1:500])
sipp08_master_disab <- na.omit(sipp08_master_disab)

#View(sipp08_master_disab[,Colnames_Keep_merged])
str(sipp08_master_disab)

########################################################################
## Get Income Poverty information
########################################################################
Data_forIncPov <- aggregate(cbind(thtotinc, rhpov, disb_wrk_ageR2) ~ ssuid + shhadid + yearmon, 
                            data = sipp08_master_disab, FUN = mean)
Data_forIncPov <- Data_forIncPov[order(Data_forIncPov$ssuid, Data_forIncPov$yearmon), ]
Data_forIncPov$rhpov2 <- 2 * Data_forIncPov$rhpov

Data_forIncPov$FPL200 <- Data_forIncPov$thtotinc < Data_forIncPov$rhpov2
Data_forIncPov$FPL100 <- Data_forIncPov$thtotinc < Data_forIncPov$rhpov
Data_forIncPov$FPL200[Data_forIncPov$FPL100 == TRUE] <- FALSE

Data_forIncPov$Pct_rhpov <- Data_forIncPov$thtotinc/Data_forIncPov$rhpov
Data_forIncPov$disb_wrk_ageR2 <- factor(Data_forIncPov$disb_wrk_ageR2, labels = c('no', 'yes'))
str(Data_forIncPov)
rownames(Data_forIncPov) <- NULL
head(Data_forIncPov)
comment(Data_forIncPov) <- 'The lower the value of Pct_rhpov, the worse off the household is'
str(Data_forIncPov)
#qplot() + geom_boxplot(aes(x = as.factor(as.Date(yearmon)), y = Pct_rhpov), data = Data_forIncPov )
########################################################################
## Boxplot of ratio of thtotinc and rhpov, by yearmon
########################################################################
Plot1_box <- qplot() + geom_boxplot(aes(x = as.factor(as.Date(yearmon)), y = Pct_rhpov, fill = disb_wrk_ageR2, 
                                        col = disb_wrk_ageR2), 
                       data = Data_forIncPov , outlier.colour = 'gray30', outlier.size = 0.3) + 
  ylab(label = 'thtotinc / rhpov') + xlab('Year month') +
  theme(
    legend.position = 'top',
    axis.text.x = element_text(angle=90, vjust=1)
  )
#Plot1_box

########################################################################
## Boxplot of ratio of thtotinc and 2*rhpov, by yearmon
########################################################################
Data_forIncPov$Pct_rhpov2 <- Data_forIncPov$thtotinc/Data_forIncPov$rhpov2
Plot2_box <- qplot() + geom_boxplot(aes(x = as.factor(as.Date(yearmon)), y = Pct_rhpov2, fill = disb_wrk_ageR2, 
                                        col = disb_wrk_ageR2), 
                                    data = Data_forIncPov , outlier.colour = 'gray30', outlier.size = 0.3) + 
  ylab(label = 'thtotinc / (2*rhpov)') + xlab('Year month') +
  theme(
    legend.position = 'top',
    axis.text.x = element_text(angle=90, vjust=1)
  )
#Plot2_box
# qplot() + geom_line(aes(x = as.Date(yearmon), y = Pct_rhpov, col = ssuid), data = Data_forIncPov )

Filename.plot <- paste0(PlotPath, 'IncomePovertyPlots_', Today, '.pdf')
pdf(file = Filename.plot, onefile = TRUE)
print(Plot1_box)
print(Plot2_box)
dev.off()

Filename <- paste0(RDataPath, 'Data_forIncPov.RData')
save(Data_forIncPov, file = Filename)




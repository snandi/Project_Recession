rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script analyzes the income poverty over time. The data was 
## created by RScript2.
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
Filename <- paste0(RDataPath, 'Data_forIncPov.RData')
load(file = Filename)

Data_forIncPov <- subset(Data_forIncPov, yearmon !=  'May 2008')

#View(Data_forIncPov[1:50,])
Data_Sub <- subset(Data_forIncPov, yearmon ==  'Jun 2008')

Fun <- function(Vec){unlist(as.data.frame(cbind(N_Total = length(Vec), N_FPL100 = sum(Vec))))}
output <- func(Data_Sub$FPL100)
output

Total <- aggregate(FPL100 ~ yearmon + disb_wrk_ageR2, data = Data_forIncPov,
                   FUN = length)
colnames(Total) <- c('yearmon', 'disab', 'TotalHH')
FPLnum <- aggregate(cbind(FPL100, FPL200) ~ yearmon + disb_wrk_ageR2, data = Data_forIncPov,
                    FUN = sum)
colnames(FPLnum) <- c('yearmon', 'disab', 'FPL100_num', 'FPL200_num')

IncPovPct <- merge(Total, FPLnum)
IncPovPct$FPL100_pct <- IncPovPct$FPL100_num/IncPovPct$TotalHH
IncPovPct$FPL200_pct <- IncPovPct$FPL200_num/IncPovPct$TotalHH
IncPovPct <- IncPovPct[order(IncPovPct$yearmon),]
IncPovPct$FPL200plusFPL100 <- IncPovPct$FPL200_pct+ IncPovPct$FPL100_pct
Ylim <- range(IncPovPct$FPL100_pct, IncPovPct$FPL200_pct)

Plot100 <- qplot() + 
  geom_line(aes(x = as.numeric(yearmon), y = FPL100_pct, col = disab), data = IncPovPct, size = 2) +
  ylim(Ylim) + ggtitle(label = 'Below 100% FPL') + ylab(label = 'Percent') + xlab(label = 'Date') +
  theme(legend.position = 'top')
Plot200 <- qplot() + 
  geom_line(aes(x = as.numeric(yearmon), y = FPL200_pct, col = disab), data = IncPovPct, size = 2) +
  ylim(Ylim) + ggtitle(label = 'Below 200% FPL') + ylab(label = 'Percent') + xlab(label = 'Date') +
  theme(legend.position = 'top')
Plot200plus100 <- qplot() + 
  geom_line(aes(x = as.numeric(yearmon), y = FPL200plusFPL100, col = disab), data = IncPovPct, size = 2) +
  ggtitle(label = 'Below 200% FPL & Below 100%') + ylab(label = 'Percent') + xlab(label = 'Date') +
  theme(legend.position = 'top')

## Filename.csv <- paste0(RDataPath, 'IncPovPct.csv')
## write.csv(IncPovPct, file = Filename.csv, row.names = F)

################ Plotting columns of poverty pcts ################
IncPovPct$AboveFPL <- 1 - rowSums(IncPovPct[,c('FPL100_pct', 'FPL200_pct')])

IncPovPct_Long <- melt(data=IncPovPct[,c('yearmon', 'disab', "FPL100_pct", "FPL200_pct", 
                                         'AboveFPL')], 
                       id = c('yearmon', 'disab'))

Barplot <- qplot() +
  geom_bar(aes(y = value, x = as.numeric(yearmon), fill = variable), 
                              data = IncPovPct_Long, stat = 'identity') + 
  facet_wrap(~disab) +
  theme(legend.position = 'top')

  
Filename.plot <- paste0(PlotPath, 'IncomePovertyPct_', Today, '.pdf')
pdf(file = Filename.plot, onefile = TRUE)
Plot100
Plot200
Plot200plus100
#grid.arrange(Plot100, Plot200, ncol = 2)
Barplot
dev.off()


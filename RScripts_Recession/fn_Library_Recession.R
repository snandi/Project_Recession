source('~/RScripts/fn_Library_SN.R')

################################################################## 
## Separate safety net program analysis
################################################################## 
fn_separateSafetyNet <- function(
  Data, 
  ProgramVar = 'thsocsec',
  Maintitle = 'Total Household Social Security Income, per household',
  ylabel = 'thsocsec per household'
){
  Program_yrmon <- aggregate(Data[,ProgramVar] ~ yearmon + gender_ms + erace + adult_disb, data = Data, FUN = sum)
  names(Program_yrmon)[5] <- 'Program'
  HH_yrmon <- aggregate(ssuid ~ yearmon + gender_ms + erace + adult_disb, data = Data, FUN = length)
  Program_yrmon <- merge(
    x = Program_yrmon,
    y = HH_yrmon,
    by = c('yearmon', 'erace', 'gender_ms', 'adult_disb')
  )
  Program_yrmon$Program_perhh <- Program_yrmon$Program/Program_yrmon$ssuid
  
  Plot <- qplot() + geom_line(aes(x = as.Date(yearmon), y = Program_perhh, group = adult_disb, col = adult_disb), 
  data = Program_yrmon, size = 1) +
  ggtitle(label = Maintitle) +
  xlab(label = '') + ylab(label = ylabel) +
#   facet_grid(race ~ gender_ms, scales = 'free_y') +
  facet_grid(erace ~ gender_ms) +
  theme(legend.position = 'top')

  return(list(Program_yrmon = Program_yrmon, Plot = Plot))
}

################################################################## 
## Returns disability information, from disability_2008 dataset
##################################################################
fn_returnDisb_ssuid <- function(disbData = disability_2008){
  
  disb_only <- unique(disbData[,c('ssuid', 'disb_wrk_ageR2')])
  disb_only$ssuid <- as.factor(disb_only$ssuid)
  disb_only_table <- table(disb_only$ssuid)
  ssuid_disb_1 <- names(disb_only_table[disb_only_table == 1])
  ## ssuid_disb_1 is a list of ssuids that have only ONE disability status, per ssuid
  ssuid_disb_2_ORmore <- disb_only_table[disb_only_table > 1] 
  #length(disb_only_table[disb_only_table > 1])
  
  return(list(disb_only_table = disb_only_table, 
              disability_2008_1 = subset(disability_2008, ssuid %in% ssuid_disb_1), 
              ssuid_disb_1 = ssuid_disb_1, 
              ssuid_disb_2_ORmore = ssuid_disb_2_ORmore
  ))
}

################################################################## 
## Prepares data for income poverty with FPL100 and FPL200
##################################################################
normalize <- function(Data, Colname){
  Mean <- mean(Data[,Colname])
  SD <- sd(Data[,Colname])
  Normalized <- (Data[,Colname] - Mean)/SD
  #Normalized <- (Data[,Colname] - Mean)
  Data$New <- Normalized
  names(Data)[names(Data) == 'New'] <- paste(Colname, 'Norm', sep='_')
  return(Data)
}
fn_lagData <- function(Data, Colname){
  BeforeLag <- Data[,Colname]
  N <- length(BeforeLag)
  AfterLag <- c(0, BeforeLag[-N])
  return(AfterLag)
}

normalize_baseline <- function(Data, Colname){
  Baseline <- Data[1,Colname]
  Normalized <- Data[,Colname] - Baseline
  return(Normalized)
}

fn_DataforIncPov <- function(Data){
  Data <- Data[order(Data$ssuid, Data$yearmon), ]
  Data$rhpov2 <- 2 * Data$rhpov
  
  Data$FPL200 <- Data$thtotinc < Data$rhpov2
  Data$FPL100 <- Data$thtotinc < Data$rhpov
  Data$FPL200[Data$FPL100 == TRUE] <- FALSE
  
  Data$Pct_rhpov <- Data$thtotinc/Data$rhpov
  Data$disb_wrk_ageR2 <- factor(Data$disb_wrk_ageR2, labels = c('no', 'yes'))
  
  rownames(Data) <- NULL
  Data <- subset(Data, yearmon != 'May 2008')
  Data$FPL100_num <- Data$thtotinc/Data$rhpov
  Data$FPL200_num <- Data$thtotinc/Data$rhpov2
  
  ## Normalize FPL100
  SplitByssuid <- split(x = Data, f = as.factor(Data$ssuid))
  Data <- do.call(what = rbind, args = lapply(X = SplitByssuid, FUN = normalize, Colname = 'FPL100_num'))
  rownames(Data) <- NULL
  try(rm(SplitByssuid))
  gc()
  
  ## Normalize FPL200
  SplitByssuid <- split(x = Data, f = as.factor(Data$ssuid))
  Data <- do.call(what = rbind, args = lapply(X = SplitByssuid, FUN = normalize, Colname = 'FPL200_num'))
  rownames(Data) <- NULL
  try(rm(SplitByssuid))
  gc()
  
  ## Lag FPL100
  SplitByssuid <- split(x = Data, f = as.factor(Data$ssuid))
  Data <- do.call(what = rbind, args = lapply(X = SplitByssuid, FUN = fn_lagData, Colname = 'FPL100_num'))
  rownames(Data) <- NULL
  try(rm(SplitByssuid))
  gc()
  
  ## Lag FPL200
  SplitByssuid <- split(x = Data, f = as.factor(Data$ssuid))
  Data <- do.call(what = rbind, args = lapply(X = SplitByssuid, FUN = fn_lagData, Colname = 'FPL200_num'))
  rownames(Data) <- NULL
  try(rm(SplitByssuid))
  gc()
  
  ## Lag FPL100_Norm
  SplitByssuid <- split(x = Data, f = as.factor(Data$ssuid))
  Data <- do.call(what = rbind, args = lapply(X = SplitByssuid, FUN = fn_lagData, Colname = 'FPL100_num_Norm'))
  rownames(Data) <- NULL
  try(rm(SplitByssuid))
  gc()
  
  ## Lag FPL200_Norm
  SplitByssuid <- split(x = Data, f = as.factor(Data$ssuid))
  Data <- do.call(what = rbind, args = lapply(X = SplitByssuid, FUN = fn_lagData, Colname = 'FPL200_num_Norm'))
  rownames(Data) <- NULL
  try(rm(SplitByssuid))
  gc()
  
  comment(Data) <- 'The lower the value of Pct_rhpov, the worse off the household is'
  return(Data)
}

################################################################## 
## Prepares data for income poverty with FPL100 and FPL200, after
## meeting with Yajuan on 11/18. This normalizes based on the 
## baseline value
## Also, this one uses yearqtr instead of yearmon
##################################################################
fn_DataforIncPov_v2 <- function(Data){
  Data <- Data[order(Data$ssuid, Data$yearqtr), ]
  Data$rhpov2 <- 2 * Data$rhpov
  
  Data$FPL200 <- Data$thtotinc < Data$rhpov2
  Data$FPL100 <- Data$thtotinc < Data$rhpov
  Data$FPL200[Data$FPL100 == TRUE] <- FALSE
  
  Data$Pct_rhpov <- Data$thtotinc/Data$rhpov
  ## Data$disb_wrk_ageR2 <- factor(Data$disb_wrk_ageR2, labels = c('no', 'yes'))
  Data$adult_disb <- factor(Data$adult_disb, labels = c('no', 'yes'))
  
  rownames(Data) <- NULL
  #Data <- subset(Data, yearmon != 'May 2008')
  Data$FPL100_num <- Data$thtotinc/Data$rhpov
  Data$FPL200_num <- Data$thtotinc/Data$rhpov2
  
  ## Normalize FPL100
  SplitByssuid <- split(x = Data, f = as.factor(Data$ssuid))
  Data$FPL100_noBaseline <- do.call(what = c, args = lapply(X = SplitByssuid, FUN = normalize_baseline, Colname = 'FPL100_num'))
  rownames(Data) <- NULL
  
  ## Normalize FPL200
  Data$FPL200_noBaseline <- do.call(what = c, args = lapply(X = SplitByssuid, FUN = normalize_baseline, Colname = 'FPL200_num'))
  
  ## Lag FPL100
  Data$FPL100_num_Lag <- do.call(what = c, args = lapply(X = SplitByssuid, FUN = fn_lagData, Colname = 'FPL100_num'))
  
  ## Lag FPL200
  Data$FPL200_num_Lag <- do.call(what = c, args = lapply(X = SplitByssuid, FUN = fn_lagData, Colname = 'FPL200_num'))
  
  try(rm(SplitByssuid))
  gc()
  
  ## Lag FPL100_Norm
  SplitByssuid <- split(x = Data, f = as.factor(Data$ssuid))
  Data$FPL100_norm_Lag <- do.call(what = c, args = lapply(X = SplitByssuid, FUN = fn_lagData, Colname = 'FPL100_noBaseline'))
  rownames(Data) <- NULL
  
  ## Lag FPL200_Norm
  Data$FPL200_norm_Lag <- do.call(what = c, args = lapply(X = SplitByssuid, FUN = fn_lagData, Colname = 'FPL200_noBaseline'))
  
  try(rm(SplitByssuid))
  gc()
  
  comment(Data) <- 'The lower the value of Pct_rhpov, the worse off the household is'
  return(Data)
}

################################################################## 
## Return diagnostic plots of lm models in ggplot
##################################################################
require(ggplot2)
diagPlot<-function(model){
  p1<-ggplot(model, aes(.fitted, .resid))+geom_point()
  p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
  p1<-p1+xlab("Fitted values")+ylab("Residuals")
  p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()
  
  p2<-ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
  p2<-p2+geom_abline(aes(qqline(.stdresid)))+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
  p2<-p2+ggtitle("Normal Q-Q")+theme_bw()
  
  p3<-ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)
  p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
  p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
  p3<-p3+ggtitle("Scale-Location")+theme_bw()
  
  p4<-ggplot(model, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")
  p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
  p4<-p4+ggtitle("Cook's distance")+theme_bw()
  
  p5<-ggplot(model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
  p5<-p5+stat_smooth(method="loess", na.rm=TRUE)
  p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
  p5<-p5+ggtitle("Residual vs Leverage Plot")
  p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
  p5<-p5+theme_bw()+theme(legend.position="bottom")
  
  p6<-ggplot(model, aes(.hat, .cooksd))+geom_point(na.rm=TRUE)+stat_smooth(method="loess", na.rm=TRUE)
  p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance")
  p6<-p6+ggtitle("Cook's dist vs Leverage hii/(1-hii)")
  p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
  p6<-p6+theme_bw()
  
  return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3, cdPlot=p4, rvlevPlot=p5, cvlPlot=p6))
}

################################################################## 
## Keep the ssuids that are in wave 15 and trace them back
## Keep ssuids with hh_heads > 18 yrs old
## Keep the ssuids where the ehrefper hasn't changed
##################################################################
fn_keepWave15ehref <- function(Subset){
  Subset <- Subset[order(Subset$swave, Subset$ehrefper),]
  ehrefper_last <- last(Subset$ehrefper)
  ehrefper_first <- Subset$ehrefper[ 1 ]
  Subset$epppnum <- as.numeric(Subset$epppnum)
  Subset <- Subset[Subset$ehrefper == Subset$epppnum, ]
  Subset <- subset(Subset, ehrefper == ehrefper_last)
  
  Return <- TRUE
  
  ## Check if last wave is wave 15
  swave_last <- last(Subset$swave)
  if(swave_last < 15){
    Return <- FALSE
  }
  ## Check if age of ref per is < 18
  Age <- min(as.numeric(Subset[, 'tage']))
  if(Age < 18){
    Return <- FALSE
  }
  ## Check if the ehrefper has remained the same
  if(ehrefper_first != ehrefper_last){
    Return <- FALSE
  }
  
  if(Return == TRUE){
    return(Subset)
  }
}



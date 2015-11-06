is.integer0 <- function(x)
{
  is.integer(x) && !length(x)
}

## Function library for Curve registration

fn_get_pValue <- function (lmobject) {
  if (class(lmobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(lmobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(round(p, 6))
}

################################################################## 
## Returns the SE of mean of each row of a dataset
##################################################################
rowSE <- function(Data){
  SE <- apply(X = Data, MARGIN = 1, FUN=function(Row){sd(Row)/sqrt(length(Row))})
  return(SE)
}
################################################################## 

################################################################## 
## Returns the SD of each row of a dataset
##################################################################
rowSD <- function(Data){
  SD <- apply(X = Data, MARGIN = 1, FUN=function(Row){sd(Row)})
  return(SD)
}

colSD <- function(Data){
  rowSD(t(Data))
}
################################################################## 

################################################################## 
## Returns the SD of each row of a dataset
##################################################################
rowVar <- function(Data){
  SD <- apply(X = Data, MARGIN = 1, FUN=function(Row){var(Row)})
  return(SD)
}

colVar <- function(Data){
  rowVar(t(Data))
}
################################################################## 

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

fn_DataforIncPov <- function(Data){
  Data <- Data[order(Data$ssuid, Data$yearmon), ]
  Data$rhpov2 <- 2 * Data$rhpov
  
  Data$FPL200 <- Data$thtotinc < Data$rhpov2
  Data$FPL100 <- Data$thtotinc < Data$rhpov
  Data$FPL200[Data$FPL100 == TRUE] <- FALSE
  
  Data$Pct_rhpov <- Data$thtotinc/Data$rhpov
  Data$disb_wrk_ageR2 <- factor(Data$disb_wrk_ageR2, labels = c('no', 'yes'))

  rownames(Data) <- NULL
  
  comment(Data) <- 'The lower the value of Pct_rhpov, the worse off the household is'
  return(Data)
}

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


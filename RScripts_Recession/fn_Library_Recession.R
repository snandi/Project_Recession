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

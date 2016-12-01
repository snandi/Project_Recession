#library(plyr), MASS, Hmisc, ggplot2
############################################################################
#function to identify the colors of bar according to significance of effects
############################################################################
calc.cols <- function(x, mCompLevels = c(0.001, 0.01, 0.05))
{
  if(x < mCompLevels[1]) 
    return("red") 
  if(x < mCompLevels[2]) 
    return("orange") 
  if(x < mCompLevels[3]) 
    return("yellow") 
  return("grey50")
}

calc.cols2 <- function(x, mCompLevels = c(0.001, 0.01, 0.05))
{
  if(x < mCompLevels[1]) 
    return(paste("p-value <", mCompLevels[1]))#return("red")# 
  if(x < mCompLevels[2]) 
    return(paste("p-value <", mCompLevels[2]))#return("orange")# 
  if(x < mCompLevels[3]) 
    return(paste("p-value <", mCompLevels[3]))#return("yellow")# 
  return("Not Sig")#return("grey")#
}

#get names for ploting barplots for the effects
getNamesForPlot <- function(names, ind)
{
  namesForPlot <- unlist(lapply(names, 
                                function(y) 
                                  substring2(y, 1, 
                                             substring.location(y, " ")$first[1]-1)))
  namesForLevels <- unlist(lapply(names,
                                  function(y)  
                                    substring2(y, 
                                               substring.location(y, " ")$first[1]+ind, nchar(y))))
  return(list(namesForPlot=namesForPlot, namesForLevels=namesForLevels))
}

## plots for LSMEANS or DIFF of LSMEANS
plotLSMeans <- function(table, response, Ylabel = 'response', 
                        which.plot=c("LSMEANS", "DIFF of LSMEANS"), 
                        main = NULL, cex = 1.4, effs = NULL, mult = TRUE)
{
  
  if(!is.null(effs)){
    rnames <- rownames(table)
    diffs.facs <- sapply(rnames, 
                         function(x) substring(x, 1, 
                                               substring.location(x, " ")$first[1]-1), 
                         USE.NAMES = FALSE)    
    find.fac <- diffs.facs %in% effs
    table <- table[find.fac,]
  }
  
  if(which.plot=="LSMEANS")
    names <- getNamesForPlot(rownames(table),2)
  else
    names <- getNamesForPlot(rownames(table),1)   
  
  
  
  namesForPlot <- names$namesForPlot
  namesForLevels <- names$namesForLevels
  un.names <- unique(namesForPlot)
  
  
  ### changed code to transfer to ggplot
  ttplot <- table
  ttplot$namesforplots <- namesForPlot
  ttplot$levels <- as.factor(namesForLevels)
  colnames(ttplot)[which(colnames(ttplot)=="p-value")] <- "pvalue"
  colnames(ttplot)[which(colnames(ttplot)=="Lower CI")] <- "lci"
  colnames(ttplot)[which(colnames(ttplot)=="Upper CI")] <- "uci"
  ttplot$col.bars <-  unlist(lapply(ttplot[,"pvalue"], FUN = calc.cols2, 
                                    mCompLevels = c(0.001, 0.01, 0.05)))
  ttplot <- ttplot[,c("levels", "Estimate", "col.bars", "lci", "uci", 
                      "namesforplots")]
  uci <- lci <- col.bars <- Estimate <- NULL
  ttplot <- ttplot[order(ttplot$Estimate, decreasing = T),]
  #print(ttplot)
  
  # if(mult)
  # ggplot(ttplot, aes(x=levels, y = Estimate, fill = col.bars)) + 
  ggplot(ttplot, aes(x = reorder(levels, Estimate), y = Estimate, fill = col.bars)) + 
    geom_bar(position = "dodge", stat = "identity") +  
    coord_flip() +
    # geom_errorbar(aes(ymin = lci, ymax = uci ), colour="black", width=.1) + 
    # geom_errorbar(aes(xmin = lci, xmax = uci ), colour="black", width=.1) + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.4), ## was angle 90 
          axis.title = element_text(size = rel(1)), ## was rel(1.4)
          axis.text = element_text(size = rel(1)), 
          legend.position = 'top',
          legend.text = element_text(size = rel(1)), 
          legend.title = element_text(size = rel(1)))  + 
    scale_fill_manual(values  = 
                        c(  "Not Sig" = "grey50", "p-value < 0.01" = "orange", 
                            "p-value < 0.05" = "yellow", 
                            "p-value < 0.001" = "red"), name="Significance")  +
    xlab(label = '') + ylab(label = Ylabel)
  #facet_wrap( ~ namesforplots, scales = "free")
  # else{
  #   for(i in 1:length(un.names)){
  #     names.plot <- un.names[i]
  #     subplot <- ttplot[ttplot$namesforplots == names.plot,]
  #     ggplot(subplot, aes(x=levels, y = Estimate, fill = col.bars)) + 
  #       geom_bar(position = "dodge", stat = "identity") +  
  #       geom_errorbar(aes(ymin = lci, ymax = uci ), colour="black", width=.1) + 
  #       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4), 
  #             axis.title.y = element_text(size = rel(1.4)), 
  #             axis.text = element_text(size = rel(1)), 
  #             legend.text = element_text(size = rel(1)), 
  #             legend.title = element_text(size = rel(1)))  + 
  #       scale_fill_manual(values  = 
  #                           c(  "NS" = "grey", "p-value < 0.01" = "orange", 
  #                               "p-value < 0.05" = "yellow", 
  #                               "p-value < 0.001" = "red"), name="Significance")  +
  #       ylab(response) + xlab(names.plot)
  #   }
# }
}

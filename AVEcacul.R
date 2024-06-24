library(psych)
library("zoo")
library(Hmisc)


library(GPArotation)
library(lavaan)
library(semPlot)
library(stringr)
library(rlist)
library(semTools)


#tModel <- " shouye =~ p1+p3+p4+p7+p8\n    
#zhanshi =~ p6+p14+p9+p13+p15\n      yinxiao =~ p5+p11+p12+p17+p18+p20+p22\n      shezhi =~ p16+p24"
#tModel <- 'shouye =~ p1+p3+p4+p7+p8\nzhanshi =~ p6+p14+p9+p13+p15\nyinxiao =~ p5+p11+p12+p17+p18+p20+p22\nshezhi =~ p16+p24'
# tModel2 <- 'shouye =~ p3+p4+p7+p8\n\nzhanshi =~ p6+p14+p9+p13+p15\n\nyinxiao =~ p5+p11+p12+p17+p18+p20+p22\n\nshezhi =~ p16+p24'

#df <- read.csv('D:/ACADEMIA/RArelated/shiny/app_SEMrun/test1.csv')
#df[is.na(df)] <- 0

AVEComp <- function(df, model){
  
      cfafit <- cfa(tModel,df, std.lv = T)
      aa =  reliability(cfafit)
      
      dd = as.data.frame(aa[5,])
      colnames(dd)[1] <-"AVE"
      is.num <- sapply(dd, is.numeric)
      dd[is.num] <- lapply(dd[is.num], round,3)
      
      eee = cov2cor(lavInspect(cfafit, what = "est")$psi)
      
      fff = as.data.frame(eee)
      
      
      for(i in 1: nrow(fff)){
        for(j in 1: nrow(fff)){
          if(j < i){
            fff[[i]][j] = NA
          }
          if(j == i){
            fff[[i]][i] = dd[i,]
          }
        }
      }
      return(fff)
}



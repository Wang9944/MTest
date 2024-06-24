library(psych)
library("zoo")
library(Hmisc)


library(GPArotation)
library(lavaan)
library(semPlot)
library(stringr)
library(rlist)
library(semTools)

# 用来判断各个indices，是否需要查看MI来修改模型的
checkIndices <- function(indiceInput){
  # chisquare p value, df, chisq/df, cfi,tli, ifi, rmsea,srmr
  
  # 还是新建matrix比较好，rowName:name, colname:value, judge
  
  indiceList <- list(round(indiceInput[3]/indiceInput[4],2),
                     round(indiceInput[9],2),round(indiceInput[10],2),
                     round(indiceInput[15],2),round(indiceInput[23],2),
                     round(indiceInput[29],2))
  
  
  
  rowName = c('chisq/df','cfi','tli','ifi',
              'rmsea','srmr')
  colName = 'value'
  indiceMatrix = matrix(data = indiceList, nrow = 6, ncol = 1, byrow = TRUE,
                        dimnames = list(rowName, colName))
  
  size = 800
  sizeMax = 500
  chCutoff = vector('list',6)
  judge = 0 
  
  
  # p value >0.05 但是在N很大的情况下就无所谓
  # 卡方自由度比 <=3
  # cfi tli >= 0.095(0.09) ifi >= 0.09
  # rmsea  <= 0.06 srmr<=0.08
  # rule: 1. df<=0, 直接restart; 后面6个指标，必须都符合;
  # p value看size
  
  

  if(indiceList[1] <= 5 ){
    chCutoff[1] <- TRUE
  }else{
    chCutoff[1] <- FALSE
  }
  
  if(indiceList[2] >= 0.9 ){
    chCutoff[2] <- TRUE
  }else{
    chCutoff[2] <- FALSE
  }
  
  if(indiceList[3] >= 0.9 ){
    chCutoff[3] <- TRUE
  }else{
    chCutoff[3] <- FALSE
  }
  
  if(indiceList[4] >= 0.9 ){
    chCutoff[4] <- TRUE
  }else{
    chCutoff[4] <- FALSE
  }
  
  if(indiceList[5] <= 0.1 ){
    chCutoff[5] <- TRUE
  }else{
    chCutoff[5] <- FALSE
  }
  
  if(indiceList[6] <= 0.1 ){
    chCutoff[6] <- TRUE
  }else{
    chCutoff[6] <- FALSE
  }
  
  indiceMatrix = cbind(indiceMatrix,chCutoff)
  indiceMatrix <- as.data.frame(indiceMatrix) 
  #print(list(indiceMatrix))
  
  for( i in 1:6){
    if(indiceMatrix[i,2] == FALSE){
      #print('need to refine your model')
      #judge = 1
      indiceMatrix[i,2] <- 'bad'
    }else{
      #print('Cong, a very decent model')
      #judge = 2
      indiceMatrix[i,2] <- 'good'
    }
  }
  #print(class(indiceMatrix))
  print(indiceMatrix)
  colnames(indiceMatrix)[2] <-"judge"
  return(indiceMatrix)
  
}


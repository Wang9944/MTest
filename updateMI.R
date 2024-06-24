
library('devtools')
library(stringr)


updateModel2 <- function(fit, milist, model){

  flCutoff = 0.5
  miCutoff = 30
  inimodel = model
  
  
  flTable = parameterEstimates(fit, standardized=TRUE) %>%
    filter(op == "=~") %>% arrange(std.all)%>%
    select('Latent Factor'=lhs,Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta =std.all)
  
  
  
  if (flTable$Beta[1] < flCutoff){ 
    
    outcome = deleteItem2(model = model, value = flTable$Indicator[1])
    return(list(a = outcome, d= flTable$Indicator[1]))
    
    }else{
      
      if (milist[1,4]< miCutoff){
        print('MI looks pretty fine, no further modification needed')
        return('a')
      }else{
        lfvalue = milist[1,1]
        mdvalue = milist[1,2]
        rtvalue = milist[1,3]
        mivalue = milist[1,4]}
      
      # 加入他们
      mdStrPlus = str_c(paste(lfvalue, mdvalue, rtvalue))
      modelUpdate1 = paste(model, mdStrPlus, sep = '\n')
      
      # 删去最糟糕的一个
      chValue = NULL
      for(i in 1:length(flTable$Indicator)){
        if (flTable$Indicator[i] == lfvalue ){
          value1 = flTable$Beta[i]}
        
        if (flTable$Indicator[i] == rtvalue){
          value2 = flTable$Beta[i]}
      }
      if(value1 > value2){
        chValue = rtvalue
      }else{
        chValue = lfvalue
      }

      modelUpdate2 = deleteItem2(model = model, value = chValue)

      return(list(a = modelUpdate2, d = chValue))
  }

}


deleteItem2 <- function(model, value){
  aaa = str_locate(model,paste(value,'\\+',sep = ''))
  if(is.na(aaa[[1]]))
    {
    #print('let me start the testing')
    locList = str_locate(model,value)
    lfString1 = str_sub(model,1,locList[[1]]-2)
    rtString1 = str_sub(model,locList[[2]]+1)
  }else{
    #print('MCF is testing')
    locList = str_locate(model,value)
    lfString1 = str_sub(model,1,locList[[1]]-1)
    rtString1 = str_sub(model,locList[[2]]+2)
  }
  
  modelNew = str_c(lfString1, rtString1)
  return(modelNew)
  
}


######### 只删除，不增加
updateModel3 <- function(fit, milist, model){
  
  flCutoff = 0.5
  miCutoff = 30
  inimodel = model
  
  
  flTable = parameterEstimates(fit, standardized=TRUE) %>%
    filter(op == "=~") %>% arrange(std.all)%>%
    select('Latent Factor'=lhs,Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta =std.all)
  

  
  
  if (flTable$Beta[1] < flCutoff){ 
    
    outcome = deleteItem2(model = model, value = flTable$Indicator[1])
    return(outcome)
    
  }else{
    
    if (milist[1,4]< miCutoff){
      print('MI looks pretty fine, no further modification needed')
      return('a')
    }else{
      lfvalue = milist[1,1]
      mdvalue = milist[1,2]
      rtvalue = milist[1,3]
      mivalue = milist[1,4]}
    
    # 加入他们
    mdStrPlus = str_c(paste(lfvalue, mdvalue, rtvalue))
    modelUpdate1 = paste(model, mdStrPlus, sep = '\n')
    
    # 删去最糟糕的一个
    chValue = NULL
    for(i in 1:length(flTable$Indicator)){
      if (flTable$Indicator[i] == lfvalue ){
        value1 = flTable$Beta[i]}
      
      if (flTable$Indicator[i] == rtvalue){
        value2 = flTable$Beta[i]}
    }
    if(value1 > value2){
      chValue = rtvalue
    }else{
      chValue = lfvalue
    }
    
    modelUpdate2 = deleteItem2(model = model, value = chValue)
    
    return(modelUpdate2)
  }
  
}

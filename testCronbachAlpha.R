library(psych)
library("zoo")
library(Hmisc)


library(GPArotation)
library(lavaan)
library(semPlot)
library(stringr)
library(rlist)
library(semTools)


alphaVector <- vector('integer')

getAlpha <- function(df, model){
   
   
   
   alphaVector <- vector('integer')
   listHT <- vector('integer')
   rownameValue = NULL
   #rownameValue <- vector('integer')
   model = str_replace_all(model, " ", "")
   
   loLi1 = str_locate_all(model,'\\=~')
   loLi2 = str_locate_all(model,'\\\n')
   loLi3 = str_locate_all(model,'\\+')
   
   
   a = NULL
   b = NULL
   
   for(i in 1:(length( loLi2[[1]])/2 - 1)){
      listHT[1]<- loLi2[[1]][1]
      a <-  loLi2[[1]][i]
      b <-  loLi2[[1]][i+1]
      if((b-a) < 2){
         next
      }else{
         listHT[length(listHT)+1] <- b
      }
   }
   
   if(str_sub(model, str_length(model), str_length(model)) != '\n'){
      model = paste(model,'\n', sep='')
      listHT[length(listHT)+ 1] = str_length(model)
   }
   
   
   #print(listHT)
   
   
   
   for(i in 1: length(listHT)){
      
      # 每一个construct生成的对应alpha列表， numberA 就是对应construct的alpha
      s1 = str_sub(model, loLi1[[1]][i,2]+1, listHT[i]-1)
      s1 =  str_trim(s1, side ='both')
      s1 = str_split(s1, "\\+")
      s1 =  unlist(s1, use.names=FALSE)
      df1 = df[,s1]
      aa = psych::alpha(df1)

      
      numberA = aa$total['std.alpha']
      numberA = round(numberA, 2)
      rowNumber = 0
      cutoffValue = 0.04 #这个选择emmm
      
      #print(numberA)
      alphaVector[length(alphaVector)+1] <- round(numberA,3)
      
      
      # 对于要删掉某个就可以提高alpha的列表
      for(j in 1:length(aa$alpha.drop['std.alpha'][[1]])){
         #print('whattttt?')
         #print(aa$alpha.drop['std.alpha'][[1]][j] -  numberA)
         if((aa$alpha.drop['std.alpha'][[1]][j] -  numberA > cutoffValue) &&
            (aa$alpha.drop['std.alpha'][[1]][j]>rowNumber)){
           
            #print(rownames(aa$alpha.drop['std.alpha'])[j])
            rownameValue = rownames(aa$alpha.drop['std.alpha'])[j]
            #print(rownameValue)
            rowNumber = aa$alpha.drop['std.alpha'][[1]][j]
            
         }}

          
      if(is.null(rownameValue)){
         model2 = 'b'
         print('crown is memmmmeee')
         print(model2)
         }else{
         model2 = deleteItem3(model = model, value = rownameValue)
         }
    
       
   }
   
   
   return(list(a = alphaVector, b = model2, c = rownameValue))
   
   }



deleteItem3 <- function(model, value){
   aaa = str_locate(model,paste(value,'\\+',sep = ''))
   if(is.na(aaa[[1]]))
   {
      locList = str_locate(model,value)
      lfString1 = str_sub(model,1,locList[[1]]-2)
      rtString1 = str_sub(model,locList[[2]]+1)
   }else{
      locList = str_locate(model,value)
      lfString1 = str_sub(model,1,locList[[1]]-1)
      rtString1 = str_sub(model,locList[[2]]+2)
   }
   modelNew = str_c(lfString1, rtString1)
   return(modelNew)
   
 }


# 最后return的a是vector，b是模型，c是一个value
#ddd = getAlpha(df = df, model = tModel3)

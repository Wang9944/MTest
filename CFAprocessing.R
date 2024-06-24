library(psych)
library("zoo")
library(Hmisc)


library(GPArotation)
library(lavaan)
library(semPlot)
library(stringr)
library(rlist)

getCFA <- function(a, b){
  #df2 = df[, -c(15,16,17,18,19,20)]
  options(digits=2)
  df2 = b[,-1]
  ModelFit = cfa(a,data = df2, std.lv = T)
  
  if(is.null(ModelFit)){
    print('this is the end of model renewal')
  }else{
    factorLoading = parameterEstimates(ModelFit, standardized=TRUE) %>%
      filter(op == "=~") %>% select('Latent Variable'=lhs,
                                    'observed variable'=rhs,'factor loading'=std.all)
    
    
    
    factorLoading = mutate(factorLoading, across(where(is.numeric), round, 2))
    
    fitData = fitMeasures(ModelFit)
    dataframeMI = modificationindices(ModelFit,sort. = T)
    #dataframeMI= dataframeMI[dataframeMI$op == "=~",]
    dataframeMI= dataframeMI[dataframeMI$op == "~~",]
    return(list(a = fitData, b= dataframeMI, c=df2, d = ModelFit, e = factorLoading))
    
  }
  
}


duReturn <- function(a, b) {
  output <- list(a = a, b = b)
  return(output)
}


#tModel <- 'shouye =~ p3+p4+p7+p8\nzhanshi =~ p6+p14+p9+p13+p15\nyinxiao =~ p5+p11+p12+p17+p18+p20+p22\nshezhi =~ p16+p24'
#tModel2 <- 'shouye =~ p3+p4+p7+p8\n\nzhanshi =~ p6+p14+p9+p13+p15\n\nyinxiao =~ p5+p11+p12+p17+p18+p20+p22\n\nshezhi =~ p16+p24'
#tModel3 <- 'shouye =~ p3+p4+p7+p8\n\n\nzhanshi =~ p6+p14+p9+p13+p15\n\nyinxiao =~ p5+p11+p12+p17+p18+p20+p22\n\n\nshezhi =~ p16+p24'
# check alpha 
# checkAlpha <- function(model){
#   loLi1 = str_locate_all(model,'\\=~')
#   loLi2 = str_locate_all(model,'\\\n')
#   loLi3 = str_locate_all(model,'\\+')
#   return(list(a = loLi1, b = loLi2, c = loLi3))
# }
# 
# result = checkAlpha(model = tModel3)
# print(result$a)
# print(result$b)
# print(result$c)

# 找到最左边标记的\n
# listHT <- vector('integer')
# 
# a = NULL
# b = NULL
# 
# for(i in 1:(length(result$b[[1]])/2 - 1)){
#   listHT[1]<-result$b[[1]][1]
#   a <- result$b[[1]][i]
#   b <- result$b[[1]][i+1]
#   if((b-a) < 2){
#     next
#   }else{
#     listHT[length(listHT)+1] <- b
#   }
# }
# 
# for(i in 1:length(listHT)+1){
#   headNum = result$a[[1]][i,2]+2
#   for (j in 1:length(result$c[[1]])/2)
#     if(result$c[[1]][j,1] > headNum & result$c[[1]][j,1] < listHT[i] ){
#     
#   }
# 
# }

# 

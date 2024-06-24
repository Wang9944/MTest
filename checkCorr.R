# 判断是否适合用SME模型
# 1. cor （用户决定删掉哪个） 2. 三张图 3. KMO和巴雷特 （显示好坏）
# 1. UI界面上 左侧俩按钮，一个恢复到初始状态； 一个运行整体的


lstCo = list(NULL)
lstRo = list(NULL)
#lstVa = list(NULL)



checkCor <- function(df, lstCo, lstRo){
  cormax = cor(df)
  cutoff = 0.9
  count = 1
  mxlth = length(cormax[,1])
  for(i in 1:mxlth){
    for (j in i:mxlth){
      if(cormax[j,i]>cutoff & i!= j){
        #print(class(cormax[j,i]))
        lstCo[count] <<- colnames(cormax)[i]
        lstRo[count] <<- rownames(cormax)[j]
        #lstVa[count] <<- cormax[j,i]
        #print(cormax[j,i])
        count <<- count + 1
      }
    }
  }
  output = list(aa = lstCo, bb = lstRo)
}

# 改成检测到有cov大于0.9的就break出循环
checkCor2 <- function(df){
  lstCo = list(NULL)
  lstRo = list(NULL)
  cormax = cor(df[,-1])
  print(cormax)
  cutoff = 0.9
  mxlth = length(cormax[,1])
  for(i in 1:mxlth){
    for (j in i:mxlth){
      if(cormax[j,i]>cutoff & i!= j){
        lstCo =  colnames(cormax)[i]
        lstRo =  rownames(cormax)[j]
        output = list(aa = lstCo, bb = lstRo)
      }}
    return(output)
  
}}

  
checkCor3 <- function(df,switch){
  
  cormax = cor(df[,-1])
  cutoff = 0.9
  mxlth = length(cormax[,1])
  for(i in 1:mxlth){
    for (j in i:mxlth){
      if(cormax[j,i]>cutoff & i!= j){
        lstCo$data =  colnames(cormax)[i]
        lstRo$data =  rownames(cormax)[j]
        output = list(aa = lstCo$data, bb = lstRo$data)
        if(switch == 0){
          return(output)
          break}else{ 
            switch = 0
            next}
      }
    }
    
  }}


drawingPlot <- function(df){
  
  random = rchisq(nrow(df), 7)
  fake = lm(random ~., data = df)
  standardized = rstudent(fake)
  fitted= fake$fitted.values
  print(fitted)
  output = list(a = standardized, b = fitted)
  return(output)
}
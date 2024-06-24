# 用来处理missing data
# 1. 两个选择（1） 全部填0；（2）全部填平均数
# 默认输入的数据已经是标准格式了（左侧ID，上方item）

fillwZero <- function(a, df){
  if(a == 2){
    df[is.na(df)] <- 0
  }
  if(a == 3){
    df5 = df[,-1]
    for (i in 1:length(df5[1,])){
      df5[is.na(df5[,i]),i] <- round(sum(df5[,i], na.rm = TRUE)/length(df5[,i]),2)
    }
    df <- cbind(df[,1],df5)
  }
  return(df)
}




#locEnv <- new.env()

#locEnv$df6 <- df
#locEnv$df7 <- df

#fillwZero <- function(){
  #locEnv$df6[is.na(locEnv$df6)] <- 0
  #return(locEnv$df6)
#}




#fillwMean <- function(){
  #for (i in 1:length(locEnv$df7[1,])){
    #locEnv$df7[is.na(locEnv$df7[,i]),i] <-
      #sum(locEnv$df7[,i], na.rm = TRUE)/length(locEnv$df7[,i])
  #}
  #return(locEnv$df7)
#}



# 下面的是work的

#.locEnv <- new.env()
#assign('a', 2, envir = .locEnv)
#changetwo <- function(){
  #assign('a',1, envir = .locEnv)
  #get("a", envir = .locEnv)
#}
#changetwo()


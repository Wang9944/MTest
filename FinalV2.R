setwd('D:/teamwork/EasyCFA/pilotTestV5')
#setwd('C:/Users/Zhang/Desktop/BEIJING/app_SEMrun')
# 目前的问题：其他的图形还没有更新最新的模式
#  latest version: 只有删除的，没有其他???
#  目前的问题：信度的有点丑

library(DT)
library(shiny)
library(shinydashboard)
library(lavaan)
library('psych')
library("zoo")
library(Hmisc)
library(GPArotation)
library(semPlot)
library('devtools')
library(stringr)
library(knitr)
library(dplyr)
library(tidyr)
library(semTools)
library(zip)
library(rsconnect)

source('missingData.R')
source('CFAprocessing.R')
source('checkIndices.R')
source('updateMI.R')
source('checkCorr.R')
source('testCronbachAlpha.R')
source('AVEcacul.R')


######################## 下载部分尝试 ################
downloadUI <- function(id,label = "download") {
  ns <- NS(id)
  
  downloadButton(ns("data_download"), label = label)
}

# Define the server logic for a module
downloadServer <- function(id, data1,data2,data3,data4) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      
      output$data_download <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".zip", sep="")
        },
        
        
        content = function(file) {
          #write.csv(data1, file)
          tmpdir <- tempdir()
          setwd(tempdir())
          print(tempdir())
          
          fs <- c("因子载荷.csv", "AVE表格.csv",
                  "拟合指数.csv","模型.txt")
          write.csv(data1, file = "因子载荷.csv")
          write.csv(data2, file = "AVE表格.csv")
          write.csv(data3, file = "拟合指数.csv")
          cat(data4, file = "模型.txt")
          # ggsave(filename = "model.png", plot = plotPNG(),
          #        device = "png", width = 16, 
          #        height = 20, units = "cm", dpi = 600)
          
          #print (fs)
          
          zip(zipfile=file, files=fs)
        },
        contentType = "application/zip"
      )
      
    }
  )
}



downloadObj <- function(input, output, session, data) {
  
  
}

######################## 下载部分尝试 ################


tableList = vector("list")

ModelnIndiceList= vector('list')
#dashboard的头
header <- dashboardHeader(title = "模型评估与迭代")

#dashboard的主???
body <- dashboardBody(
  
  conditionalPanel("input.sidebarMenu == 'showtable'",
                   class = "shiny-input-container1",
                   DT::dataTableOutput("sample_table"), 
                   
  ),
  
  conditionalPanel("input.sidebarMenu == 'vcheck' ",
                   class = "shiny-input-container2",
                   fluidPage(
                     fluidRow(tags$div(class='aaa',DT::dataTableOutput("alphaTable"), 
                                       style = "font-size: 175%; width: 100%; 
                                  margin-left: 0px !important;")),
                     #fluidRow(div(DT::dataTableOutput("alphaTable"), style = "font-size: 175%; width: 175%; align: center")),
                     br(),
                     br(),
                     br(),
                     fluidRow(plotOutput("fstPlot"),style = "font-size: 150%; width: 100%;"),
                     div(id='aux'),
                   )),
  
  conditionalPanel("input.sidebarMenu == 'ccheck' ",
                   class = "shiny-input-container3",
                   fluidPage(tags$div(id = 'myid1'))
                   #fluidPage(tags$div(id = "newid"))
  ))


sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebarMenu",
    menuItem("1. 加载数据", tabName = "showtable", icon = icon("dashboard")),
    #menuItem("数据特征判断", tabName = "check", icon = icon("dashboard")),
    menuItem("2. 信度检验", icon = icon("th"), tabName = "vcheck",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("3. 效度检验", icon = icon("th"), tabName = "ccheck",
             badgeLabel = "new", badgeColor = "green")
  ),
  conditionalPanel("input.sidebarMenu == 'showtable'",
                   class = "shiny-input-container1",
                   fileInput('file','选择加载的文件')
                   #radioButtons("missingData", label = h4("填充缺失值"),choices = list("没有填充" = 1, "填充为0" = 2, "填充为均值" = 3),selected = 1)
  ),
  
  conditionalPanel("input.sidebarMenu == 'vcheck'",
                   class = "shiny-input-container2",
                   textAreaInput("modelInput", label = h3("输入模型"), 
                                 value = " ", width = '340px', height = '300px'),
                   actionButton("CFA", label = "运行CFA"),
                   br(),
                   actionButton("updateAlpha", label = "更新模型"),
                   br(),
                   p(HTML("<A HREF=\"javascript:history.go(0)\">点击这里，重启程序</A>"))
  ),
  
  
  conditionalPanel("input.sidebarMenu == 'ccheck'",
                   class = "shiny-input-container3",
                   textAreaInput("modelInput2", label = h3("输入模型"), 
                                 value = " ", width = '340px', height = '300px'),
                   actionButton("CFA2", label = "运行CFA"),
                   br(),
                   actionButton("update", label = "更新模型"),
                   br(),
                   p(HTML("<A HREF=\"javascript:history.go(0)\">点击这里，重启程序</A>"))
  )
)


ui <- dashboardPage(header,sidebar,body)

##############################################################


server <- shinyServer(function(input, output) {
  
  #模型全更???
  modelRecord <- reactiveValues(tList = vector('list'), value = NULL)
  
  # delete或者add的item
  
  deloradd <- reactiveValues(delValue = NULL, addValue = NULL)
  
  # 更新mi table
  miTable <- reactiveValues(value = vector('list'), value2 = vector('list'))
  
  # 效度之后模型更新
  modelUpdate <- reactiveValues(value = 0 )
  
  #信度之后模型更新
  modelUpdate2 <- reactiveValues(value = 0)
  
  #SEMmodels <- reactiveValues(nextM = 0 , crtM = 0, iniM = iniModel )
  
  # 信度更新计数
  alphaNum <- reactiveValues(preData = 0, crtData = 0)
  
  # 信效度检???
  genSwitch <- reactiveValues(value = 0)
  
  # AVE的对比表???
  AVEList <- reactiveValues(tList2 = vector('list'), fstValue = NULL, value = NULL, value2 = NULL)
  
  # cronbach alpha list
  alphaList <- reactiveValues(tList = vector('list'), value = NULL, iniValue = NULL)
  
  # alphaCronbach删掉的按???
  rowName <- reactiveValues(value = 0)
  
  SEMmodels <- reactiveValues(nextM = 0 , crtM = 0, iniM = 0, nextM2 = 0)
  
  modelNCount <- reactiveValues(preData = 0, crtData = 0)
  
  # indeice table?
  udList <- reactiveValues(tList = tableList , fstAdd = NULL,addList = NULL, addList2 = NULL )
  
  showTable <- reactiveValues(value = NULL)
  
  MIList <- reactiveValues(tList = vector('list'), iniMI = NULL, iniMI2 = NULL, fstMI= NULL)
  
  fitList <- reactiveValues(tList = vector('list'),fstValue = NULL,fitValue = NULL, fitValueOld = NULL, fitValue2 = NULL)
  
  corCheck <- reactiveValues(data = NULL)
  
  corLeft <- reactiveValues(data = NULL)
  
  corRight <- reactiveValues(data = NULL)
  
  fullDataset <- reactiveValues(data = NULL)
  
  standardizedValue <- reactiveValues(data =NULL)
  
  fittedValue <- reactiveValues(data =NULL)
  
  switchValue <- reactiveValues(data = 0)
  
  btest <- reactiveValues(data = 0, judge = NULL)
  calpha <- reactiveValues(data = 0, judge = NULL)
  KMOValue <- reactiveValues(data = 0, judge = NULL)
  
  # 加载数据并且填充missing data
  df_products_upload <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    fullDataset$data <- read.csv(inFile$datapath)
    
  })
  
  
  output$sample_table<- DT::renderDataTable({
    df_products_upload()
    DT::datatable(fullDataset$data,options = list(searching = FALSE,pageLength = 10,
                                                  scrollX = T))
  })
  
  
  # 信度检查，当没法更好的更新模型的时候，出现弹框，告知这一消息???
  
  observeEvent(input$pass, {
    removeModal()
  })
  
  
  
  dataModal <-reactive({
    modalDialog(span('模型已经无需再更新了'),style = "font-size: 50px",footer = tagList(modalButton("了解"))
    )
    
  })
  
  dataModal2 <-reactive({
    modalDialog(span('模型已经无需再更新了'),style = "font-size: 50px",footer = tagList(modalButton("了解"))
    )
    
  })
  
  
  
  # 点击信度检查后，出现提示信度检查流程弹???
  observeEvent(input$sidebarMenu, {
    req(input$sidebarMenu == "vcheck")
    
    str1 = tags$span(
      paste('1. 输入模型'),
      style = "font-size: 20px;"
    )
    str2 = tags$span(
      paste('2. 运行CFA，检查信度'),
      style = "font-size: 20px"
    )
    
    str3 = tags$span(
      paste('3. 更新模型，检查信度'),
      style = "font-size: 20px"
    )
    
    str4 = tags$span(
      paste('4. 当无法更新模型时，根据信度检查结果选择最佳模型'),
      style = "font-size: 20px"
    )
    str5 = tags$span(
      paste('信度检查流程'),
      style = "font-size: 25px"
    )
    
    showModal(modalDialog(
      title = str5,
      tagList(str1, br(), str2, br(), str3, br(), str4),footer = tagList(modalButton("了解"))
    ))
    
  })
  
  observeEvent(input$pass, {
    removeModal()
  })
  
  #  点击效度检查后，出现提示效度检查流程弹???
  
  observeEvent(input$sidebarMenu, {
    req(input$sidebarMenu == "ccheck")
    
    str1 = tags$span(
      paste('1. 输入模型'),
      style = "font-size: 20px;"
    )
    str2 = tags$span(
      paste('2. 运行CFA，检查效度'),
      style = "font-size: 20px"
    )
    
    str3 = tags$span(
      paste('3. 更新模型，检查效度'),
      style = "font-size: 20px"
    )
    
    str4 = tags$span(
      paste('4. 当无法更新模型时，根据效度检查结果选择最佳模型'),
      style = "font-size: 20px"
    )
    str5 = tags$span(
      paste('效度检查流程'),
      style = "font-size: 25px"
    )
    
    showModal(modalDialog(
      title = str5,
      tagList(str1, br(), str2, br(), str3, br(), str4),footer = tagList(modalButton("了解"))
    ))
    
  })
  
  observeEvent(input$pass, {
    removeModal()
  })
  
  # 进行第一次CFA的运行，获得各种信度
  runCFA <- eventReactive(input$CFA, {
    
    SEMmodels$iniM = input$modelInput
    
    CFAvar = getCFA(a = SEMmodels$iniM,b = fullDataset$data)
    indiceCheck <- checkIndices(indiceInput = CFAvar$a)
    fitList$fitValue = CFAvar$d
    MIList$iniMI = CFAvar$b
    
    udList$addList = indiceCheck
    
    df2 = fullDataset$data[,-1]
    cfafit <- cfa(SEMmodels$iniM,df2, std.lv = T)
    aa =  reliability(cfafit)
    bb = as.data.frame(aa[2,])
    colnames(bb)[1] <-"组合信度"
    
    
    is.num <- sapply(bb, is.numeric)
    bb[is.num] <- lapply(bb[is.num], round,3)
    
    
    
    alphaList$value = bb
    
    
    ########################## AVE的部???
    dd = as.data.frame(aa[5,])
    colnames(dd)[1] <-"AVE"
    is.num <- sapply(dd, is.numeric)
    dd[is.num] <- lapply(dd[is.num], round,3)
    
    eee = cov2cor(lavInspect(cfafit, what = "est")$psi)
    fff = as.data.frame(eee)
    is.num <- sapply(fff, is.numeric)
    fff[is.num] <- lapply(fff[is.num], round,3)
    
    
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
    
    
    alphaList$iniValue = alphaList$value
    
    returnList = list(a = CFAvar$a, b = CFAvar$b, c = CFAvar$c,
                      d = indiceCheck, e = CFAvar$d,f = CFAvar$e)
  })
  
  
  #绘制组合信度表格
  output$alphaTable<- DT::renderDataTable({
    runCFA()
    DT::datatable(alphaList$iniValue, options = list(dom = 't',searching = FALSE
    ))
  })
  
  
  
  # 绘制图案
  output$fstPlot <- renderPlot({
    if(input$CFA){
      semPaths(object = fitList$fitValue,
               whatLabels = "std",
               edge.label.cex = 1,
               layout = "circle", rotation = 1,
               color = "lightblue", height = 200, width = 300)
    }
    
    if(input$update){
      semPaths(object = fitList$fitValueOld,
               whatLabels = "std",
               edge.label.cex = 1,
               layout = "tree", rotation = 2,
               color = "lightblue")
    }
  })
  
  
  # 初步设置模型和下一个模型的变量赋???
  observeEvent(input$CFA, {
    SEMmodels$nextM = input$modelInput
    SEMmodels$crtM = input$modelInput
  })
  
  
  # 点击后更新组合信???
  genModelAlpha <- eventReactive(input$updateAlpha,{
    #df2 = fullDataset$data
    df2 = fullDataset$data[,-1]
    alphaResults <- getAlpha(df = df2, model = SEMmodels$crtM)
    SEMmodels$nextM =  alphaResults$b
    rowName$value =  alphaResults$c
    
  })
  
  
  
  observeEvent(input$updateAlpha, {
    genModelAlpha()
    
    
    # 如果长度???1??? 那么弹出资料，我不更新了
    if(nchar(SEMmodels$nextM) == 1){
      
      showModal(dataModal())
      
    }else{
      
      indiceCheck <- checkIndices(indiceInput = CFAvar$a)
      fitList$fitValue = CFAvar$d
      MIList$iniMI = CFAvar$b
      udList$addList = indiceCheck
      
      fitList$tList[[alphaNum$crtData]] = fitList$fitValue
      
      alphaList$tList[[alphaNum$crtData]] = alphaList$value
      
      
      
      insertUI(
        selector = "#aux",
        where = "afterEnd",
        
        
        
        ui = fluidPage(tags$head(
          tags$style(HTML("hr {border-top: 1px solid #000000;}"))
        ),
        hr(),
        fluidRow(plotOutput(paste0("plot3",alphaNum$crtData),width = "100%")),
        fluidRow(DT::dataTableOutput(paste0("table3",alphaNum$crtData)),width = "100%"),
        
        br(),
        hr(),
        ))
      
      
      for(i in 1:alphaNum$crtData){
        
        
        output[[paste0("table3",alphaNum$crtData)]] <- DT::renderDataTable({
          DT::datatable(alphaList$tList[[i]],
                        options = list(dom = 't',searching = FALSE
                        ))
        })
        
        output[[paste0("plot3",alphaNum$crtData)]] <- renderPlot(
          semPaths(object = fitList$tList[[i]],
                   whatLabels = "std",
                   edge.label.cex = 1,
                   layout = "tree", rotation = 2,
                   color = "lightblue"), height = 400, width = 500)
      }
      
    }
    alphaNum$preData = alphaNum$crtData
    
  })
  
  
  ###################################检查效???###########################
  
  
  # 运行CFA，第一次检查效???
  runCFA2 <- eventReactive(input$CFA2, {
    
    # 获得fit数???
    
    SEMmodels$iniM <- input$modelInput2
    df2 = fullDataset$data[,-1]
    ModelFit = cfa(SEMmodels$iniM,data = df2, std.lv = T)
    fitData = fitMeasures(ModelFit)
    
    # 画图需要的变量
    fitList$fitValue = ModelFit
    fitList$fstValue = ModelFit
    
    # 因子载荷表格
    flTable = parameterEstimates(fitList$fitValue, standardized=TRUE) %>%
      filter(op == "=~") %>% arrange(std.all)%>%
      select('Latent variable'=lhs, 'observed variable'=rhs, 'factor loading' =std.all)
    
    flTable = mutate(flTable, across(where(is.numeric), round, 2))
    
    MIList$fstMI =  flTable
    MIList$iniMI =  flTable
    # 拟合数值表???
    indiceCheck <- checkIndices(indiceInput = fitData)
    udList$addList = indiceCheck
    udList$fstAdd = indiceCheck
    
    # AVE数值表???
    aa =  reliability(ModelFit)
    dd = as.data.frame(aa[5,])
    colnames(dd)[1] <-"AVE"
    is.num <- sapply(dd, is.numeric)
    dd[is.num] <- lapply(dd[is.num], round,3)
    
    eee = cov2cor(lavInspect(ModelFit, what = "est")$psi)
    fff = as.data.frame(eee)
    is.num <- sapply(fff, is.numeric)
    fff[is.num] <- lapply(fff[is.num], round,3)
    
    
    for(i in 1: nrow(fff)){
      for(j in 1: nrow(fff)){
        fff[[i]][j] = abs(fff[[i]][j])
        if(j < i){
          fff[[i]][j] = NA
        }
        if(j == i){
          fff[[i]][i] = round(sqrt(dd[i,]),2)
        }
      }
    }
    
    AVEList$value = fff
    AVEList$fstValue = fff
    
    # 更新模型 + 每个list append到对应的list???
    SEMmodels$nextM = SEMmodels$iniM
    SEMmodels$crtM = SEMmodels$iniM
    fitList$tList = vector('list')
    
    modelNCount$crtData = modelNCount$preData + 1
    
    udList$tList[[1]] = udList$addList
    MIList$tList[[1]] = MIList$iniMI
    AVEList$tList2[[1]] = AVEList$value
    fitList$tList[[1]] = fitList$fitValue
    modelRecord$tList[[1]] = SEMmodels$iniM
    
    
    
  })
  
  
  
  #  绘制图形
  observeEvent(input$CFA2, {
    
    runCFA2()
    
    #########  插入 UI ###########
    insertUI(
      selector = "#myid1",
      where = "afterBegin",
      
      ui = column(width = 12,
                  fluidRow(style = "background:#4682b4; width: 100%;padding-top:19px",
                           
                           column(width = 6,
                                  wellPanel(
                                    style = "height:100%;",
                                    plotOutput("plot8")),
                           ),
                           column(width = 6, style = "height:100%;font-size: 225%;",
                                  fluidRow(
                                    wellPanel(
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      "初始模型",
                                      br(),
                                      br(),
                                      br(),
                                      br()),
                                    )
                                  
                           )
                  ),
                  
                  
                  fluidRow(style = "position:relative;font-size: 125%;background:#4682b4",
                           
                           column(width = 6,
                                  style = "position:absolute;top:0;bottom:0;left:0;padding-bottom:19px",
                                  wellPanel(
                                    style = "height:100%;",
                                    DT::dataTableOutput("table8")),
                           ),
                           
                           column(width = 6, 
                                  fluidRow(
                                    wellPanel(
                                      DT::dataTableOutput("table7"))
                                  ),
                                  
                                  fluidRow(
                                    wellPanel( DT::dataTableOutput("tableAVE8")))
                                  , offset = 6
                           ))
                  
      )
      
    )
    
    
    # 第一次进行效度分析后绘制图形
    output$tableAVE8<- DT::renderDataTable({
      DT::datatable(AVEList$fstValue, caption = htmltools::tags$caption( style = 'caption-side: top;
                                                                                            text-align: center; color:black;
                                                                                            font-size:150% ;','区别效度表格'),
                    options = list(dom = 't',searching = FALSE
                    ))
    })
    
    
    output$table7<- DT::renderDataTable({
      DT::datatable(udList$fstAdd,caption = htmltools::tags$caption( style = 'caption-side: top;
                                                                                            text-align: center; color:black;
                                                                                            font-size:150% ;','拟合指数表格'),
                    options = list(dom = 't',searching = FALSE
                    ))
    })
    
    # #绘制第一次运行后的图
    output$table8<- DT::renderDataTable({
      DT::datatable(MIList$fstMI,caption = htmltools::tags$caption( style = 'caption-side: top;
                                                                                            text-align: center; color:black;
                                                                                            font-size:150% ;','因子载荷表格'))
    })
    
    
    output$plot8 <- renderPlot({
      semPaths(object = fitList$fstValue,
               whatLabels = "std",
               edge.label.cex = 1,
               layout = "circle", rotation = 1,
               color = "lightblue", height = 500, width = 600)})
    
    
    
  })
  
  
  
  
  
  
  ############# 效度检查后更新模型 ###############
  
  genModel <- eventReactive(input$update, {
    
    # 更新模型后，判断新模型的个数
    
    dataframeMI = modificationindices(fitList$fitValue,sort. = T)
    dataframeMI= dataframeMI[dataframeMI$op == "~~",]
    miTable$value = dataframeMI
    
    results<- updateModel2(fit =  fitList$fitValue, milist = miTable$value, model = SEMmodels$crtM )
    
    
    # 如果nchar(results) == 1, 那么表示没有好更新的???,否则可以继续更新
    if(length(results) == 1){
      
      modelUpdate$value = 1
      showModal(dataModal2())
      print('print out to show what a beautiful model I got')
      
    }else{
      
      showTable$value = length(results)
      
      
      if(length(results) == 2){
        
        # 设置成nextM，获取重要的两个变量
        SEMmodels$nextM <- results$a
        
        modelRecord$Value <- SEMmodels$nextM
        
        deloradd$delValue <- paste0("删除了项目", results$d)
        
        
        df2 = fullDataset$data[,-1]
        ModelFit = cfa(SEMmodels$nextM,data = df2, std.lv = T)
        fitData = fitMeasures(ModelFit)
        
        # 用于画图的变???
        fitList$fitValue = ModelFit
        
        
        # 因子载荷表格
        flTable = parameterEstimates(fitList$fitValue, standardized=TRUE) %>%
          filter(op == "=~") %>% arrange(std.all)%>%
          select('Latent variable'=lhs, 'observed variable'=rhs, 'factor loading' =std.all)
        
        flTable = mutate(flTable, across(where(is.numeric), round, 2))
        MIList$iniMI =  flTable
        
        # 拟合数值表???
        indiceCheck <- checkIndices(indiceInput = fitData)
        #udList$addList2 = udList$addList
        udList$addList = indiceCheck
        
        # AVE数值表??? 
        aa2 =  reliability(ModelFit)
        dd2 = as.data.frame(aa2[5,])
        colnames(dd2)[1] <-"AVE"
        is.num <- sapply(dd2, is.numeric)
        dd2[is.num] <- lapply(dd2[is.num], round,3)
        
        eee2 = cov2cor(lavInspect(ModelFit, what = "est")$psi)
        fff2 = as.data.frame(eee2)
        is.num <- sapply(fff2, is.numeric)
        fff2[is.num] <- lapply(fff2[is.num], round,3)
        
        
        for(i in 1: nrow(fff2)){
          for(j in 1: nrow(fff2)){
            fff2[[i]][j] = abs(fff2[[i]][j])
            if(j < i){
              fff2[[i]][j] = NA
            }
            if(j == i){
              fff2[[i]][i] = round(sqrt(dd2[i,]),2)
            }
          }
        }
        
        AVEList$value = fff2
        
        ######### 每个table ??? append到对应的list???
        modelNCount$crtData = modelNCount$preData + 1
        
        udList$tList[[modelNCount$crtData]] = udList$addList
        MIList$tList[[modelNCount$crtData]] = MIList$iniMI
        AVEList$tList2[[modelNCount$crtData]] = AVEList$value
        fitList$tList[[modelNCount$crtData]] = fitList$fitValue
        modelRecord$tList[[modelNCount$crtData]] = modelRecord$Value
        
        #把nextM 设置成crtM
        SEMmodels$crtM <- SEMmodels$nextM
        
        
        
      }
      
      if(length(results) == 4){
        
        #modelNCount$crtData = modelNCount$preData + 2
        
        SEMmodels$nextM <- results$b
        SEMmodels$nextM2 <- results$a
        
        
        ##################  第一个模??? ######################3
        
        deloradd$delValue <- paste0("删除了项目", results$d)
        deloradd$addValue <- paste0("新增项目之间的关系：", results$c)
        
        
        df2 = fullDataset$data[,-1]
        ModelFit = cfa(SEMmodels$nextM,data = df2, std.lv = T)
        fitData = fitMeasures(ModelFit)
        
        # 用于画图的变???
        fitList$fitValue = ModelFit
        
        
        # 因子载荷表格
        flTable = parameterEstimates(fitList$fitValue, standardized=TRUE) %>%
          filter(op == "=~") %>% arrange(std.all)%>%
          select('Latent variable'=lhs, 'observed variable'=rhs, 'factor loading' =std.all)
        
        flTable = mutate(flTable, across(where(is.numeric), round, 2))
        MIList$iniMI =  flTable
        
        # 拟合数值表???
        indiceCheck <- checkIndices(indiceInput = fitData)
        #udList$addList2 = udList$addList
        udList$addList = indiceCheck
        
        # AVE数值表??? 
        aa2 =  reliability(ModelFit)
        dd2 = as.data.frame(aa2[5,])
        colnames(dd2)[1] <-"AVE"
        is.num <- sapply(dd2, is.numeric)
        dd2[is.num] <- lapply(dd2[is.num], round,3)
        
        eee2 = cov2cor(lavInspect(ModelFit, what = "est")$psi)
        fff2 = as.data.frame(eee2)
        is.num <- sapply(fff2, is.numeric)
        fff2[is.num] <- lapply(fff2[is.num], round,3)
        
        
        for(i in 1: nrow(fff2)){
          for(j in 1: nrow(fff2)){
            fff2[[i]][j] = abs(fff2[[i]][j])
            if(j < i){
              fff2[[i]][j] = NA
            }
            if(j == i){
              fff2[[i]][i] = round(sqrt(dd2[i,]),2)
            }
          }
        }
        
        AVEList$value = fff2
        
        
        
        ######################  第二个模??? ####################
        
        
        # 第二个模???
        ModelFit2 = cfa(SEMmodels$nextM2,data = df2)
        
        flTable2 = parameterEstimates(ModelFit2, standardized=TRUE) %>%
          filter(op == "=~") %>% arrange(std.all)%>%
          select('latent variable'=lhs,'observed variable'=rhs, 'factor loading' =std.all)
        
        
        flTable2 = mutate(flTable2, across(where(is.numeric), round, 2))
        
        MIList$iniMI2 =  flTable2
        
        
        fitData2 = fitMeasures(ModelFit2)
        indiceCheck2 <- checkIndices(indiceInput = fitData2)
        udList$addList2 = indiceCheck2
        fitList$fitValue2 = ModelFit2
        
        
        #mi的更???
        
        
        dataframeMI2 = modificationindices(ModelFit2,sort. = T)
        dataframeMI2= dataframeMI[dataframeMI2$op == "~~",]
        
        miTable$value = dataframeMI2
        
        ###################3  AVE2
        aa3 =  reliability(ModelFit2)
        dd3 = as.data.frame(aa2[5,])
        colnames(dd3)[1] <-"AVE"
        is.num <- sapply(dd3, is.numeric)
        dd3[is.num] <- lapply(dd3[is.num], round,3)
        
        eee3 = cov2cor(lavInspect(ModelFit2, what = "est")$psi)
        fff3 = as.data.frame(eee3)
        is.num <- sapply(fff3, is.numeric)
        fff3[is.num] <- lapply(fff3[is.num], round,3)
        
        
        for(i in 1: nrow(fff3)){
          for(j in 1: nrow(fff3)){
            fff3[[i]][j] = abs(fff3[[i]][j])
            if(j < i){
              fff3[[i]][j] = NA
            }
            if(j == i){
              fff3[[i]][i] = round(sqrt(dd3[i,]),3)
            }
          }
        }
        
        #print(fff2)
        AVEList$value = fff3
        
        ######### 每个table ??? append到对应的list???
        modelNCount$crtData = modelNCount$preData + 2
        
        #拟合指数
        udList$tList[[modelNCount$crtData-1]] = udList$addList
        udList$tList[[modelNCount$crtData]] = udList$addList2
        
        #因子载荷
        MIList$tList[[modelNCount$crtData - 1]] = MIList$iniMI
        MIList$tList[[modelNCount$crtData]] = MIList$iniMI2
        
        #区分效度
        AVEList$tList2[[modelNCount$crtData-1]] = AVEList$value
        AVEList$tList2[[modelNCount$crtData]] = AVEList$value2
        
        #SEM???
        fitList$tList[[modelNCount$crtData-1]] = fitList$fitValue
        fitList$tList[[modelNCount$crtData]] = fitList$fitValue2
        
      }
      
    }
    
  })
  
  
  
  ####################### 更新UI和render#############
  observeEvent(input$update, {
    
    genModel()
    
    
    ##################### 如果返回的新模型只有一个的???
    
    if(showTable$value == 2){
      
      
      insertUI(
        selector = "#myid1",
        where = "afterBegin",
        
        
        ui = fluidPage(style = " width: 100%",width = 12,
                       fluidRow(style = 'background:#4682b4',
                                
                                br(),
                                
                                column(width = 6,
                                       wellPanel(
                                         style = "height:100%;font-size: 125%",
                                         plotOutput(paste0("2plots",modelNCount$crtData))
                                         #plotOutput("plots1")
                                       ),
                                ),
                                column(width = 6, style = "height:100%;font-size: 225%;",
                                       
                                       fluidRow(
                                         wellPanel(
                                           br(),
                                           br(),
                                           br(),
                                           paste0("更新模型的次数：",modelNCount$crtData),
                                           br(),
                                           br(),
                                           paste0("和上次相比：", deloradd$delValue),
                                           br(),
                                           br(),
                                           downloadUI(paste0("download",modelNCount$crtData ),
                                                      paste0("下载资料 #", modelNCount$crtData)),
                                           br(),
                                           br(),
                                         )
                                       ),)
                                
                                
                       ),
                       
                       fluidRow(style = "position:relative;font-size:125%;background:#4682b4;",
                                
                                column(width = 6,
                                       style = "position:absolute;top:0;bottom:0;left:0;padding-bottom:19px",
                                       wellPanel(
                                         style = "height:100%;",
                                         DT::dataTableOutput(paste0("2table",modelNCount$crtData))),
                                ),
                                
                                column(width = 6, 
                                       fluidRow(
                                         wellPanel(
                                           DT::dataTableOutput(paste0('3table',modelNCount$crtData)))
                                       ),
                                       
                                       fluidRow(
                                         wellPanel( DT::dataTableOutput(paste0("2tableAVE",modelNCount$crtData))))
                                       , offset = 6
                                )),
                       br(),br(),br()
                       
        )
        
      )
      
      
      
      for(i in 1:modelNCount$crtData){
        
        downloadServer(id=paste0("download",modelNCount$crtData), 
                       data1 = MIList$tList[[i]],
                       data2 = AVEList$tList2[[i]],
                       data3 = unlist(udList$tList[[i]]),
                       data4 = modelRecord$tList[[i]])
        
        output[[paste0("3table",modelNCount$crtData)]] <- DT::renderDataTable({
          DT::datatable(as.data.frame(udList$tList[[i]]),caption = htmltools::tags$caption( style = 'caption-side: top;
                                                                                            text-align: center; color:black;
                                                                                            font-size:200% ;','拟合指数表格'),
                        options = list(dom = 't',searching = FALSE
                        ))
        })
        
        output[[paste0("2table",modelNCount$crtData)]] <- DT::renderDataTable({
          DT::datatable(as.data.frame(MIList$tList[[i]]),caption = htmltools::tags$caption( style = 'caption-side: top;
                                                                                            text-align: center; color:black;
                                                                                            font-size:200% ;','因子载荷表格'))
        })
        
        
        output[[paste0("2tableAVE",modelNCount$crtData)]] <- DT::renderDataTable({
          DT::datatable(as.data.frame(AVEList$tList2[[i]]),caption = htmltools::tags$caption( style = 'caption-side: top;
                                                                                            text-align: center; color:black;
                                                                                            font-size:200% ;','区别效度表格'),
                        
                        options = list(dom = 't',searching = FALSE
                        ))
        })
        
        output[[paste0("2plots",modelNCount$crtData)]] <- renderPlot(
          semPaths(object = fitList$tList[[i]],
                   whatLabels = "std",
                   edge.label.cex = 1,
                   layout = "circle", rotation = 1,
                   color = "lightblue"))
        
        
      }
      
      
      
      
      
    }
    
    #########################如果是两个模型更新出???
    if(showTable$value == 4){
      
      
      #######################  new UI and Rendering #################
      
      ui = fluidPage(style = " width: 100%",width = 12,
                     
                     fluidRow(style = 'background:#4682b4',
                              
                              br(),
                              
                              column(width = 6,
                                     wellPanel(
                                       style = "height:100%;font-size: 125%",
                                       plotOutput(paste0("updatePlot1",modelNCount$crtData))
                                       #plotOutput("plots1")
                                     ),
                              ),
                              column(width = 6, style = "height:100%;font-size: 225%;",
                                     
                                     fluidRow(
                                       wellPanel(
                                         br(),
                                         br(),
                                         br(),
                                         paste0("更新模型的次数：",modelNCount$crtData),
                                         br(),
                                         br(),
                                         paste0("和上次相比：", deloradd$delValue),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                       )
                                     ),)),
                     
                     fluidRow(style = "position:relative;font-size:125%;background:#4682b4;",
                              
                              column(width = 6,
                                     style = "position:absolute;top:0;bottom:0;left:0;padding-bottom:19px",
                                     wellPanel(
                                       style = "height:100%;",
                                       DT::dataTableOutput(paste0("flTable1",modelNCount$crtData))),
                              ),
                              
                              column(width = 6, 
                                     fluidRow(
                                       wellPanel(
                                         DT::dataTableOutput(paste0('fittingTable1',modelNCount$crtData)))
                                     ),
                                     
                                     fluidRow(
                                       wellPanel( DT::dataTableOutput(paste0("AVEtable1",modelNCount$crtData))))
                                     , offset = 6
                              )),
                     br(),br(),br(),
                     fluidRow(style = 'background:#4682b4',
                              
                              br(),
                              
                              column(width = 6,
                                     wellPanel(
                                       style = "height:100%;font-size: 125%",
                                       plotOutput(paste0("updatePlot1",modelNCount$crtData - 1))
                                       #plotOutput("plots1")
                                     ),
                              ),
                              column(width = 6, style = "height:100%;font-size: 225%;",
                                     
                                     fluidRow(
                                       wellPanel(
                                         br(),
                                         br(),
                                         br(),
                                         paste0("更新模型的次数：",modelNCount$crtData - 1),
                                         br(),
                                         br(),
                                         paste0("和上次相比：", deloradd$addValue),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                       )
                                     ),)),
                     
                     fluidRow(style = "position:relative;font-size:125%;background:#4682b4;",
                              
                              column(width = 6,
                                     style = "position:absolute;top:0;bottom:0;left:0;padding-bottom:19px",
                                     wellPanel(
                                       style = "height:100%;",
                                       DT::dataTableOutput(paste0("flTable1",modelNCount$crtData-1))),
                              ),
                              
                              column(width = 6, 
                                     fluidRow(
                                       wellPanel(
                                         DT::dataTableOutput(paste0('fittingTable1',modelNCount$crtData-1)))
                                     ),
                                     
                                     fluidRow(
                                       wellPanel( DT::dataTableOutput(paste0("AVEtable1",modelNCount$crtData-1))))
                                     , offset = 6
                              )),
                     
      )
      
      
      
      
      
      ############################插入UI界面end
      
      for(i in 1:modelNCount$crtData){
        
        
        
        output[[paste0("AVEtable1",modelNCount$crtData)]] <- DT::renderDataTable({
          DT::datatable(as.data.frame(AVEList$tList2[[i]]),caption = htmltools::tags$caption( style = 'caption-side: top;
                                                                                            text-align: center; color:black;
                                                                                            font-size:200% ;','区别效度表格'),
                        
                        options = list(dom = 't',searching = FALSE
                        ))
        })
        
        
        output[[paste0("fittingTable1",modelNCount$crtData)]] <- DT::renderDataTable({
          DT::datatable(as.data.frame(udList$tList[[i]]),caption = htmltools::tags$caption( style = 'caption-side: top;
                                                                                            text-align: center; color:black;
                                                                                            font-size:200% ;','拟合指数表格'),
                        
                        options = list(dom = 't',searching = FALSE
                        ))
        })
        
        
        
        output[[paste0("flTable1",modelNCount$crtData)]] <- DT::renderDataTable({
          DT::datatable(as.data.frame(MIList$tList[[i]]),caption = htmltools::tags$caption( style = 'caption-side: top;
                                                                                            text-align: center; color:black;
                                                                                            font-size:200% ;','因子载荷表格'))
        })
        
        output[[paste0("updatePlot1",modelNCount$crtData)]] <- renderPlot(
          semPaths(object = fitList$tList[[i]],
                   whatLabels = "std",
                   edge.label.cex = 1,
                   layout = "circle", rotation = 1,intercepts = FALSE, residuals = FALSE,
                   color = "lightblue"))
        
        
        # 第二个模型的全部
        
        output[[paste0("AVEtable1",modelNCount$crtData-1)]] <- DT::renderDataTable({
          DT::datatable(as.data.frame(AVEList$tList2[[i-1]]),caption = htmltools::tags$caption( style = 'caption-side: top;
                                                                                            text-align: center; color:black;
                                                                                            font-size:200% ;','区别效度表格'),
                        
                        options = list(dom = 't',searching = FALSE
                        ))
        })
        
        
        output[[paste0("fittingTable1",modelNCount$crtData - 1)]] <- DT::renderDataTable({
          DT::datatable(as.data.frame(udList$tList[[i - 1]]),caption = htmltools::tags$caption( style = 'caption-side: top;
                                                                                            text-align: center; color:black;
                                                                                            font-size:200% ;','拟合指数表格'),
                        
                        options = list(dom = 't',searching = FALSE
                        ))
        })
        
        
        output[[paste0("flTable1",modelNCount$crtData - 1)]] <- DT::renderDataTable({
          DT::datatable(as.data.frame(MIList$tList[[i - 1]]),caption = htmltools::tags$caption( style = 'caption-side: top;
                                                                                            text-align: center; color:black;
                                                                                            font-size:200% ;','因子载荷???'),
                        
                        options = list(dom = 't',searching = FALSE
                        ))
        })
        
        output[[paste0("updatePlot1",modelNCount$crtData - 1)]] <- renderPlot(
          semPaths(object = fitList$tList[[i-1]],
                   whatLabels = "std",
                   edge.label.cex = 1,
                   layout = "circle",rotation = 1,intercepts = FALSE, residuals = FALSE,
                   color = "lightblue"))
        
      }
    }
    
    
    modelNCount$preData = modelNCount$crtData
  }
  )} )

shinyApp(ui, server)





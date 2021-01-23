library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(d3r)
library(DescTools)
library(corrplot)
library(ggpubr)
library(sunburstR)
library(FactoMineR)
library(factoextra)
library(fastDummies)
library(caret)
library(randomForest)
library(cvms)
library(broom)
library(tibble)
library(nnet)
library(ROCR)
library(gamlss.add)
library(rattle)


set.seed(123)

namesFUN <- function(x,y){
  l = c()
  for(i in 1:length(x)){
    l = c(l,c(paste(y[i],"%",x[i],sep=" ")))
  }
  return(l)
}

server <- function(input, output){
  filedata <- reactive({
    data <- read.csv("/home/baha/Documents/student-por.csv")
    
    data$sex <- ifelse(data$sex=="F","female","male")
    data$address <- ifelse(data$address=="U","urban","rural")
    data$Pstatus <- ifelse(data$Pstatus=="T","living together","apart")
    data$Medu <- ifelse(data$Medu==0,"no education",ifelse(data$Medu==1,"primary school",ifelse(data$Medu==2,"middle school",ifelse(data$Medu==3,"high school","university"))))
    data$schoolsup <-ifelse(data$schoolsup=="yes","schoolsup_yes","schoolsup_no")
    data$famsup <-ifelse(data$famsup=="yes","famsup_yes","famsup_no")
    data$paid <-ifelse(data$paid=="yes","paid_yes","paid_no")
    data$activities <-ifelse(data$activities=="yes","activities_yes","activities_no")
    data$nursery <-ifelse(data$nursery=="yes","nursery_yes","nursery_no")
    data$higher <- ifelse(data$higher=="no","lazy","ambitious")
    data$internet <- ifelse(data$internet=="no","no_internet","Internet")
    data$romantic <- ifelse(data$romantic=="no","no_romance","in_love")
    data$famrel <- ifelse(data$famrel==1,"very_bad_famrel",ifelse(data$famrel==2,"bad_famrel",ifelse(data$famrel==3,"average_famrel",ifelse(data$famrel==4,"good_famrel","excelent_famrel"))))
    data$freetime <- ifelse(data$freetime==1,"very_low_FT",ifelse(data$freetime==2,"low_FT",ifelse(data$freetime==3,"average_FT",ifelse(data$freetime==4,"high_FT","very_high_FT"))))
    data$goout <- ifelse(data$goout==1,"very_low_Gout",ifelse(data$goout==2,"low_Gout",ifelse(data$goout==3,"average_Gout",ifelse(data$goout==4,"high_Gout","very_high_Gout"))))
    data$Dalc <- ifelse(data$Dalc==1,"very_low_Dalc",ifelse(data$Dalc==2,"low_Dalc",ifelse(data$Dalc==3,"average_Dalc",ifelse(data$Dalc==4,"high_Dalc","very_high_Dalc"))))
    data$Walc <- ifelse(data$Walc==1,"very_low_Walc",ifelse(data$Walc==2,"low_Walc",ifelse(data$Walc==3,"average_Walc",ifelse(data$Walc==4,"high_Walc","very_high_Walc"))))
    data$health <- ifelse(data$health==1,"very_bad_health",ifelse(data$health==2,"bad_health",ifelse(data$health==3,"average_health",ifelse(data$health==4,"good_health","excelent_health"))))
    data$Class <-ifelse(data$G3>=10,"devoted","not_devoted")
    
    return(as.data.frame(data))
  })
  output$filetable= DT::renderDataTable({
    filedata()
  })
  
  output$desc1 <- renderPrint({
    summary(as.factor(filedata()[[input$varCat]]))
  })
  
  output$desc2 <- renderPrint({
    summary(filedata()[[input$varNum]])
  })
  
  output$barplotUA <- renderPlotly({
    data <- filedata()
    x <- levels(as.factor(data[[input$varCat]]))
    y <-round(prop.table(table(as.factor(data[[input$varCat]])))*100,2)
    names(y)<-NULL
    y <- as.vector(y)
    text <- namesFUN(x,y)
    data <- data.frame(x, y, text)
    fig <- plot_ly(data, x = ~x, y = ~y, type = 'bar', text = text,
                   marker = list(color = 'rgb(158,202,225)',
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1.5)))
    fig <- fig %>% layout(title = "",
                          xaxis = list(title = ""),
                          yaxis = list(title = ""))
    
    return(fig)
    
  })
  output$boxplotUA <- renderPlotly({
    data <- filedata()
    fig <- plot_ly(y = ~data[[input$varNum]], type = "box")
    return(fig)
  })
  
  output$histUA <- renderPlotly({
    data <- filedata()
    fig <- ggplot(data,aes(x=data[[input$varNum]])) +
      geom_histogram() +
      stat_function(fun = dnorm, colour = "red",
                    arg = list(mean = mean(data[[input$varNum]], na.rm = TRUE),
                               sd = sd(data[[input$varNum]], na.rm = TRUE)))
    return(ggplotly(fig))
  })
  
  tree <- reactive({
    data <- filedata()
    vec <- input$varCat2
    df0 <- data[,vec]
    t <- table(df0)
    t0 <- as.data.frame(t,stringsAsFactors = FALSE)
    t00 <-t0[t0[["Freq"]] !=0,]
    names(t00) <- c(paste("level",1:(ncol(t00)-1)),"size")
    tree <- d3_nest(t00, value_cols = "size")
    return(tree)
  })
  output$sunburst <- renderSunburst({
    sunburst(tree(), width="100%", height=600)
  })
  
  output$khi2 <- renderPrint({
    chisq.test(filedata()[[input$varCat3]],filedata()[[input$varCat4]])
  })
  output$Gtest <- renderPrint({
    GTest(filedata()[[input$varCat3]],filedata()[[input$varCat4]])
  })
  
  output$SWNorm <- renderPrint({
    data <- filedata()
    shap <- shapiro.test(as.numeric(data[[input$varNum]]))
    if(shap$p.value<0.05){
      ch <- paste("p-value equals",shap$p.value,"<< 0.05 then" ,input$varNum,"is not normally distributed",sep=" ")
    }else{
      ch <- paste("p-value equals",shap$p.value,"> 0.05 then" ,input$varNum,"is normally distributed",sep=" ")
    }
    return(ch[1])
  })
  
  output$CT <- renderPrint({
    ftable(filedata()[,input$varCat5])
  })
  
  output$corrplot <- renderPlot({
    data <- filedata()
    corrplot.mixed(cor(data[,c("age","studytime","failures","absences","G3")]))
  })
  
  output$PearsonCorr <- renderPrint({
    data <- filedata()
    cor.test(data[[input$varNum1]],data[[input$varNum2]],method = "pearson")
  })
  output$KendallCorr <- renderPrint({
    data <- filedata()
    cor.test(data[[input$varNum1]],data[[input$varNum2]],method = "kendall")
  })
  output$SpearmanCorr <- renderPrint({
    data <- filedata()
    cor.test(data[[input$varNum1]],data[[input$varNum2]],method = "spearman")
  })
  
  output$scatter <- renderPlot({
    data <- filedata()
    ggscatter(data, x = input$varNum3, y = input$varNum4, 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = input$varNum3, ylab = input$varNum4)
  })
  
  output$qqplot <- renderPlot({
    data <- filedata()
    return(ggqqplot(data[[input$varNum]],ylab = input$varNum))
  })
  
  output$screeP <- renderPlot({
    data <- filedata()
    data_mca <- data[,c("Mjob","Medu","internet","freetime","sex","goout")]
    res.mca <- MCA(data_mca, graph = FALSE)
    p <- fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 15))
    return(p)
  })
  
  output$corrP <- renderPlot({
    data <- filedata()
    data_mca <- data[,c("Mjob","Medu","internet","freetime","sex","goout")]
    res.mca <- MCA(data_mca, graph = FALSE)
    return(fviz_mca_var(res.mca, choice = "mca.cor", 
                        repel = TRUE, # Avoid text overlapping (slow)
                        ggtheme = theme_minimal()))
  })
  
  output$varP <- renderPlot({
    data <- filedata()
    data_mca <- data[,c("Mjob","Medu","internet","freetime","sex","goout","Class")]
    res.mca <- MCA(data_mca, graph = FALSE,quali.sup=ncol(data_mca))
    fviz_mca_var(res.mca, 
                 repel = TRUE, # Avoid text overlapping (slow)
                 ggtheme = theme_minimal())
  })
  
  output$cos2P <- renderPlot({
    data <- filedata()
    data_mca <- data[,c("Mjob","Medu","internet","freetime","sex","goout")]
    res.mca <- MCA(data_mca, graph = FALSE)
    fviz_mca_var(res.mca, col.var = "cos2",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                 repel = TRUE, # Avoid text overlapping
                 ggtheme = theme_minimal())
  })
  
  output$anova <- renderPrint({
    data <- filedata()
    anova <- aov(data[[input$varNum5]]~data$Class,data=data)
    return(summary(anova))
  })
  
  
  
  output$Resamp <- DT::renderDataTable({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    
    
    
    df_dum <- dummy_cols(df[,-16])
    df_dum <- df_dum[,-1:-11]
    df_dum <- cbind(df_dum,df["Class"])
    
    index <- createDataPartition(df_dum$Class, p = 0.7, list = FALSE)
    train_data <- df_dum[index, ]
    test_data  <- df_dum[-index, ]
    train_downsampling <- downSample(x=train_data[,-45],y=as.factor(train_data[,45]),list=FALSE)
    return(train_downsampling)
  })
  
  
  output$tabletarget <- renderPrint({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    df_dum <- dummy_cols(df[,-16])
    df_dum <- df_dum[,-1:-11]
    df_dum <- cbind(df_dum,df["Class"])
    index <- createDataPartition(df_dum$Class, p = 0.7, list = FALSE)
    train_data <- df_dum[index, ]
    test_data  <- df_dum[-index, ]
    train_downsampling <- downSample(x=train_data[,-50],y=as.factor(train_data$Class),list=FALSE)
    return(table(train_downsampling$Class))
  })
  
  output$FRcm <- renderPlot({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    df_dum <- dummy_cols(df[,-16])
    df_dum <- df_dum[,-1:-11]
    df_dum <- cbind(df_dum,df["Class"])
    index <- createDataPartition(df_dum$Class, p = 0.7, list = FALSE)
    train_data <- df_dum[index, ]
    test_data  <- df_dum[-index, ]
    train_downsampling <- downSample(x=train_data[,-50],y=as.factor(train_data$Class),list=FALSE)
    attach(train_downsampling)
    rf <- randomForest(Class ~ .,data=train_downsampling,importance=TRUE)
    pred <- predict(rf,newdata = test_data)
    dtest <- tibble("target"=test_data$Class,"prediction"=pred)
    basic_table_test <- table(dtest)
    cfm2 <- tidy(basic_table_test)
    plot_confusion_matrix(cfm2, 
                          target_col = "target", 
                          prediction_col = "prediction",
                          counts_col = "n")
  })
  
  output$RFroc <- renderPlot({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    df_dum <- dummy_cols(df[,-16])
    df_dum <- df_dum[,-1:-11]
    df_dum <- cbind(df_dum,df["Class"])
    index <- createDataPartition(df_dum$Class, p = 0.7, list = FALSE)
    train_data <- df_dum[index, ]
    test_data  <- df_dum[-index, ]
    train_downsampling <- downSample(x=train_data[,-50],y=as.factor(train_data$Class),list=FALSE)
    attach(train_downsampling)
    rf <- randomForest(Class ~ .,data=train_downsampling,importance=TRUE)
    modele1.posterior<-predict(rf,type='prob',newdata=test_data[,-50])[,2]
    modele1.pred<-prediction(modele1.posterior, test_data[,50])
    modele1.roc<-performance(modele1.pred, "tpr","fpr")
    plot(modele1.roc,colorize=TRUE) 
    
  })
  
  output$RFauc <- renderPrint({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    df_dum <- dummy_cols(df[,-16])
    df_dum <- df_dum[,-1:-11]
    df_dum <- cbind(df_dum,df["Class"])
    index <- createDataPartition(df_dum$Class, p = 0.7, list = FALSE)
    train_data <- df_dum[index, ]
    test_data  <- df_dum[-index, ]
    train_downsampling <- downSample(x=train_data[,-50],y=as.factor(train_data$Class),list=FALSE)
    attach(train_downsampling)
    rf <- randomForest(Class ~ .,data=train_downsampling,importance=TRUE)
    modele1.posterior<-predict(rf,type='prob',newdata=test_data[,-50])[,2]
    modele1.pred<-prediction(modele1.posterior, test_data[,50])
    modele1.roc<-performance(modele1.pred, "tpr","fpr")
    modele1.auc<-performance(modele1.pred, "auc") 
    return(paste("AUC = ",modele1.auc@y.values[[1]]))
    
  })
  
  
  output$ANNplot <- renderPlot({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    df_dum <- dummy_cols(df[,-16])
    df_dum <- df_dum[,-1:-11]
    df_dum <- cbind(df_dum,df["Class"])
    index <- createDataPartition(df_dum$Class, p = 0.7, list = FALSE)
    train_data <- df_dum[index, ]
    test_data  <- df_dum[-index, ]
    train_downsampling <- downSample(x=train_data[,-50],y=as.factor(train_data$Class),list=FALSE)
    nnet_model <- nnet(formula=Class~.,data = train_downsampling,size=5)
    return(plot(nnet_model,rel.rsc=3))
  })
  
  output$ANNcm <- renderPlot({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    df_dum <- dummy_cols(df[,-16])
    df_dum <- df_dum[,-1:-11]
    df_dum <- cbind(df_dum,df["Class"])
    index <- createDataPartition(df_dum$Class, p = 0.7, list = FALSE)
    train_data <- df_dum[index, ]
    test_data  <- df_dum[-index, ]
    train_downsampling <- downSample(x=train_data[,-50],y=as.factor(train_data$Class),list=FALSE)
    nnet_model <- nnet(formula=Class~.,data = train_downsampling,size=5)
    pred <- predict(nnet_model,newdata = test_data,type="class")
    dtest <- tibble("target"=test_data$Class,"prediction"=pred)
    basic_table_test <- table(dtest)
    cfm2 <- tidy(basic_table_test)
    plot_confusion_matrix(cfm2, 
                          target_col = "target", 
                          prediction_col = "prediction",
                          counts_col = "n")
  })
  
  output$ANNroc <- renderPlot({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    df_dum <- dummy_cols(df[,-16])
    df_dum <- df_dum[,-1:-11]
    df_dum <- cbind(df_dum,df["Class"])
    index <- createDataPartition(df_dum$Class, p = 0.7, list = FALSE)
    train_data <- df_dum[index, ]
    test_data  <- df_dum[-index, ]
    train_downsampling <- downSample(x=train_data[,-50],y=as.factor(train_data$Class),list=FALSE)
    nnet_model <- nnet(formula=Class~.,data = train_downsampling,size=5)
    modele1.posterior<-predict(nnet_model,type='raw',newdata=test_data[,-50])
    modele1.pred<-prediction(modele1.posterior, test_data[,50])
    modele1.roc<-performance(modele1.pred, "tpr","fpr")
    plot(modele1.roc,colorize=TRUE) 
  })
  
  output$ANNauc <- renderPrint({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    df_dum <- dummy_cols(df[,-16])
    df_dum <- df_dum[,-1:-11]
    df_dum <- cbind(df_dum,df["Class"])
    index <- createDataPartition(df_dum$Class, p = 0.7, list = FALSE)
    train_data <- df_dum[index, ]
    test_data  <- df_dum[-index, ]
    train_downsampling <- downSample(x=train_data[,-50],y=as.factor(train_data$Class),list=FALSE)
    nnet_model <- nnet(formula=Class~.,data = train_downsampling,size=5)
    modele1.posterior<-predict(nnet_model,type='raw',newdata=test_data[,-50])
    modele1.pred<-prediction(modele1.posterior, test_data[,50])
    modele1.roc<-performance(modele1.pred, "tpr","fpr")
    modele1.auc<-performance(modele1.pred, "auc") 
    return(paste("AUC = ",modele1.auc@y.values[[1]])) 
  })
  
  output$TR <- renderPrint({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    index <- createDataPartition(df$Class, p = 0.7, list = FALSE)
    train_data <- df[index, ]
    test_data  <- df[-index, ]
    train_downsampling <- downSample(x=train_data[,-16],y=as.factor(train_data[,16]),list=FALSE)
    arbre.full <- rpart(Class~., data =train_downsampling, method = "class")
    return(arbre.full)
  })
  
  output$TF <- renderPlot({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    index <- createDataPartition(df$Class, p = 0.7, list = FALSE)
    train_data <- df[index, ]
    test_data  <- df[-index, ]
    train_downsampling <- downSample(x=train_data[,-16],y=as.factor(train_data[,16]),list=FALSE)
    arbre.full <- rpart(Class~., data =train_downsampling, method = "class")
    fancyRpartPlot(arbre.full,uniform=TRUE)
  })
  
  output$CARTcm <- renderPlot({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    index <- createDataPartition(df$Class, p = 0.7, list = FALSE)
    train_data <- df[index, ]
    test_data  <- df[-index, ]
    train_downsampling <- downSample(x=train_data[,-16],y=as.factor(train_data[,16]),list=FALSE)
    arbre.full <- rpart(Class~., data =train_downsampling, method = "class")
    tree_pred <- predict(arbre.full,type='class',newdata=train_downsampling[,-16])
    dtest <- tibble("target"=train_downsampling$Class,"prediction"=tree_pred)
    basic_table_test <- table(dtest)
    cfm2 <- tidy(basic_table_test)
    plot_confusion_matrix(cfm2, 
                          target_col = "target", 
                          prediction_col = "prediction",
                          counts_col = "n")
  })
  
  output$CARTroc <- renderPlot({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    index <- createDataPartition(df$Class, p = 0.7, list = FALSE)
    train_data <- df[index, ]
    test_data  <- df[-index, ]
    train_downsampling <- downSample(x=train_data[,-16],y=as.factor(train_data[,16]),list=FALSE)
    arbre.full <- rpart(Class~., data =train_downsampling, method = "class")
    modele3.posterior<-predict(arbre.full,type='prob',newdata=train_downsampling)[,2]
    modele3.pred<-prediction(modele3.posterior, train_downsampling[16])
    modele3.roc<-performance(modele3.pred, "tpr","fpr")
    plot(modele3.roc,colorize=TRUE) 
  })
  
  output$CARTauc <- renderPrint({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    index <- createDataPartition(df$Class, p = 0.7, list = FALSE)
    train_data <- df[index, ]
    test_data  <- df[-index, ]
    train_downsampling <- downSample(x=train_data[,-16],y=as.factor(train_data[,16]),list=FALSE)
    arbre.full <- rpart(Class~., data =train_downsampling, method = "class")
    modele3.posterior<-predict(arbre.full,type='prob',newdata=train_downsampling)[,2]
    modele3.pred<-prediction(modele3.posterior, train_downsampling[16])
    modele3.auc<-performance(modele3.pred, "auc")
    return(paste("AUC = ",modele3.auc@y.values[[1]]))
  })
  RF <- reactive({
    df <- filedata()
    df <- df[,c("address","Medu","Mjob","reason","higher","internet","famrel","freetime","goout","Dalc","Walc","age","studytime","failures","absences","Class")]
    df_dum <- dummy_cols(df[,-16])
    df_dum <- df_dum[,-1:-11]
    df_dum <- cbind(df_dum,df["Class"])
    index <- createDataPartition(df_dum$Class, p = 0.7, list = FALSE)
    train_data <- df_dum[index, ]
    test_data  <- df_dum[-index, ]
    train_downsampling <- downSample(x=train_data[,-50],y=as.factor(train_data$Class),list=FALSE)
    rf <- randomForest(Class ~ .,data=train_downsampling)
    return(rf)
  })


  
  
}

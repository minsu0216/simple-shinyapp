# packages
#install.packages(c("shiny","shinydashboard"))
#install.packages(c("plot3D", "dendextend", "RcolorBrewer"))
#install.packages(c("shinymanager", "keyring"))
# install.packages("NbClust")
library(shiny); library(shinydashboard)
library(plotly); library(dendextend);library(RColorBrewer)
library(shinymanager)
library(MASS); library(xgboost); library(e1071)
library(class); library(ROCR); library(pROC); library(NbClust)

#credentials <- data.frame(
#  user = c("shinymanager", "user"),
#  password = c("shinymanager@)!^", "12345"),
#  admin = c(T, F), stringsAsFactors = FALSE)


#create_db(
#  credentials_data = credentials,
#  sqlite_path = "database.sqlite", 
#)


# ui

ui <- dashboardPage(
                    
                    dashboardHeader(
                      title = "Clustering and Prediction",
                      titleWidth = 400
                    ),
                    
                    dashboardSidebar(width = 300,
                                     sidebarMenu(
                                       menuItem("Data", tabName = "data"),
                                       menuItem("Clustering", tabName = "clustering", icon = icon("chart-bar"), startExpanded = T,
                                                menuSubItem("Principle Component Analysis", tabName = "pca"),
                                                menuSubItem("Kmeans Clustering using PCs", tabName = "kmeans")
                                       ),
                                       menuItem("Prediction", tabName = "prediction", icon = icon("database"))
                                     )
                    ),
                    
                    
                    dashboardBody(
                      shinyDashboardThemes(
                        theme = "grey_light"
                      ),
                      tags$head(
                        tags$link( rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
                      tabItems(
                        tabItem(tabName = "data",
                                fluidRow(
                                  box(
                                    width = 9,
                                    title = "Use .csv File Format below for Uploading Your Data",
                                    status = "info",
                                    solidHeader = TRUE,
                                    DT::dataTableOutput("data")
                                  ),
                                  box(
                                    width = 3,
                                    status = "info",
                                    fileInput("file", "Load your data")
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "pca",
                                fluidRow(
                                  box(
                                    title = "Scatter Plot of Principle Components",
                                    solidHeader = TRUE,
                                    status = "info",
                                    width = 9, height = 500,
                                    plotlyOutput("pcaplot")
                                  ),
                                  
                                  valueBoxOutput("pcbox", width = 3),
                                  
                                  box(
                                    width = 3,
                                    status = "info",
                                    radioButtons("pcnumber1", "Choose the number of Principle Components",
                                                 c("2 Principle Components" = 2, "3 Principle Components" = 3))
                                  )
                                  
                                )
                        ),
                        
                        tabItem(tabName = "kmeans",
                                fluidRow(
                                  box(
                                    width = 9,
                                    title = "Kmeans Clustering by Principle Components",
                                    status = "info",
                                    solidHeader = TRUE,
                                    plotlyOutput("kmeansplot")
                                  ),
                                  
                                  box(
                                    width = 3,
                                    status = "info",
                                    
                                    radioButtons("pcnumber2", "Choose the number of Principle Components",
                                                 c("2 Principle Components" = 2, "3 Principle Components" = 3)),
                                    numericInput("clusters", "Number of Clusters", 2,
                                                 min = 2, max = 10)
                                  )
                                  
                                ),
                                fluidRow(
                                  box(width = 9,
                                      status = "info",
                                      DT::dataTableOutput("kmeansindex")
                                  )
                                )
                        ),  
                        
                        
                        tabItem(tabName = "prediction",
                                fluidRow(
                                  box(
                                    width = 9,
                                    title = "Prediction Result",
                                    status = "info",
                                    solidHeader = TRUE,
                                    plotlyOutput("res1")
                                  ),
                                  
                                  box(
                                    width = 3,
                                    status = "info",
                                    
                                    selectInput("model", label = "Select a Model", 
                                                choices = list("KNN" = "knn", "LDA" = "lda", 
                                                               "Logistic Regression" = "glm"), 
                                                selected = "knn"),
                                    
                                    radioButtons("pcnumber3", "Choose the number of Principle Components",
                                                 c("2 Principle Components" = 2, "3 Principle Components" = 3))
                                    
                                  )
                                  
                                ),
                                fluidRow(
                                  box(
                                    width = 6,
                                    title = "ROC Curve",
                                    status = "info",
                                    solidHeader = TRUE,
                                    plotlyOutput("res2")
                                  ),
                                  box(
                                    width = 6,
                                    status = "info",
                                    tableOutput("confmat"),
                                    DT::dataTableOutput("indexmat")
                                  )
                                )
                        )
                        
                      )
                    )
)

#ui <- secure_app(ui, enable_admin = T)

server <- function(input, output) {
  res_auth <- secure_server(
    check_credentials = check_credentials("database.sqlite")
  )

  data <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      df <- read.csv("data.csv")
      return(df)
    }
    else {
      df <- read.csv(inFile$datapath)
      df[,1] <- as.factor(df[,1])
      return(df)
    } 
  })
  
  output$data <- DT::renderDataTable({
    return(data())
  }, rownames = FALSE, options = list(scrollX = TRUE)
  )
  
  cate <- reactive({ length(levels(data()[,1])) })
  
  pca <- reactive({ prcomp(data()[,-1], center = FALSE) })
  
  output$pcbox <- renderValueBox({
    valueBox(
      paste0(round(sum(summary(pca())$importance[2,1:input$pcnumber1])*100,1), "%"), 
      "Cumulative Proportion of Total Variance", color = "black"
    )
  })
  
  output$pcaplot <- renderPlotly({
    
    qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))[1:cate()]
    x <- pca()$x[,1]
    y <- pca()$x[,2]
    z <- pca()$x[,3]
    
    if(input$pcnumber1 == 2) {
      plot_ly(x=x,y=y, color=data()[,1], colors=col_vector, type = "scatter", mode = "markers",
              hovertext = data()[,1], hoverinfo = "text") %>%
        layout(xaxis = list(title = paste("PC1(", round(summary(pca())$importance[2,1]*100,1),"%)")),
               yaxis = list(title = paste("PC2(", round(summary(pca())$importance[2,2]*100,1),"%)")))
    } else{
      plot_ly(x=x,y=y,z=z, color=data()[,1], colors=col_vector, type = "scatter3d", mode = "markers",
              hovertext = data()[,1], hoverinfo = "text", marker = list(size = 5)) %>%
        layout(scene = list(xaxis = list(title = paste("PC1(", round(summary(pca())$importance[2,1]*100,1),"%)")),
                            yaxis = list(title = paste("PC2(", round(summary(pca())$importance[2,2]*100,1),"%)")),
                            zaxis = list(title = paste("PC3(", round(summary(pca())$importance[2,3]*100,1),"%)"))))
    }
    
  })
  
  output$kmeansplot <- renderPlotly({
    
    qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    
    if(input$pcnumber2 == 2) {
      k.dat <- pca()$x[,1:2]
      kc = kmeans(k.dat, input$clusters, nstart = 100, trace=F)
      x <- k.dat[,1]
      y <- k.dat[,2]
      centers <- kc$centers[order(kc$centers[,1]),]
      
      scatter <- plot_ly(x=x,y=y, color=as.factor(kc$cluster), colors=col_vector[1:input$clusters], type = "scatter", mode = "markers",
                         hovertext = data()[,1], hoverinfo = "text") %>%
        layout(xaxis = list(title = paste("PC1(", round(summary(pca())$importance[2,1]*100,1),"%)")),
               yaxis = list(title = paste("PC2(", round(summary(pca())$importance[2,2]*100,1),"%)")))
      
      if(max(x)-min(x) < max(y)-min(y)) {
        range_x <- range(x)
        addx <- seq(range_x[1],range_x[2],length.out = length(x))
        for(k in 2:input$clusters) {
          addy <- (centers[k-1,1]-centers[k,1])/(centers[k,2]-centers[k-1,2]) * (addx-(centers[k-1,1]+centers[k,1])/2) + (centers[k-1,2]+centers[k,2])/2
          scatter <- scatter %>% add_lines(x = addx, y = addy, line = list(color = "red", dash = "dash"), showlegend = FALSE, hoverinfo = "none")
        }
      } else {
        range_y <- range(y)
        addy <- seq(range_y[1], range_y[2], length.out = length(y))
        for(k in 2:input$clusters) {
          addx <- (centers[k,2]-centers[k-1,2])/(centers[k-1,1]-centers[k,1]) * (addy-(centers[k-1,2]+centers[k,2])/2) + (centers[k-1,1]+centers[k,1])/2
          scatter <- scatter %>% add_lines(x = addx, y = addy, line = list(color = "red", dash = "dash"), showlegend = FALSE, hoverinfo = "none")
        }
      }
      
      scatter
      
    } else{
      k.dat = pca()$x[,1:3]
      kc = kmeans(k.dat, input$clusters, nstart = 100, trace=F)
      x <- k.dat[,1]
      y <- k.dat[,2]
      z <- k.dat[,3]
      
      plot_ly(x=x,y=y,z=z, color=as.factor(kc$cluster), colors=col_vector[1:input$clusters], type = "scatter3d", mode = "markers",
              hovertext = data()[,1], hoverinfo = "text", marker = list(size = 5)) %>%
        layout(scene = list(xaxis = list(title = paste("PC1(", round(summary(pca())$importance[2,1]*100,1),"%)")),
                            yaxis = list(title = paste("PC2(", round(summary(pca())$importance[2,2]*100,1),"%)")),
                            zaxis = list(title = paste("PC3(", round(summary(pca())$importance[2,3]*100,1),"%)"))))
    }
    
  })
  
  output$kmeansindex <- DT::renderDataTable({
    if(input$pcnumber2 == 2) {
      
      k.dat <- pca()$x[,1:2]
      index <- data.frame(matrix(nrow = 3, ncol = 9))
      rownames(index) <- c("Dunn", "Silhouette", "Gap"); colnames(index) <- paste("k =",2:10)
      method <- c("dunn", "silhouette", "gap")
      
      for(i in 1:3) {
        index[i,] <- NbClust(data = k.dat, method = "kmeans", min.nc = 2, max.nc = 10, index = method[i])$All.index
      }
      index
      
    } else{
      
      k.dat = pca()$x[,1:3]
      index <- data.frame(matrix(nrow = 3, ncol = 9))
      rownames(index) <- c("Dunn", "Silhouette", "Gap"); colnames(index) <- paste("k =",2:10)
      method <- c("dunn", "silhouette", "gap")
      
      for(i in 1:3) {
        index[i,] <- NbClust(data = k.dat, method = "kmeans", min.nc = 2, max.nc = 10, index = method[i])$All.index
      }
      index
      
    }
    
    
  }, options = list(lengthChange = FALSE, searching = FALSE))
  
  
  
  
  ##### Prediction #####
  pdata <- reactive({
    class <- factor(data()[,1], labels = c(0,1))
    if(input$pcnumber3 == 2) return( data.frame(class = class, pca()$x[,1:2]) )
    else return( data.frame(class = class, pca()$x[,1:3]) )
  })
  
  n <- reactive({ nrow(pdata()) })
  
  idx <- reactive({ set.seed(12345); sample(1:n(), 0.7*n(), replace = F) })
  train <- reactive({ pdata()[idx(),-1] })
  test <- reactive({pdata()[-idx(),-1] })
  train_y <- reactive({ pdata()[idx(),1] })
  test_y <- reactive({ pdata()[-idx(),1] })
  
  # knn
  k = reactive({ sqrt(dim(pdata())[1]) })
  knn_pred <- reactive({ knn(train = train(), test = test(), cl = train_y(), k = round(k()), prob = TRUE) })
  
  knn_prob <- reactive({ attr(knn_pred(), "prob") })
  knn_prob1 <- reactive({ 2*ifelse(knn_pred() == "0", 1-knn_prob(), knn_prob()) - 1 })

  pred_knn <- reactive({ prediction(knn_prob1(), test_y()) })
  pred_knn1 <- reactive({ performance(pred_knn(), "tpr", "fpr") })

  # lda
  lda_obj <- reactive({ lda(x = train(), grouping = train_y()) })
  roc.tmp1 <- reactive({ roc(train_y() ~ predict(lda_obj(), train())$posterior[,2], quiet = T) })
  cutoff1 <- reactive({ coords(roc.tmp1(), "best", ret = "threshold", best.method = "youden", transpose = T) })
  
  lda_prob1 <- reactive({ predict(lda_obj(), test())$posterior[,2] })
  lda_pred <- reactive({ factor(lda_prob1() > cutoff1(), labels = c(0,1)) })
  
  pred_lda <- reactive({ prediction(lda_prob1(), test_y()) })
  pred_lda1 <- reactive({ performance(pred_lda(), "tpr", "fpr") })
  
  # logistic
  glm_obj <- reactive({ glm(y ~., data = cbind(y = train_y(), train()), family = binomial(link = "logit")) })
  roc.tmp2 <- reactive({ roc(glm_obj()$y ~ glm_obj()$fit, quiet = T) })
  cutoff2 <- reactive({ coords(roc.tmp2(), "best", ret = "threshold", best.method = "youden", transpose = T) })
  
  glm_prob1 <- reactive({ predict(glm_obj(), test(), type = "response") })
  glm_pred <- reactive({ factor(glm_prob1() > cutoff2(), labels = c(0,1)) })
  
  pred_glm <- reactive({ prediction(glm_prob1(), test_y()) })
  pred_glm1 <- reactive({ performance(pred_glm(), "tpr", "fpr") })
  
  
  
  
  # res1
  output$res1 <- renderPlotly({
    qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    
    
    if(input$model == "knn") {
      if(input$pcnumber3 == 2) {
        
        plot_ly(x=test()[,1],y=test()[,2], color=factor(knn_pred(), labels = levels(data()[,1])), colors=col_vector[1:cate()], type = "scatter", mode = "markers",
                hovertext = factor(test_y(), labels = levels(data()[,1])), hoverinfo = "text") %>%
          layout(xaxis = list(title = paste("PC1(", round(summary(pca())$importance[2,1]*100,1),"%)")),
                 yaxis = list(title = paste("PC2(", round(summary(pca())$importance[2,2]*100,1),"%)")))
      } else{
        
        plot_ly(x=test()[,1],y=test()[,2],z=test()[,3], color=factor(knn_pred(), labels = levels(data()[,1])), colors=col_vector[1:cate()], type = "scatter3d", mode = "markers",
                hovertext = factor(test_y(), labels = levels(data()[,1])), hoverinfo = "text", marker = list(size = 5)) %>%
          layout(scene = list(xaxis = list(title = paste("PC1(", round(summary(pca())$importance[2,1]*100,1),"%)")),
                              yaxis = list(title = paste("PC2(", round(summary(pca())$importance[2,2]*100,1),"%)")),
                              zaxis = list(title = paste("PC3(", round(summary(pca())$importance[2,3]*100,1),"%)"))))
      }
    } else if(input$model == "lda") {
      if(input$pcnumber3 == 2) {
        
        plot_ly(x=test()[,1],y=test()[,2], color=factor(lda_pred(), labels = levels(data()[,1])), colors=col_vector[1:cate()], type = "scatter", mode = "markers",
                hovertext = factor(test_y(), labels = levels(data()[,1])), hoverinfo = "text") %>%
          layout(xaxis = list(title = paste("PC1(", round(summary(pca())$importance[2,1]*100,1),"%)")),
                 yaxis = list(title = paste("PC2(", round(summary(pca())$importance[2,2]*100,1),"%)")))
      } else{
        
        plot_ly(x=test()[,1],y=test()[,2],z=test()[,3], color=factor(lda_pred(), labels = levels(data()[,1])), colors=col_vector[1:cate()], type = "scatter3d", mode = "markers",
                hovertext = factor(test_y(), labels = levels(data()[,1])), hoverinfo = "text", marker = list(size = 5)) %>%
          layout(scene = list(xaxis = list(title = paste("PC1(", round(summary(pca())$importance[2,1]*100,1),"%)")),
                              yaxis = list(title = paste("PC2(", round(summary(pca())$importance[2,2]*100,1),"%)")),
                              zaxis = list(title = paste("PC3(", round(summary(pca())$importance[2,3]*100,1),"%)"))))
      }
    } else if(input$model == "glm") {
      if(input$pcnumber3 == 2) {
        
        plot_ly(x=test()[,1],y=test()[,2], color=factor(glm_pred(), labels = levels(data()[,1])), colors=col_vector[1:cate()], type = "scatter", mode = "markers",
                hovertext = factor(test_y(), labels = levels(data()[,1])), hoverinfo = "text") %>%
          layout(xaxis = list(title = paste("PC1(", round(summary(pca())$importance[2,1]*100,1),"%)")),
                 yaxis = list(title = paste("PC2(", round(summary(pca())$importance[2,2]*100,1),"%)")))
      } else{
        
        plot_ly(x=test()[,1],y=test()[,2],z=test()[,3], color=factor(glm_pred(), labels = levels(data()[,1])), colors=col_vector[1:cate()], type = "scatter3d", mode = "markers",
                hovertext = factor(test_y(), labels = levels(data()[,1])), hoverinfo = "text", marker = list(size = 5)) %>%
          layout(scene = list(xaxis = list(title = paste("PC1(", round(summary(pca())$importance[2,1]*100,1),"%)")),
                              yaxis = list(title = paste("PC2(", round(summary(pca())$importance[2,2]*100,1),"%)")),
                              zaxis = list(title = paste("PC3(", round(summary(pca())$importance[2,3]*100,1),"%)"))))
      }
    }
    
    
  })

  # roc curve(res2)
  output$res2 <- renderPlotly({
    if(input$model == "knn") {
      plot_ly(x = pred_knn1()@x.values[[1]], y = pred_knn1()@y.values[[1]], mode = "lines", type = "scatter") %>%
        layout(
          title = "ROC curve",
          xaxis = list(title = "1 - Specificity"),
          yaxis = list(title = "Sensitivity")
        )
    } else if(input$model == "lda") {
      plot_ly(x = pred_lda1()@x.values[[1]], y = pred_lda1()@y.values[[1]], mode = "lines", type = "scatter") %>%
        layout(
          title = "ROC curve",
          xaxis = list(title = "1 - Specificity"),
          yaxis = list(title = "Sensitivity")
        )
    } else if(input$model == "glm") {
      plot_ly(x = pred_glm1()@x.values[[1]], y = pred_glm1()@y.values[[1]], mode = "lines", type = "scatter") %>%
        layout(
          title = "ROC curve",
          xaxis = list(title = "1 - Specificity"),
          yaxis = list(title = "Sensitivity")
        )
    }
    
  })
  
  # confmat
  resmat <- function(pred, y) {
    cf <- table(factor(pred, labels = levels(data()[,1])), 
                factor(y, labels = levels(data()[,1])))
    acc <- sum(diag(cf))/sum(cf)
    sens <- cf[2,2]/sum(cf[,2])
    spec <- cf[1,1]/sum(cf[,1])

    index <- data.frame("Accuracy"= acc, "Sensitivity" = sens, "Specificity" = spec)
    list(cf, index)
  }
  output$confmat <- renderTable({
    
    if(input$model == "knn") {
      as.data.frame.matrix(resmat(knn_pred(), test_y())[[1]])
    } else if(input$model == "lda") {
      as.data.frame.matrix(resmat(lda_pred(), test_y())[[1]])
    } else if(input$model == "glm") {
      as.data.frame.matrix(resmat(glm_pred(), test_y())[[1]])
    }
    
  }, rownames = TRUE
  )
  
  # idexmat
  output$indexmat <- DT::renderDataTable({
    
    if(input$model == "knn") {
      auc <- roc(test_y(), knn_prob1(), quiet = T)$auc
      round( cbind(resmat(knn_pred(), test_y())[[2]], AUC = auc), 4)
    } else if(input$model == "lda") {
      auc <- roc(test_y(), lda_prob1(), quiet = T)$auc
      round( cbind(resmat(lda_pred(), test_y())[[2]], AUC = auc), 4 )
    } else if(input$model == "glm") {
      auc <- roc(test_y(), glm_prob1(), quiet = T)$auc
      round( cbind(resmat(glm_pred(), test_y())[[2]], AUC = auc), 4 )
    } 
    
  }, rownames = FALSE, options = list(lengthChange = FALSE, searching = FALSE))

}
shinyApp(ui, server)

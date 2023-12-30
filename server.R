library(shiny)
library(shiny)
library(psych)
library(shinythemes)
library(knitr)


shinyServer(function(input,output, session){


  output$table <- renderTable({
    file1 <- input$file
    if(is.null(file1)){return()}
    x<-read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    total.score<- rowSums(x)
    x1<- cbind(x,total.score)
    x1
  })

  # summary
  output$sum <- shiny::renderTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    y <- read.csv(inFile$datapath, header = input$header,
                  sep = input$sep, quote = input$quote)
    total.score<- rowSums(y)
    y1<- cbind(y,total.score)
    Variable<- colnames(y1)
    y3<- psych::describe(y1)
    y4<- as.data.frame(y3)
    y5<-cbind(Variable,y4)
    y5

  })

  # Histograms
  output$hist<-shiny::renderPlot({
    inFile2 <- input$file
    if (is.null(inFile2))
      return(NULL)
    a1 <- read.csv(file= inFile2$datapath, header = input$header,
                   sep = input$sep, quote = input$quote)
    total.score<- rowSums(a1)
    a2<- cbind(a1,total.score)

    dd<- function(a0){
      nrt<-ncol(a0)
      par(mfrow = c(5,4), oma =c(1,1,1,1), mar=c(1.5,1.5,1.5,1.5))
      loop<- 1:nrt
      c<- colnames(a0)
      for (i in loop){
        x<-a0[,i]
        hist(x, main = paste("Histogram of", c[i]),col="grey",
             breaks = 15)
      }
    }
    dd(a2)
  })

  #Q-Q plot
  output$qq<- shiny::renderPlot({
    qq <- input$file
    if (is.null(qq))
      return(NULL)
    qq1 <- read.csv(file= qq$datapath, header = input$header,
                    sep = input$sep, quote = input$quote)
    total.score<- rowSums(qq1)
    qq2<- cbind(qq1,total.score)

    dq<- function(q0){
      n<-ncol(q0)
      par(mfrow = c(5,4), oma =c(1,1,1,1), mar=c(1.5,1.5,1.5,1.5))
      l<- 1:n
      c<- colnames(q0)
      for (i in l){
        x<-q0[,i]
        qqnorm(x, main = paste("Normal Q-Q plot of", c[i]), col ="grey",
               )
      }
    }
    dq(qq2)
  })

  # corr
  output$corr<- shiny::renderPlot({
    inFile3 <- input$file
    if (is.null(inFile3))
      return(NULL)
    a0 <- read.csv(file= inFile3$datapath, header = input$header,
                  sep = input$sep, quote = input$quote)
    psych::cor.plot(a0)
  })

 ############################################################

  # Score calculations

  data <- reactive({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    s<-read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    s[is.na(s)]<-0
    total.score<- rowSums(s)
    s1<- cbind(s,total.score)

  })

  observeEvent(data(), {
    updateSelectInput(session, "col", choices = names(data()))
  })

###############################################################
  #score calculation
  output$score <- shiny::renderTable({
    Values<- data()[[input$col]]
    Percentile.Rank<- round(dplyr::percent_rank(Values)*100, 1)
    Z.value <- (Values-mean(Values))/sd(Values)
    T.value<- Z.value*10 +50
    Z.normalized.score<- round(qnorm(Percentile.Rank/100),2)
    T.normalized.score<- Z.normalized.score*10 +50
    stanines<-5+Z.value*2
    stanines[stanines<1]<-1
    stanines[stanines>9] <-9
    score <- cbind(Values,Percentile.Rank,Z.value, T.value,Z.normalized.score ,T.normalized.score,stanines)
    score
  })

  # histogram

  #output$hist <- shiny::renderPlot({
   # hist(data()[[input$col]])

    #})

################ALPHA#############################
  #alpha

  output$alpha<- shiny::renderPrint({

    input$calculate

    alpha1 <- input$file
    if (is.null(alpha1))
      return(NULL)
    a1 <- read.csv(file= alpha1$datapath, header = input$header,
                   sep = input$sep, quote = input$quote)
    a2<- psych::alpha(a1, check.keys = TRUE)
    #a3<- a2$total
    #a4<- knitr::kable(a3,"simple", caption = "Chronbach's Alpha estimates")
    isolate(a2)

  })

  # itemdrops
  #output$itemdrops<- shiny::renderPrint({
   # id <- input$file
    #if (is.null(id))
     # return(NULL)
    #id1 <- read.csv(file= id$datapath, header = input$header,
     #              sep = input$sep, quote = input$quote)
    #id2<- psych::alpha(id1, check.keys = TRUE)
    #id3<- id1$alpha.drop
    #id4<- knitr::kable(id3,"simple", caption = "Reliability estimates if an item is dropped")
    #id4
  #})


  #itemstat

############################################
  #Omega
  output$omega<- shiny::renderPrint({
    omega1 <- input$file
    if (is.null(omega1))
      return(NULL)
    o1 <- read.csv(file= omega1$datapath, header = input$header,
                   sep = input$sep, quote = input$quote)
    o2<- psych::omega(o1, check.keys = TRUE)
    o2
  })
  #############################################
  output$ic<- shiny::renderTable({
    ic <- input$file
    if (is.null(ic))
      return(NULL)
    ic0 <- read.csv(file= ic$datapath, header = input$header,
                   sep = input$sep, quote = input$quote)
    ic0<-subset(ic0, complete.cases(ic0))
    ic1<- round(cor(ic0), digits = 2)
    ic1
  })
  ###################################################
  
  output$sp<- shiny::renderPrint({
    sp <- input$file
    if (is.null(sp))
      return(NULL)
    sp0 <- read.csv(file= sp$datapath, header = input$header,
                   sep = input$sep, quote = input$quote)
    sp0<-subset(sp0, complete.cases(sp0))
    sp1<- cor(sp0)
    sp2<-psych::splitHalf(sp1,raw=FALSE,brute=FALSE,n.sample=10000,covar=FALSE,check.keys=TRUE,
                          key=NULL,ci=.05,use="pairwise")
    sp2
    
  })
  
  ############################################
  output$is<- shiny::renderTable({
    
    is <- input$file
    if (is.null(is))
      return(NULL)
    is0 <- read.csv(file= is$datapath, header = input$header,
                   sep = input$sep, quote = input$quote)
    is0<-subset(is0, complete.cases(is0))
    is1<- psych::alpha(is0, check.keys = TRUE)
    
  
  # combine item results and export them:
  is2<- cbind.data.frame(rownames(is1$item.stats), 
                                 is1$item.stats$mean,
                                 is1$item.stats$r.drop)
  # give the item_stats object clearer names:
  names(is2) <- c("item_name", "average_score", "item_total_correlation")
  is2
  
  
  })
  
  
###############################################
  #factor analysis
###############################################
  dataf <- reactive({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    s<-read.csv(inFile$datapath, header = input$header,
                sep = input$sep, quote = input$quote)
    s[is.na(s)]<-0
    s
  })
  
  #-----------
  observeEvent(dataf(), {
    updateNumericInput(session, "nfactor")
  })
  
  
  output$fa<- shiny:: renderPrint({
    psych::fa(dataf(), nfactors = input$nfactor, rotate = input$rotate)

  })

   #output$cor1<- shiny:: renderplot({
    # fac2<- input$file
     #if(is.null(fac2)) return(NULL)
     #f11 <- read.csv(file= fac2$datapath, header = input$header,
                    #sep = input$sep, quote = input$quote)
     #psych::cor.plot(f11)

   #})
######################
  #last bracket
  })




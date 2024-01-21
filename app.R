##########         ########## 
########## LIBRARY ##########
##########         ##########

library(shiny)
library(shinythemes)
library(mvtnorm)
library(quadprog)
library(readr)
library(DT)
library(data.table)
library(dplyr)
library(rsconnect)

##########.   ############
########## UI ############
##########.   ############

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("DEA-MVEP without Short-Selling"),
  navbarPage("by: Agy Aldira",
             tabPanel("DEA: Input Data",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Upload CSV File:", accept = c(".csv")),
                          uiOutput("input_select"),
                          uiOutput("output_select"),
                        ),
                        mainPanel(
                          verbatimTextOutput("dea_result"),
                          DTOutput("uploaded_data")
                        )
                      )
             ),
            
             tabPanel(
               "DEA: Efficiency Results",
               shiny::sidebarLayout(
                 shiny::sidebarPanel(
                   
                   shiny::selectInput(
                     inputId = "ID_choose",
                     label = "Select the Identification column",
                     choices = ""
                   ),
                   shiny::selectInput(
                     inputId = "RTS_choose",
                     label = "Select the Returns to Scale assumption",
                     choices = c("CRS", "VRS", "IRS", "DRS", "ADD", "FDH")
                   ),
                   shiny::selectInput(
                     inputId = "orientation_choose",
                     label = "Select the orientation",
                     choices = c("Input"="input", "Output"="output")
                   ),
                   shiny::tags$h3("DEA Summary"),
                   shiny::verbatimTextOutput("summary"),
                ),
                 shiny::mainPanel(
                   shinycssloaders::withSpinner(DT::dataTableOutput("eff_results1"), color = "#324C63"),
                   shiny::helpText("Click on the download button to get a csv file of the results"),
                   shiny::downloadButton(outputId = "dbtn1", label = "download") 
                 )
                )
               ),
      
             tabPanel("MVEP: Input Data",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("data", "Upload CSV File:", accept = c(".csv"))
                        ),
                        mainPanel(
                          uiOutput("data_display_selector"),
                          uiOutput("mvep_output"),
                          verbatimTextOutput("statdes")
                        )
                      )
             ),
             
             tabPanel("MVEP: Plot Return",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("Return", "Select Stock Returns:", choices = c("Aset 1" = 1, "Aset 2" = 2, "Aset 3" = 3, "Aset 4" = 4)),
                          selectInput("warna", "Select Plot Color:", choices = c("Red" = "red", "Blue" = "blue", "Black" = "black")),
                          selectInput("tipe", "Select Plot Type:", choices = c("Line" = "l", "Dot" = "p", "Line with Dot" = "b"))
                        ),
                        mainPanel(plotOutput("plot"), verbatimTextOutput("FluktuasiHarga"))
                      )
             ),
             
             tabPanel("MVEP: Normality Test",
                      mainPanel(
                        tabsetPanel(type = "pills", id = "navbar",
                                    tabPanel("Multivariate Normality Assumption",
                                             tags$b("Critical Value"),
                                             verbatimTextOutput("tentuan"),
                                             tags$b("Multivariate Normality Test"),
                                             verbatimTextOutput("normal"),
                                             tags$b("Conclusion"),
                                             verbatimTextOutput("simpul")
                                    )
                        )
                      )
             ),
             
             tabPanel("MVEP: Stock Portofolio",
                      mainPanel(
                        tabsetPanel(type = "pills", id = "navbar",
                                    tabPanel("Portfolio Weight Percentage",
                                             verbatimTextOutput("bobot"),
                                             tags$b("Conclusion"),
                                             verbatimTextOutput("maksud")
                                    ),
                        )
                      )
             ),
             
             tabPanel("VaR: Portfolio Risk",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Investment Data"),
                          textInput("modal", "Your Capital"),
                          textInput("hp", "Holding Period (month)"),
                          actionButton("hitung", 'Calculate', class = "btn-success")
                        ),
                        mainPanel(
                          tabsetPanel(type = "pills", id = "navbar",
                                      tabPanel("Value at Risk (VaR) Calculation",
                                               tags$b("VaR:"),
                                               verbatimTextOutput("var1"),
                                               tags$b("%VaR:"),
                                               verbatimTextOutput("var2"),
                                               tags$b("Conclusion"),
                                               verbatimTextOutput("simpul2")
                                      )
                          )
                        )
                      )
             )
  )
)

################        #################
################ SERVER #################
################        #################

server <- function(input, output, session){
  data <- reactive({
    req(input$file1)
    data <- data.table::fread(input$file1$datapath)
    
    updateSelectInput(
      session = session,
      inputId = "ID_choose",
      choices = names(data)
    )
    return(data)
  })
  
  output$uploaded_data <- renderDT({
    datatable(data(), options = list(scrollX = TRUE, scrollY = TRUE))
  })
  
  output$input_select <- renderUI({
    checkboxGroupInput("input_select", "Select the Input Variables", choices = NULL)
  })
  
  output$output_select <- renderUI({
    checkboxGroupInput("output_select", "Select the Output Variables", choices = NULL)
  })
  
  observe({
    data_dea <- data()
    updateCheckboxGroupInput(
      session,
      "input_select",
      choices = names(data_dea %>% select_if(is.numeric)),
      selected = NULL
    )
    
    updateCheckboxGroupInput(
      session,
      "output_select",
      choices = names(data_dea %>% select_if(is.numeric)),
      selected = NULL
    )
  })
  
  scores <- reactive({
    df <- data()
    inputs <- df %>% dplyr::select(input$input_select)
    outputs <- df %>% dplyr::select(input$output_select)
    orientation <- switch(input$orientation_choose, "input" = "in" , "output" = "out")
    r_eff <- Benchmarking::dea(
      X = data.matrix(inputs),
      Y = data.matrix(outputs),
      RTS = input$RTS_choose,
      ORIENTATION = orientation
    )
    return(r_eff)
  })
  
  dea_results <- reactive({
    req(
      input$input_select,
      input$output_select,
      input$orientation_choose,
      input$RTS_choose,
      input$ID_choose
    )
    
    df <- data()
    r_eff2 <- scores()
    id <- df %>% dplyr::select(input$ID_choose)
    results <- tibble(score = r_eff2$eff)
    peers <- Benchmarking::peers(r_eff2, NAMES = df %>% dplyr::pull(input$ID_choose))
    peers <- as.data.frame(peers)
    results <- cbind(id, results, peers)
    results <- results %>% dplyr::arrange(dplyr::desc(score))
    results <-  results %>% dplyr::mutate(
      score =  sprintf('%.4f', score)
    )
    return(results)
  })

  output$eff_results1 <- DT::renderDataTable({
    DT::datatable(dea_results(), rownames = FALSE, class = "compact") %>% 
      DT::formatStyle(
        'score',
        target = 'row',
        backgroundColor = DT::styleEqual(1, 'lightgreen')
      )
  })

  output$dbtn1 <- shiny::downloadHandler(
    filename = function() {
      paste('efficiency-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      readr::write_csv(dea_results(), path = file)
    }
  )
  
  output$summary <- shiny::renderPrint({
    shiny::req(
      input$input_select,
      input$output_select,
      input$orientation_choose,
      input$RTS_choose,
      input$ID_choose
    )
    
    print("Debug Information:")
    print(class(data()))
    print(class(dea_results()))

    r_eff2 <- dea_results()
    print(class(r_eff2))
    print(str(r_eff2))
    print("Summary Information:")
    summary(r_eff2)
    return("Summary Printed Successfully!")
  })
  
  output$data_display_selector <- renderUI({
    selectInput("data_display", "Choose Display", choices = c("datatable", "renderTable"), selected = "datatable")
  })
  
  output$mvep_output <- renderUI({
    data_display <- input$data_display
    if (data_display == "datatable") {
      renderDT({
        datainput <- input$data
        if (is.null(datainput)) {
          return()
        }
        datapakai <- read.csv(datainput$datapath, header = TRUE)
        datatable(datapakai, options = list(scrollX = TRUE, scrollY = TRUE), class = "compact", rownames = FALSE)
      })
    } else {
      renderTable({
        datainput <- input$data
        if (is.null(datainput)) {
          return()
        }
        datapakai <- read.csv(datainput$datapath, header = TRUE)
        datapakai
      })
    }
  })
  
  output$statdes<-renderPrint({
    datainput=input$data
    if (is.null(datainput)){return()}
    datapakai=read.csv(datainput$datapath,header = TRUE)
    summary(datapakai)
  })
  
  selectInput("data_display", "Choose Display", choices = c("datatable", "renderTable"), selected = "datatable")
  output$tabel <- renderUI({
    data_display <- input$data_display
    if (data_display == "datatable") {
      renderDT({
        datainput <- input$data
        if (is.null(datainput)) {
          return()
        }
        datapakai <- read.csv(datainput$datapath, header = TRUE)
        datatable(datapakai, options = list(scrollX = TRUE, scrollY = TRUE), 
                  caption = "Uploaded Data", class = "compact", 
                  rownames = FALSE)
      })
    } else {
      renderTable({
        datainput <- input$data
        if (is.null(datainput)) {
          return()
        }
        datapakai <- read.csv(datainput$datapath, header = TRUE)
        datapakai
      })
    }
  })
  
  observe({
    datainput <- input$data
    if (!is.null(datainput)) {
      datapakai <- read.csv(datainput$datapath, header = TRUE)
      kolom_names <- colnames(datapakai)
      updateRadioButtons(
        session,
        "Return",
        label = "Select Stock Return:",
        choices = setNames(1:length(kolom_names), kolom_names),
        selected = 1
      )
    }
  })
  
  output$plot <- renderPlot({
    datainput <- input$data
    if (is.null(datainput)) {
      return()
    }
    datapakai <- read.csv(datainput$datapath, header = TRUE)
    kolom <- as.numeric(input$Return)
    nilaireturn <- datapakai[, kolom]
    nama_kolom <- names(datapakai)[kolom]
    plot(nilaireturn, type = input$tipe, col = input$warna, main = paste("Stock Return -", nama_kolom))
  })
  
  output$FluktuasiHarga<-renderPrint({
    datainput=input$data
    if (is.null(datainput)){return()}
    datapakai=read.csv(datainput$datapath,header = TRUE)
    kolom=as.numeric(input$Return)
    nilaireturn=datapakai[,kolom]
    cat("Descriptive Statistics:\n")
    print(summary(nilaireturn))
  })
  output$tentuan<-renderPrint({
    cat("Asset returns have a multivariate normal distribution if the test results show that:\n 
the p-value is > 0.05")
  })
  output$normal<-renderPrint({
    mnorm.test<-function(x)
    {
      rata2 <- apply(x, 2, mean)
      mcov <- var(x)
      ds <- sort(mahalanobis(x, center = rata2,cov = mcov))
      n <- length(ds)
      p <- (1:n - 0.5)/n
      chi <- qchisq(p, df = ncol(x))
      return(ks.test(ds,'pchisq',df = ncol(x), 
                     alternative='g'))
    }
    datainput=input$data
    if (is.null(datainput)){return()}
    datapakai=read.csv(datainput$datapath,header = TRUE)
    e=datapakai[,1]
    f=datapakai[,2]
    g=datapakai[,3]
    h=datapakai[,4]
    test <- as.matrix(datapakai)
    mnorm.test(test)
  })
  output$simpul<-renderPrint({
    mnorm.test<-function(x)
    {
      rata2 <- apply(x, 2, mean)
      mcov <- var(x)
      ds <- sort(mahalanobis(x, center = rata2,cov = mcov))
      n <- length(ds)
      p <- (1:n - 0.5)/n
      chi <- qchisq(p, df = ncol(x))
      return(ks.test(ds,'pchisq',df = ncol(x), 
                     alternative='g'))
    }
    datainput=input$data
    if (is.null(datainput)){return()}
    datapakai=read.csv(datainput$datapath,header = TRUE)
    e=datapakai[,1]
    f=datapakai[,2]
    g=datapakai[,3]
    h=datapakai[,4]
    test=matrix(c(e,f,g,h),ncol=4)
    L <- mnorm.test(test)
    hasil=if(L$p.value>=0.05){
      cat("p-value > 0.05.\n 
So it can be concluded that the return has a multivariate normal distribution,
then the data meets the assumptions for forming a Mean-Variance model portfolio")
    }else{
      cat("p-value < 0.05.\n 
So it can be concluded that returns are not normally distributed in a multivariate way,
so the data does not meet the assumptions for forming a Mean-Variance model portfolio")
    }
    
  })
  output$bobot <- renderPrint({
    datainput <- input$data
    if (is.null(datainput)) {
      return()
    }
    datapakai <- read.csv(datainput$datapath, header = TRUE)
    
    matrixCov <- function(X) {
      n <- dim(X)[1]
      ones <- rep(1, n)
      C <- diag(1, n) - ones %*% t(ones) / n
      Xc <- C %*% X
      matCov <- t(Xc) %*% Xc / (n - 1)
      return(matCov)
    }
    
    mat.data <- as.matrix(datapakai)
    sigma.mat <- matrixCov(mat.data)
    D.mat <- 2 * sigma.mat
    d.vec <- rep(0, ncol(mat.data))
    A.mat <- cbind(rep(1, ncol(mat.data)), diag(ncol(mat.data)))
    b.vec <- c(1, rep(0, ncol(mat.data)))
    
    qp.out <- solve.QP(Dmat = D.mat, dvec = d.vec, Amat = A.mat, bvec = b.vec, meq = 1)
    Percentage <- 100 * t(t(qp.out$solution))

    result_df <- data.frame(Asset = colnames(datapakai), Percentage)
    return(result_df)
  })

  output$maksud <- renderPrint({
    datainput <- input$data
    if (is.null(datainput)) {
      return()
    }
    datapakai <- read.csv(datainput$datapath, header = TRUE)

    aset_names <- colnames(datapakai)
    output_names <- paste("aset", seq_along(aset_names), sep = " ")
    colnames(datapakai) <- output_names
    
    matrixCov <- function(X) {
      n <- dim(X)[1]
      ones <- rep(1, n)
      C <- diag(1, n) - ones %*% t(ones) / n
      Xc <- C %*% X
      matCov <- t(Xc) %*% Xc / (n - 1)
      return(matCov)
    }
    
    mat.data <- as.matrix(datapakai)
    sigma.mat <- matrixCov(mat.data)
    D.mat <- 2 * sigma.mat
    d.vec <- rep(0, ncol(mat.data))
    A.mat <- cbind(rep(1, ncol(mat.data)), diag(ncol(mat.data)))
    b.vec <- c(1, rep(0, ncol(mat.data)))
    
    qp.out <- solve.QP(Dmat = D.mat, dvec = d.vec, Amat = A.mat, bvec = b.vec, meq = 1)
    Percentage <- 100 * t(t(qp.out$solution))
    
    cat("To get an optimal portfolio, you need to invest your capital in the amount of:\n")
    for (i in seq_along(aset_names)) {
      cat(Percentage[i, 1], "% on", aset_names[i], ",\n")
    }
  })

  observeEvent(input$hitung,{
    output$var1<-renderPrint({
      datainput=input$data
      if (is.null(datainput)){return()}
      datapakai=read.csv(datainput$datapath,header = TRUE)
      matrixCov <- function(X){ 
        n <- dim(X)[1]
        ones <- rep(1,n) 
        C <- diag(1,n) - ones %*% t(ones) / n 
        Xc <- C %*% X 
        matCov <- t(Xc) %*% Xc / (n-1) 
        return(matCov)
      }
      e=datapakai[,1]
      f=datapakai[,2]
      g=datapakai[,3]
      h=datapakai[,4]
      mat.data=cbind(e,f,g,h)
      sigma.mat=matrixCov(mat.data)
      D.mat = 2*sigma.mat
      d.vec = rep(0, 4) 
      A.mat = cbind(rep(1,4), diag(4)) 
      b.vec = c(1, rep(0,4))
      qp.out = solve.QP(Dmat=D.mat, dvec=d.vec,Amat=A.mat, 
                        bvec=b.vec, meq=1)
      bbt=t(t(qp.out$solution))
      rtn=mat.data%*%bbt
      modal<-input$modal
      hp<-input$hp
      modal=as.numeric(modal)
      hp=as.numeric(hp)
      data_urut=sort(rtn, decreasing = FALSE)
      q=quantile(rtn,0.05,na.rm=TRUE)
      VaR_HS=-(modal*q*sqrt(hp))
      print(VaR_HS)
    })
    output$var2<-renderPrint({
      datainput=input$data
      if (is.null(datainput)){return()}
      datapakai=read.csv(datainput$datapath,header = TRUE)
      matrixCov <- function(X){ 
        n <- dim(X)[1] 
        ones <- rep(1,n)
        C <- diag(1,n) - ones %*% t(ones) / n 
        Xc <- C %*% X 
        matCov <- t(Xc) %*% Xc / (n-1) 
        return(matCov)
      }
      e=datapakai[,1]
      f=datapakai[,2]
      g=datapakai[,3]
      h=datapakai[,4]
      mat.data=cbind(e,f,g,h)
      sigma.mat=matrixCov(mat.data)
      D.mat = 2*sigma.mat
      d.vec = rep(0, 4) 
      A.mat = cbind(rep(1,4), diag(4)) 
      b.vec = c(1, rep(0,4))
      qp.out = solve.QP(Dmat=D.mat, dvec=d.vec,Amat=A.mat, 
                        bvec=b.vec, meq=1)
      bbt=t(t(qp.out$solution))
      rtn=mat.data%*%bbt
      hp<-input$hp
      hp=as.numeric(hp)
      data_urut=sort(rtn, decreasing = FALSE)
      q=quantile(rtn,0.05,na.rm=TRUE)
      VaR_persen=-(1*q*sqrt(hp)*100)
      cat(VaR_persen,"%")
    })
    output$simpul2<-renderPrint({
      datainput=input$data
      if (is.null(datainput))return()
      datapakai=read.csv(datainput$datapath,header = TRUE)
      matrixCov <- function(X){ 
        n <- dim(X)[1] 
        ones <- rep(1,n)
        C <- diag(1,n) - ones %*% t(ones) / n
        Xc <- C %*% X 
        matCov <- t(Xc) %*% Xc / (n-1) 
        return(matCov)
      }
      e=datapakai[,1]
      f=datapakai[,2]
      g=datapakai[,3]
      h=datapakai[,4]
      mat.data=cbind(e,f,g,h)
      sigma.mat=matrixCov(mat.data)
      D.mat = 2*sigma.mat
      d.vec = rep(0, 4) 
      A.mat = cbind(rep(1,4), diag(4)) 
      b.vec = c(1, rep(0,4))
      qp.out = solve.QP(Dmat=D.mat, dvec=d.vec,Amat=A.mat, 
                        bvec=b.vec, meq=1)
      bbt=t(t(qp.out$solution))
      rtn=mat.data%*%bbt
      modal<-input$modal
      hp<-input$hp
      modal=as.numeric(modal)
      dana=format(modal, scientific = FALSE)
      hp=as.numeric(hp)
      data_urut=sort(rtn, decreasing = FALSE)
      q=quantile(rtn,0.05,na.rm=TRUE)
      67
      VaR_HS=-(modal*q*sqrt(hp))
      VaR_persen=-(1*q*sqrt(hp)*100)
      cat("If you invest Rp",dana,"within a period of",hp,"months, with an error rate of 5%, 
the maximum possible loss risk is Rp",VaR_HS,"or",VaR_persen,"%")
    })
  })
  
  read_data <- function(file_path) {
    if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
      return(read.csv(file_path))
    } else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
      return(read_xlsx(file_path))
    } else {
      stop("Unsupported file format")
    }
  }}

shinyApp(ui=ui,server=server)

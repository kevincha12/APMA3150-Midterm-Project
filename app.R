library(shiny)
library(gridExtra)

uploaded_csv = data.frame()

if(interactive()) {
  ui = fluidPage(
    titlePanel("Improved Interactive Hypothesis Testing App"),
    titlePanel(h4("Created by Kevin Cha, Qixiang Gao, Prabhnoor Kaur")),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "hypothesis", "Choose Hypothesis Test:",
          c("One-Sample t-Test", "Two-Sample t-Test", "Proportion Test")
        ),
        conditionalPanel(
          condition = "input.hypothesis == 'One-Sample t-Test'",
          radioButtons(
            "inputFiles", "Select data input option:",
            c("Manual Input", "Sample Data", "CSV Upload (needs headers)")
          ),
          conditionalPanel(
            condition = "input.inputFiles == 'Manual Input'",
            textInput(
              "singleSampleData", "Enter Sample Data (comma-separated):", "1, 2, 4, 8, 9"
            )
          ),
          conditionalPanel(
            condition = "input.inputFiles == 'Sample Data'",
            selectInput(
              "singleSampleColumn", "Choose column:",
              colnames(mtcars)
            )
          ),
          conditionalPanel(
            condition = "input.inputFiles == 'CSV Upload (needs headers)'",
            fileInput(
              "singleSampleFile", "Upload File (csv)", accept=".csv", placeholder="Currently using sample..."
            ),
            uiOutput("singlePlaceholder")
          ),
          textInput(
            "mu", "Population Mean (H0):", 20
          ),
          textInput(
            "alphaOneSample", "Significance Level (α):", 0.05
          ),
          selectInput(
            "graphSelOneSamp", "Graph Selection:",
            c("Histogram", "Boxplot")
          )
        ),
        conditionalPanel(
          condition = "input.hypothesis == 'Two-Sample t-Test'",
          radioButtons(
            "inputFiles", "Select data input option:",
            c("Manual Input", "Sample Data", "CSV Upload (needs headers)")
          ),
          conditionalPanel(
            condition = "input.inputFiles == 'Manual Input'",
            textInput(
              "sampleData1", "Enter Sample 1 Data (comma-separated):", "1, 2, 4, 8, 9"
            ),
            textInput(
              "sampleData2", "Enter Sample 2 Data (comma-separated):", "5, 8, 15, 19, 23"
            )
          ),
          conditionalPanel(
            condition = "input.inputFiles == 'Sample Data'",
            selectInput(
              "twoSampleColumnOne", "Choose column:",
              colnames(mtcars)
            ),
            selectInput(
              "twoSampleColumnTwo", "Choose column:",
              colnames(mtcars)
            )
          ),
          conditionalPanel(
            condition = "input.inputFiles == 'CSV Upload (needs headers)'",
            fileInput(
              "twoSampleFile", "Upload File (csv)", accept=".csv", placeholder="Currently using sample..."
            ),
            uiOutput("multiPlaceholderOne"),
            uiOutput("multiPlaceholderTwo")
          ),
          textInput(
            "alphaTwoSample", "Significance Level (α):", 0.05
          ),
          selectInput(
            "graphSelTwoSamp", "Graph Selection:",
            c("Histogram", "Boxplot")
          )
        ),
        conditionalPanel(
          condition = "input.hypothesis == 'Proportion Test'",
          textInput(
            "numSuccesses", "Number of Successes", 8
          ),
          textInput(
            "numTrials", "Number of Trials", 14
          ),
          textInput(
            "propHypo", "Null Proportion (H0):", 0.5
          ),
          textInput(
            "alphaProp", "Significance Level (α):", 0.05
          )
        ),
        actionButton("runButton", "Run Test", class = "btn-primary")
      ),
      mainPanel(
        verbatimTextOutput("results"),
        plotOutput("plot")
      )
    )
  )
}

server = function(input, output) {
  observeEvent(input$singleSampleFile, {
  output$singlePlaceholder = renderUI({
      file = input$singleSampleFile
      uploaded_csv = read.csv(file$datapath, header=TRUE)
      choices=colnames(uploaded_csv[unlist(lapply(uploaded_csv, is.numeric))])
      selectInput("singleUploadedColumn", "Select Column:", choices=choices)
  })})
  observeEvent(input$twoSampleFile, {
    file = input$twoSampleFile
    uploaded_csv = read.csv(file$datapath, header=TRUE)
    choices=colnames(uploaded_csv[unlist(lapply(uploaded_csv, is.numeric))])
    output$multiPlaceholderOne = renderUI({
      selectInput("multiUploadedColumnOne", "Select First Column:", choices=choices)
    })
    output$multiPlaceholderTwo = renderUI({
      selectInput("multiUploadedColumnTwo", "Select Second Column:", choices=choices)
    })})
  output$results = renderPrint({
    if(input$hypothesis == "One-Sample t-Test") {
      if(input$inputFiles == "CSV Upload (needs headers)" & is.null(input$singleSampleFile)){
        return("Please upload a CSV to continue!")
      } else if(input$inputFiles == "Manual Input") {
        sample = unlist(strsplit(input$singleSampleData, '\\s*,\\s*'))
        sample = as.numeric(sample)
      } else if(input$inputFiles == "Sample Data") {
        sample = mtcars[input$singleSampleColumn]
      } else {
        file = input$singleSampleFile
        uploaded_csv = read.csv(file$datapath, header=TRUE)
        sample = uploaded_csv[input$singleUploadedColumn]
      }
      mu = as.numeric(input$mu)
      alpha = as.numeric(input$alphaOneSample)
      results = t.test(sample, mu=mu, conf.level=1-alpha)
      results
    }
    else if(input$hypothesis == "Two-Sample t-Test") {
      if(input$inputFiles == "CSV Upload (needs headers)" & is.null(input$twoSampleFile)){
        return("Please upload a CSV to continue!")
      } else if(input$inputFiles == "Manual Input") {
        sample1 = unlist(strsplit(input$sampleData1, '\\s*,\\s*'))
        sample1 = as.numeric(sample1)
        sample2 = unlist(strsplit(input$sampleData2, '\\s*,\\s*'))
        sample2 = as.numeric(sample2)
      } else if(input$inputFiles == "Sample Data") {
        sample1 = mtcars[input$twoSampleColumnOne]
        sample2 = mtcars[input$twoSampleColumnTwo]
      } else {
        file = input$twoSampleFile
        uploaded_csv = read.csv(file$datapath, header=TRUE)
        sample1 = uploaded_csv[input$multiUploadedColumnOne]
        sample2 = uploaded_csv[input$multiUploadedColumnTwo]
      }
      alpha = as.numeric(input$alphaTwoSample)
      results = t.test(sample1, sample2, conf.level=1-alpha)
      results
    }
    else {
      n_success = as.numeric(input$numSuccesses)
      n_trial = as.numeric(input$numTrials)
      prop_hypo = as.numeric(input$propHypo)
      alpha = as.numeric(input$alphaProp)
      prop.test(n_success, n_trial, p=prop_hypo, conf.level=1-alpha)
    }
  })
  
  # this needs to be updated to depend on user input for graphs
  output$plot = renderPlot({
    if(input$hypothesis == "One-Sample t-Test") {
      if(input$inputFiles == "CSV Upload (needs headers)" & is.null(input$singleSampleFile)){
        return("Please upload a CSV to continue!")
      } else if(input$inputFiles == "Manual Input") {
        sample = unlist(strsplit(input$singleSampleData, '\\s*,\\s*'))
        sample = as.numeric(sample)
      } else if(input$inputFiles == "Sample Data") {
        sample = unlist(unname(as.vector(mtcars[input$singleSampleColumn])))
      } else {
        file = input$singleSampleFile
        uploaded_csv = read.csv(file$datapath, header=TRUE)
        sample = unlist(unname(as.vector(uploaded_csv[input$singleUploadedColumn])))
      }
      if(input$graphSelOneSamp == "Histogram") {
        hist(sample, xlab="Values", title="Sample Data Distribution")
      } else {
        boxplot(sample, main="Boxplot of Sample Data")
      }
    } else if(input$hypothesis == "Two-Sample t-Test") {
      if(input$inputFiles == "CSV Upload (needs headers)" & is.null(input$twoSampleFile)){
        return("Please upload a CSV to continue!")
      } else if(input$inputFiles == "Manual Input") {
        sample1 = unlist(strsplit(input$sampleData1, '\\s*,\\s*'))
        sample1 = as.numeric(sample1)
        sample2 = unlist(strsplit(input$sampleData2, '\\s*,\\s*'))
        sample2 = as.numeric(sample2)
      } else if(input$inputFiles == "Sample Data") {
        sample1 = unname(unlist(as.vector(mtcars[input$twoSampleColumnOne])))
        sample2 = unname(unlist(as.vector(mtcars[input$twoSampleColumnTwo])))
      } else {
        file = input$twoSampleFile
        uploaded_csv = read.csv(file$datapath, header=TRUE)
        sample1 = unlist(unname(as.vector(uploaded_csv[input$multiUploadedColumnOne])))
        sample2 = unlist(unname(as.vector(uploaded_csv[input$multiUploadedColumnTwo])))
      }
      if(input$graphSelTwoSamp == "Histogram") {
        par(mfrow=c(1,2))
        hist1 = hist(sample1, xlab="Values", title="Sample 1 Distribution")
        hist2 = hist(sample2, xlab="Values", title="Sample 2 Distribution")
      } else {
        boxplot(sample1, sample2, names=c("Sample 1", "Sample 2"), main="Boxplot of Two Samples")
      }
    } else {
      n_success = as.numeric(input$numSuccesses)
      n_trial = as.numeric(input$numTrials)
      n_failures = n_trial - n_success
      barplot(c(n_success, n_failures), names=c("Successes", "Failures"), main="Proportion Test Breakdown")
    }
  })
} 

shinyApp(ui=ui, server=server)
library(shiny)

# this looks weird but uploaded_csv needed to be instatiated globally
# or else it had a frame issue
uploaded_csv = data.frame()

ui = fluidPage(
  # title elements
  titlePanel("Improved Interactive Hypothesis Testing App"),
  titlePanel(h4("Created by Kevin Cha, Qixiang Gao, Prabhnoor Kaur")),
  sidebarLayout(
    # left side selector elements
    sidebarPanel(
      # hypothesis input
      selectInput(
        "hypothesis", "Choose Hypothesis Test:",
        c("One-Sample t-Test", "Two-Sample t-Test", "Proportion Test")
      ),
      # hypothesis conditional for single sample
      conditionalPanel(
        condition = "input.hypothesis == 'One-Sample t-Test'",
        radioButtons(
          "inputFiles", "Select data input option:",
          c("Manual Input", "Sample Data", "CSV Upload (needs headers)")
        ),
        # conditionals based on data input selection
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
            "singleSampleFile", "Upload File (csv)", accept=".csv"
          ),
          uiOutput("singlePlaceholder")
        ),
        # single samp params
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
      # hypothesis conditional for two sample
      conditionalPanel(
        condition = "input.hypothesis == 'Two-Sample t-Test'",
        radioButtons(
          "inputFiles", "Select data input option:",
          c("Manual Input", "Sample Data", "CSV Upload (needs headers)")
        ),
        # conditionals based on data input selection, for two samp
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
            "twoSampleFile", "Upload File (csv)", accept=".csv"
          ),
          uiOutput("multiPlaceholderOne"),
          uiOutput("multiPlaceholderTwo")
        ),
        # two samp params
        textInput(
          "alphaTwoSample", "Significance Level (α):", 0.05
        ),
        selectInput(
          "graphSelTwoSamp", "Graph Selection:",
          c("Histogram", "Boxplot")
        )
      ),
      # prop test conditional
      conditionalPanel(
        condition = "input.hypothesis == 'Proportion Test'",
        # nothing special for prop tests
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
      # run button
      actionButton("runButton", "Run Test", class = "btn-primary")
    ),
    # right side, hypo test + graph results
    mainPanel(
      tabsetPanel(
        tabPanel("Result", verbatimTextOutput("results"), plotOutput("plot")),
        tabPanel("Help", h3("What is Hypothesis Testing?"), 
                 p("Hypothesis testing is a method of making decisions or inferences about population parameters based on sample data"),
                 p("In this app, you can perform the following tests:"),
                 p("\t- One-Sample t-Test: Compare the mean of a sample to a known population mean."),
                 p("\t- Two-Sample t-Test: Compare the means of two independent samples."),
                 p("\t- Proportion Test: Test the proportion of successes against a hypothesized proportion."),
                 p("Use the Results tab to see outputs such as p-values, test statistics, and visualizations."))
      )
    )
  )
)

server = function(input, output) {
  # on single samp file upload, parse the file + update column selection
  observeEvent(input$singleSampleFile, {
  output$singlePlaceholder = renderUI({
      file = input$singleSampleFile
      uploaded_csv = read.csv(file$datapath, header=TRUE)
      choices=colnames(uploaded_csv[unlist(lapply(uploaded_csv, is.numeric))])
      selectInput("singleUploadedColumn", "Select Column:", choices=choices)
  })})
  # on two samp file upload, parse the file + update column selection
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
  # hypothesis test result renderer
  output$results = renderPrint({
    # conditionals based on type of hypothesis test
    if(input$hypothesis == "One-Sample t-Test") {
      # base case - if user selected CSV + there's no file input yet display message
      if(input$inputFiles == "CSV Upload (needs headers)" & is.null(input$singleSampleFile)){
        return("Please upload a CSV to continue!")
      } else if(input$inputFiles == "Manual Input") {
        # looks weird but string splitting commas with surrounding whitespace via regex parsing
        # to read in user input comma separated lists
        sample = unlist(strsplit(input$singleSampleData, '\\s*,\\s*'))
        sample = as.numeric(sample)
      } else if(input$inputFiles == "Sample Data") {
        sample = mtcars[input$singleSampleColumn]
      } else {
        file = input$singleSampleFile
        uploaded_csv = read.csv(file$datapath, header=TRUE)
        sample = uploaded_csv[input$singleUploadedColumn]
      }
      # other field grabbing + displaying the results
      mu = as.numeric(input$mu)
      alpha = as.numeric(input$alphaOneSample)
      results = t.test(sample, mu=mu, conf.level=1-alpha)
      results
    }
    # mostly same idea in here as one samp code, just doubled for two samp
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
      # prop test code - all just read ins
      n_success = as.numeric(input$numSuccesses)
      n_trial = as.numeric(input$numTrials)
      prop_hypo = as.numeric(input$propHypo)
      alpha = as.numeric(input$alphaProp)
      prop.test(n_success, n_trial, p=prop_hypo, conf.level=1-alpha)
    }
  })
  
  # graph result renderer
  output$plot = renderPlot({
    # conditional on hypothesis again
    if(input$hypothesis == "One-Sample t-Test") {
      # most of this code is repeated
      if(input$inputFiles == "CSV Upload (needs headers)" & is.null(input$singleSampleFile)){
        return("Please upload a CSV to continue!")
      } else if(input$inputFiles == "Manual Input") {
        sample = unlist(strsplit(input$singleSampleData, '\\s*,\\s*'))
        sample = as.numeric(sample)
      } else if(input$inputFiles == "Sample Data") {
        # need to vectorize/unname/unlist otherwise the grapher complains
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
      # largely same idea as one samp
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
      # prop test - can't meaningfully be anything else besides a barplot tbh
      n_success = as.numeric(input$numSuccesses)
      n_trial = as.numeric(input$numTrials)
      n_failures = n_trial - n_success
      barplot(c(n_success, n_failures), names=c("Successes", "Failures"), main="Proportion Test Breakdown")
    }
  })
} 

shinyApp(ui=ui, server=server)
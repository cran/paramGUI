#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## paramGUI dependencies
library(TIMP)

## Shiny app.R dependencies ##
library(shiny)
library(shinydashboard)
# Potentially useful but unused libraries
# library(shinyjs)
# library(DT) # https://yihui.shinyapps.io/DT-rows/
# library(shinyBS)

#' Compile a list of spectral parameters (location, width, skewness)
#'
#' @param spec_loc
#' @param spec_wid
#' @param spec_b
#'
#' @return
#'
newSpecList <- function(spec_loc, spec_wid, spec_b) {
  specvec <- vector("list", length(spec_loc))
  for(i in 1:length(spec_loc)){
    specvec[[i]][1] <- as.double(spec_loc[i])
    specvec[[i]][2] <- as.double(spec_wid[i])
    specvec[[i]][3] <- as.double(spec_b[i])
  }
  specvec
}

ui <- dashboardPage(

  dashboardHeader(title = "paramGUI"), # Remove ), from this line an uncomment the next to enable notifcation menu.
  # dropdownMenuOutput("messageMenu"),dropdownMenuOutput("notificationMenu")),

  dashboardSidebar(

    tags$head(tags$style(HTML('
                              /* Change padding of sub-menu items */
                              .row {
                              margin-right: 5px;
                              margin-left: 1px;
                              }
                              .sidebar {
                              padding-right: 5px;
                              }
                              section.sidebar .shiny-input-container {
                              padding: 0px 4px 0px 4px;
                              }
                              .col-sm-4, .col-sm-8 {
                              min-height: 1px;
                              padding-right: 0px;
                              padding-left: 1px;
                              }
                              .col-sm-6 {
                              min-height: 1px;
                              padding-right: 0px;
                              padding-left: 1px;
                              }
                              .col-sm-12 {
                              min-height: 1px;
                              padding-right: 0px;
                              padding-left: 10px;
                              }
                              .form-control {
                              display: block;
                              width: 100%;
                              height: 22px;
                              padding: 0px 4px;
                              }
                              .form-group {
                              margin-bottom: 5px;
                              margin-left: 5px;
                              }


                              '))),

    tabsetPanel(
      tabPanel("Simulate",
               textInput("simDecayRates", label = "Decay rates: ", value = "0.055,0.005"),
               textInput("simAmplitudes", label = "Amplitudes: ", value = "1.,1."),
               textInput("simSpecLoc", label = div(HTML("Location (mean) of spectra (cm<sup>-1</sup>):")), value = "22000,20000"),
               textInput("simSpecWidth", label = div(HTML("Width of spectra (cm<sup>-1</sup>):")), value = "4000,3500"),
               textInput("simSpecSkew", label = "Skewness of spectra:", value = "0.1, -0.1"),
               fluidRow(
                 column(8,textInput("simMaxTime", label = "Timepoints, max:", value = "80")),
                 column(4,textInput("simTimeStep", label = "stepsize:", value = "1"))
               ),
               fluidRow(
                 column(12,HTML("<b>Wavelength (nm):</b>"))),
               fluidRow(
                 column(4,
                        textInput("simMinWavelength", label = "Min:", value = "400")),
                 column(4,
                        textInput("simMaxWavelength", label = "Max:", value = "600")),
                 column(4,
                        textInput("simWavelengthStepSize", label = "Stepsize:", value = "5"))
               ),
               fluidRow(
                 column(6,numericInput("simFracNoise", label = "Stdev. noise:", value = "0.05", min = 0, step = 0.01)),
                 column(6,numericInput("simSeed", label = "Seed:", value = "123", min = 0, step = 1))
               ),
               checkboxInput("simEnableIRF", label = "Add Gaussian IRF", value = FALSE, width = NULL),
               conditionalPanel(condition = "(input.simEnableIRF== true)",
                                fluidRow(
                                  column(6,textInput("simLocIRF", label = "IRF Location:", value = "2.0")),
                                  column(6,textInput("simWidthIRF", label = "IRF Width:", value = "1.0"))
                                )),
               checkboxInput("simSeqmod", label = "Use a sequential scheme", value = FALSE, width = NULL),
               actionButton("simButton", "Simulate")

      ), #end of Simulate tab

      tabPanel("Fitting",
               selectInput("modelType", label = h5("Select a model type"),
                           choices = list("Kinetic" = "kin", "Spectral" = "spec", "Spectrotemporal" = "spectemp"),
                           selected = "kin"),
               conditionalPanel(condition = "(input.modelType=='kin' || input.modelType=='spectemp')",
                                textInput("fitDecayRates", label = "Decay rates: ", value = "0.055,0.005")
               ),
               conditionalPanel(condition = "(input.modelType=='spec' || input.modelType=='spectemp')",
                                textInput("fitSpecLoc", label = "Location (mean) of spectra:", value = "22000,20000"),
                                textInput("fitSpecWidth", label = "Width of spectra:", value = "4000,3500"),
                                textInput("fitSpecSkew", label = "Skewness of spectra:", value = "0.1, -0.1")
               ),
               conditionalPanel(condition = "(input.modelType=='kin' || input.modelType=='spectemp')",
                                fluidRow(
                                  column(6,checkboxInput("fitEnableIRF", label = "Gaussian IRF", value = FALSE, width = NULL)),
                                  column(6,checkboxInput("fitEnableStreak", label = "Backsweep?", value = FALSE))
                                ),
                                conditionalPanel(condition = "(input.fitEnableIRF== true)",
                                                 fluidRow(
                                                   column(6,textInput("fitLocIRF", label = "IRF Location:", value = "2.0")),
                                                   column(6,textInput("fitWidthIRF", label = "IRF Width:", value = "1.0"))
                                                 )),
                                checkboxInput("fitSeqmod", label = "Use a sequential scheme", value = FALSE, width = NULL),
                                checkboxInput("fitPosDec", label = "Force positive decay rates", value = FALSE, width = NULL)
               ),
               conditionalPanel(condition = "(input.modelType=='spectemp')",
                                checkboxInput("fitKroncol", label = "Single amplitude per component", value = FALSE, width = NULL)),
               numericInput("fitLinAxis", label = "Linear-Log axis (0 for linear):", value = 0, min = 0),
               numericInput("fitNumIters", label = "Max. number of iterations:", value = "7", min = 0, max = 99, step = 1),

               # uiOutput("specControls"),
               #conditionalPanel(condition = '!input.fitButton',
               # helpText("Note: you might need to increase the number of iterations to reach convergence")
               #)
               #,
               fluidRow(
                 column(6,actionButton("fitButton", "Fit model")),
                 column(6,conditionalPanel(condition = 'input.fitButton',
                                           actionButton("updateModelButton", "Update model")))
               ),
               conditionalPanel(condition = 'input.fitButton',
                                helpText("Update model updates the input field with the results from your last fit."))),
      tabPanel("I/O",
               h4("Load data"),
               fileInput("loadData",label=NULL),
               tags$script('$( "#loadData" ).on( "click", function() { this.value = null; });'),
               # http://stackoverflow.com/questions/34441584/re-upload-same-file-shiny-r
               # TODO: http://stackoverflow.com/questions/17352086/how-can-i-update-a-shiny-fileinput-object
               actionButton('loadDefaultDataButton', label = "Load Default Data"),
               h4("Save data"),
               conditionalPanel(condition = '!input.simButton',
                                helpText("Note: once you have simulated data the option to save your data locally or export (download) your data will appear here.")),
               conditionalPanel(condition = 'input.simButton',
                                textInput("simFilename", label = "Base filename:", value = "sim"),
                                fluidRow(
                                  column(6,actionButton('saveDataButton', label = "Save")),
                                  column(6,downloadButton('downloadData', label = "Download"))
                                ),
                                helpText("The save button will save your data to your home/documents folder, the download button will allow your to download the file (but only in a real browser).")
               )

      )
    ), width = 300),

  dashboardBody(
    # Boxes need to be put in a row (or column)
    tags$head(tags$style(HTML('
                              /* Change padding of sub-menu items */
                              .sidebar .sidebar-menu .treeview-menu>li>a {
                              padding: 5px 5px 5px 8px;
                              }

                              /* Hide icons in sub-menu items */
                              .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                              display: none;
                              }
                              pre {
                              height: 80vh;
                              overflow-y: auto;
                              overflow-x: auto;
                              word-wrap: normal;
                              }
                              '))),
    fluidRow(
      tabBox(title = "RESULTS",
             id = "outputTabs", height = "700px", width = "670px",
             tabPanel("Data",
                      plotOutput("dataPlot", height = 650, width = 900)),
             tabPanel("Fit progression",
                      verbatimTextOutput("fitProgressOutput")),
             tabPanel("Fit results",
                      plotOutput("fitPlot", height = 650, width = 900)
             ),
             # http://stackoverflow.com/questions/19470426/r-shiny-add-tabpanel-to-tabsetpanel-dynamicaly-with-the-use-of-renderui
             #conditionalPanel(condition = 'input.fitButton',
             tabPanel("Diagnostics",
                      verbatimTextOutput("consoleOutput"),
                      actionButton("printSummaryButton", "Print summary")
             )
             #)

      )
    )

  )
)

server <- function(input, output, session) {

  rvs <- reactiveValues()

  # output$specControls <- renderUI({
  #  if((input$modelType == "kin" || input$modelType == "spectemp")) {
  #
  #  }
  #})

  output$downloadData <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
      paste0(isolate(input$simFilename),"-",timestamp,'.rds', sep='')
    },
    content = function(file) {
      saveRDS(isolate(rvs$simData), file)
    }
  )



  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    # msgs <- apply(messageData, 1, function(row) {
    #  messageItem(from = row[["from"]], message = row[["message"]])
    #})

    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "Administrator",
                   message = "Please register"
                 )
                 # .list = msgs
    )
  })

  output$notificationMenu <- renderMenu({
    dropdownMenu(type = "notifications",
                 messageItem(
                   from = "New User",
                   message = "How do I register?",
                   icon = icon("question"),
                   time = "13:45"
                 ),
                 notificationItem(
                   text = "Server load at 86%",
                   icon = icon("exclamation-triangle"),
                   status = "warning"
                 )
                 # .list = msgs
    )
  })


  observeEvent(input$simButton, {

    updateTabsetPanel(session,"outputTabs",selected="Data" )

    set.seed(isolate(input$simSeed))

    kinpar <- as.double(strsplit(isolate(input$simDecayRates),",")[[1]])
    amplitudes <- as.double(strsplit(input$simAmplitudes,",")[[1]])
    spec_loc <- strsplit(isolate(input$simSpecLoc), ",")[[1]]
    spec_wid <- strsplit(isolate(input$simSpecWidth), ",")[[1]]
    spec_b <- strsplit(isolate(input$simSpecSkew), ",")[[1]]
    tmax <- as.double(isolate(input$simMaxTime))
    deltat <- as.double(isolate(input$simTimeStep))
    lmin <- as.double(isolate(input$simMinWavelength))
    lmax <- as.double(isolate(input$simMaxWavelength))
    linAxis <- isolate(input$fitLinAxis)
    linr <- if(length(linAxis)==0) NA else if(linAxis<0.1) NA else as.double(isolate(input$fitLinAxis))
    deltal <- as.double(isolate(input$simWavelengthStepSize))
    sigma <- as.double(isolate(input$simFracNoise))
    irf <- isolate(input$simEnableIRF)
    irfloc <- as.double(isolate(input$simLocIRF))
    irfwidth <- as.double(isolate(input$simWidthIRF))
    seqmod <- isolate(input$simSeqmod)
    specvec <- newSpecList(spec_loc, spec_wid, spec_b)

    # TODO: add debug flag
    cat("# Simulating data with function call: \n")
    cat("simndecay_gen_paramGUI(kinpar =",deparse(kinpar),",",
        "amplitudes = ",deparse(amplitudes),",",
        "tmax = ", tmax,",",
        "deltat= ", deltat,",",
        "specpar= ", deparse(specvec), ",",
        "lmin= ", lmin,",",
        "lmax= ", lmax,",",
        "deltal= ", deltal,",",
        "sigma= ", sigma,",",
        "irf = ", irf,",",
        "irfpar = c(",irfloc,",",irfwidth,")",",",
        "seqmod =",seqmod,")\n")

    if(!is.na(lmin) && !is.na(lmax) && !is.na(tmax) && !is.na(deltal)
       && length(kinpar)==length(amplitudes) && length(kinpar)==length(spec_loc)) {
    rvs$simData <- simndecay_gen_paramGUI(kinpar=kinpar,
                                          amplitudes = amplitudes,
                                          tmax=tmax,
                                          deltat=deltat,
                                          specpar=specvec,
                                          lmin=lmin,
                                          lmax=lmax,
                                          deltal=deltal,
                                          sigma=sigma,
                                          irf = irf,irfpar = c(irfloc,irfwidth),
                                          seqmod = seqmod)

    # assign(".sim", isolate(rvs$simData) , globalenv())
    updateDataPlot(irfloc, linr)
    } else {
      print("Invalid simulation input!", file=stderr())
    }

  })

  observeEvent(input$fitButton, {

    withProgress({
      ## This works with a function like message
      ##withCallingHandlers({
      ##shinyjs::html("fitProgressOutput","")
      ##

      updateTabsetPanel(session,"outputTabs",selected="Fit progression" )

      kinpar <- as.double(strsplit(isolate(input$fitDecayRates),",")[[1]])
      spec_loc <- strsplit(isolate(input$fitSpecLoc), ",")[[1]]
      spec_wid <- strsplit(isolate(input$fitSpecWidth), ",")[[1]]
      spec_b <- strsplit(isolate(input$fitSpecSkew), ",")[[1]]
      specvec <- newSpecList(spec_loc, spec_wid, spec_b)
      kroncol <- input$fitKroncol
      irf <- input$fitEnableIRF
      irfloc <- as.double(isolate(input$fitLocIRF))
      irfwidth <- as.double(isolate(input$fitWidthIRF))
      irfpar <- c(irfloc, irfwidth)
      seqmod <- input$fitSeqmod
      positivepar <- input$fitPosDec
      streak <- input$fitEnableStreak
      rvs$modelType <- input$modelType
      linAxis <- isolate(input$fitLinAxis)
      linr <- if(is.na(linAxis)) {NA} else {if(linAxis<0.1) {NA} else {linAxis}}
      iters <- isolate(input$fitNumIters)

      isolate({

        if((isolate(rvs$modelType)=="kin")) {
          output$fitProgressOutput <- renderPrint({
            rvs$kinModel <- initModel(mod_type = "kin", kinpar = kinpar, irf = irf,
                                      irfpar = if(irf) irfpar else vector(),
                                      streak = streak,
                                      streakT = 13164.8235,
                                      positivepar = positivepar,
                                      seqmod=seqmod)

            rvs$kinFit <- fitModel(data=list(isolate(rvs$simData)), modspec=list(isolate(rvs$kinModel)),opt=kinopt(iter=iters, plot=FALSE))
            rvs$kinFitSummary <- summary(isolate(rvs$kinFit)$currModel@fit@nlsres[[1]],
                                         currModel=isolate(rvs$kinFit)$currModel,
                                         currTheta=isolate(rvs$kinFit)$currTheta,
                                         correlation=TRUE)
            updateConsole(isolate(rvs$modelType))
            updatePlots(isolate(rvs$modelType), isolate(rvs$simData), isolate(rvs$kinModel), isolate(rvs$kinFit), linr = isolate(linr))
          })

        } else if(isolate(rvs$modelType)=="spec") {
          output$fitProgressOutput <- renderPrint({
            rvs$specModel <- initModel(mod_type = "spec", specpar = specvec, nupow=1)

            rvs$specFit <- fitModel(data=list(isolate(rvs$simData)), modspec=list(isolate(rvs$specModel)),opt=kinopt(iter=iters, plot=FALSE))

            rvs$specFitSummary <- summary(isolate(rvs$specFit)$currModel@fit@nlsres[[1]],
                                          currModel=isolate(rvs$specFit)$currModel,
                                          currTheta=isolate(rvs$specFit)$currTheta,
                                          correlation=TRUE)
            updateConsole(isolate(rvs$modelType))
            updatePlots(isolate(rvs$modelType), isolate(rvs$simData), isolate(rvs$specModel), isolate(rvs$specFit), linr = isolate(linr))
          })
          # Currently the kin and spectemp models are treated the same
        } else if(isolate(rvs$modelType) == "spectemp") {
          output$fitProgressOutput <- renderPrint({
            rvs$spectempModel <-  initModel(mod_type = "kin", kinpar = kinpar, irf = irf,
                                            irfpar = if(irf) irfpar else vector(),
                                            streak = streak,
                                            streakT = 13164.8235,
                                            positivepar = positivepar,
                                            seqmod=seqmod)
            isolate({
              rvs$spectempModel@specpar <- isolate(specvec)
              rvs$spectempFit<-spectemp(isolate(rvs$simData), isolate(rvs$spectempModel), iter=iters, kroncol = kroncol, lin=linr,l_posk=positivepar)
              rvs$spectempFitSummary <- isolate(rvs$spectempFit$onls)
              rvs$spectempFitTheta <- isolate(rvs$spectempFit$theta)
              updateConsole(isolate(rvs$modelType))

              updatePlots(isolate(rvs$modelType), isolate(rvs$simData), isolate(rvs$spectempModel), isolate(rvs$spectempFitSummary), isolate(rvs$spectempFitTheta), linr = isolate(linr))
            })

          })


        } else {
          setProgress(value = 0, message = "failed.")
          print("model not implemented", file=stderr())
        }


      }) ## end of isolate

    }, message ="fitting data ...") ## end of withProgress
  }
  )

  observeEvent(input$saveDataButton, {
    tryFilename <- paste(isolate(input$simFilename),"-",format(Sys.time(), "%Y%m%d_%H%M"),".rds",sep="")
    tryFullFilename <- file.path(path.expand("~"),tryFilename)
    saveRDS(isolate(rvs$simData), tryFullFilename)
    cat("File was saved to:\n",tryFullFilename,"\n",file=stdout())

  }
  )

  observeEvent(input$updateModelButton, {
    # # fitDecayRates, fitLocIRF, fitWidthIRF
    isolate({
      if(rvs$modelType=="kin" && !is.null(isolate(rvs$kinFit))) {
        updateTextInput(session, "fitDecayRates", value = toString(signif(rvs$kinFit$currTheta[[1]]@kinpar,digits=4)))
        if(length(rvs$kinFit$currTheta[[1]]@irfpar)>0) {
          updateTextInput(session, "fitLocIRF", value = toString(signif(rvs$kinFit$currTheta[[1]]@irfpar[[1]],digits=4)))
          updateTextInput(session, "fitWidthIRF", value = toString(signif(rvs$kinFit$currTheta[[1]]@irfpar[[2]],digits=4)))
        }
        # updatePlots(isolate(rvs$modelType), isolate(rvs$simData), isolate(rvs$kinModel), isolate(rvs$kinFit), linr = isolate(linr))
      }
      # # fitSpecLoc fitSpecWidth  fitSpecSkew
      if(rvs$modelType=="spec" && !is.null(isolate(rvs$specFit)))  {
        nsc <- length(rvs$specFit$currTheta[[1]]@specpar) #numberOfSpectralComponents
        if(nsc>0) {
          spectralParameterVector <- do.call(c,rvs$specFit$currTheta[[1]]@specpar)
          updateTextInput(session, "fitSpecLoc", value = toString(signif(spectralParameterVector[seq(1,3*nsc,3)],digits=4)))
          updateTextInput(session, "fitSpecWidth", value = toString(signif(spectralParameterVector[seq(2,3*nsc,3)],digits=4)))
          updateTextInput(session, "fitSpecSkew", value = toString(signif(spectralParameterVector[seq(3,3*nsc,3)],digits=4)))
        }
        # updatePlots(isolate(rvs$modelType), isolate(rvs$simData), isolate(rvs$specModel), isolate(rvs$specFit), linr = isolate(linr))

      }
      if(rvs$modelType=="spectemp" && !is.null(isolate(rvs$spectempFit))) {
        updateTextInput(session, "fitDecayRates", value = toString(signif(rvs$spectempFit[[1]]@kinpar,digits=4)))
        if(length(rvs$spectempFit[[1]]@irfpar)>0) {
          updateTextInput(session, "fitLocIRF", value = toString(signif(rvs$spectempFit[[1]]@irfpar[[1]],digits=4)))
          updateTextInput(session, "fitWidthIRF", value = toString(signif(rvs$spectempFit[[1]]@irfpar[[2]],digits=4)))
        }
        nsc <- (length(rvs$spectempFit[[1]]@specpar[[1]])/3) #numberOfSpectralComponents
        if(nsc>0) {
          spectralParameterVector <- rvs$spectempFit[[1]]@specpar[[1]]
          updateTextInput(session, "fitSpecLoc", value = toString(signif(spectralParameterVector[seq(1,3*nsc,3)],digits=4)))
          updateTextInput(session, "fitSpecWidth", value = toString(signif(spectralParameterVector[seq(2,3*nsc,3)],digits=4)))
          updateTextInput(session, "fitSpecSkew", value = toString(signif(spectralParameterVector[seq(3,3*nsc,3)],digits=4)))
        }
      }
    })
  }
  )

  observeEvent(input$printSummaryButton, {
    resultToPrint <- switch(input$modelType,
                            kin=rvs$kinFitSummary,
                            spec=rvs$specFitSummary,
                            spectemp=rvs$spectempFitSummary
    )
    print(resultToPrint,
          file=stdout())
  }
  )

  updatePlots <- function(modType="kin", data, model=NULL, result=NULL, theta=NULL, linr = NA) {
    output$fitPlot <- renderPlot({
      plotterforGUI(modtype=modType, data=data, model=model, result=result, theta=theta, lin = linr)
    },res = 96)
  }

  updateDataPlot <- function(irfloc, linr) {
    # Plot the simulated data, and render it to the dataPlot field in output.
    output$dataPlot <- renderPlot({
      plotterforGUI(modtype="kin", data=isolate(rvs$simData), model=NULL, result=NULL,mu=irfloc,lin=linr)
    },res = 96)
  }

  updateConsole <- function(modelType) {
    resultToPrint <- switch(modelType,
                            kin=isolate(rvs$kinFitSummary),
                            spec=isolate(rvs$specFitSummary),
                            spectemp=isolate(rvs$spectempFitSummary)
    )
    output$consoleOutput <- renderPrint({print(resultToPrint)})
  }

  observe({
    linAxis <- input$fitLinAxis
    linr <- if(is.na(linAxis)) {NA} else {if(linAxis<0.1) {NA} else {linAxis}}
    irfloc <- as.double(isolate(input$simLocIRF))
    if(!is.null(isolate(rvs$simData))) {
      updateDataPlot(irfloc, linr)
    }
    # updatePlots(isolate(rvs$modelType), isolate(rvs$simData), isolate(rvs$kinModel), isolate(rvs$kinFit), linr = isolate(linr))
    if(!is.null(isolate(rvs$simData))) {
      if(rvs$modelType=="kin" && !is.null(isolate(rvs$kinFit))) updatePlots(isolate(rvs$modelType), isolate(rvs$simData), isolate(rvs$kinModel), isolate(rvs$kinFit), linr = isolate(linr))
      if(rvs$modelType=="spec" && !is.null(isolate(rvs$specFit))) updatePlots(isolate(rvs$modelType), isolate(rvs$simData), isolate(rvs$specModel), isolate(rvs$specFit), linr = isolate(linr))
      if(rvs$modelType=="spectemp" && !is.null(isolate(rvs$spectempFit))) updatePlots(isolate(rvs$modelType), isolate(rvs$simData), isolate(rvs$spectempModel), isolate(rvs$spectempFitSummary), isolate(rvs$spectempFitTheta), linr = isolate(linr))
    }
  })

  observe({
    infile <- input$loadData
    if(is.null(infile)) {
      return(NULL)
    } else {
      if(paramGUI::is_rdata(infile$datapath)) {
        load(infile$datapath)
        rvs$simData <- sim
        # assign(".sim", sim,globalenv())
      } else {
        # First check if the file is readable by TIMP
        testHeader <- scan(file = infile$datapath, skip = 2, nlines = 2, what = " ")
        matchedKeywords <- length(grep(paste(c("Time","Wavelength","explicit","Intervalnr"),collapse="|"),testHeader,ignore.case=TRUE,value=TRUE))
        if(matchedKeywords>2) {
          rvs$simData <- TIMP::readData(infile$datapath)
        } else {
          print("# Unable to load data.\n",file=stderr())
        }
      }
      updateTabsetPanel(session,"outputTabs",selected="Data" )
      output$dataPlot <- renderPlot({
        plotterforGUI(modtype="kin", data=isolate(rvs$simData), model=NULL, result=NULL,mu=0)
      },res = 96)

    }
  })

  observeEvent(input$loadDefaultDataButton, {
    loadDefaultData()
  })

  loadDefaultData <- function() {
    print("\nLoading data representing the peridinin chlorophyll protein (PCP) excited with 490 nm laser light...\n",file=stdout())
    rvs$simData <- dat(psi.df = datamat, x2 = waves, x = times,
                       nt = length(times), nl = length(waves), simdata = FALSE)
    updateTabsetPanel(session,"outputTabs",selected="Data" )
    output$dataPlot <- renderPlot({
      plotterforGUI(modtype="kin", data=isolate(rvs$simData), model=NULL, result=NULL,mu=0,lin=1)
    },res = 96)
  }

}

shinyApp(ui, server)

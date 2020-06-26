ui <- shiny::fluidPage(
  shiny::navbarPage(title = "Behaviour Analysis",id = "inTabset",
                    shiny::tabPanel(title = "Load Data", shiny::sidebarLayout(
                      # Sidebar with input parameters for loading and accessing files
                      shiny::sidebarPanel(
                        shiny::fileInput(inputId = "FileName", 
                                         label = "Choose File",
                                         multiple = T,
                                         accept = ".csv"),
                        shiny::numericInput(inputId = "FrameRate",
                                            label = "Frame Rate",
                                            value = 30,
                                            min = 1,
                                            step = 1),
                        shiny::numericInput(inputId = "GroupLabels",
                                            label = "Number of Group Labels",
                                            value = 1,
                                            min = 1,
                                            step = 1),
                        shiny::textInput(inputId = "GroupNames",
                                         label = "Names of Group Labels",
                                         placeholder = 'Names Seperated by ","'),
                        shiny::selectInput(inputId = "ObjectChoices",
                                           label = "Objects:",
                                           choices = NULL,
                                           multiple = T),
                        shiny::checkboxGroupInput(inputId = "InputSettings",
                                                  label = "Settings:",
                                                  choices = c("Jump Correction",
                                                              "Change Scaling",
                                                              "Include All Parameters")),
                        shiny::conditionalPanel(condition = "input.InputSettings.indexOf('Change Scaling') > -1",
                                                shiny::numericInput(inputId = "Scaling",
                                                                    label = "Adjust xy-Scaling",
                                                                    value = 1,
                                                                    min = 0,
                                                                    step = 0.1)), 
                        shiny::actionButton(inputId = "nextTabAnalysis",
                                            label = "Continue")
                      ),
                      
                      # Main Panel
                      shiny::mainPanel(
                        shiny::flowLayout(
                          shiny::conditionalPanel(condition = "input.GroupLabels > 0",
                                                  shiny::uiOutput(outputId = "GroupChoices")       
                          )
                        )
                      )
                    )
                    ),
                    shiny::tabPanel(title = "Analyse Data", shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        shiny::helpText("Choose a function you want to apply and fill in the variables. Possible choices will be offered once you decide for a function type."),
                        shiny::uiOutput(outputId = "UserFunctions"),
                        shiny::conditionalPanel(condition = "input.FunctionSelect.length > 0",
                                                shiny::uiOutput(outputId = "FunctionInput")),
                        shiny::actionButton(inputId = "AnalyseTable",
                                            label = "Analyse"),
                        shiny::actionButton(inputId = "nextTabPlotting",
                                            label = "Continue"),
                        shiny::selectInput(inputId = "DeleteSelection",
                                           label = "Select Variable",
                                           choices = NULL,
                                           multiple = T),
                        shiny::actionButton(inputId = "deleteColumn",
                                            label = "Delete Selection")
                      ),
                      shiny::mainPanel(
                        DT::dataTableOutput("Table", width = 'auto')
                      )
                    )
                    ),
                    shiny::tabPanel(title = "Plot Data", shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        shiny::helpText("Choose plots for visualisation. Possible choices will be offered once you decide for a function type."),
                        shiny::selectInput(inputId = "FileChoices",
                                           label = "Files:",
                                           choices = NULL,
                                           multiple = T,
                                           selectize = FALSE),
                        shiny::uiOutput(outputId = "PlottingFunctions"),
                        shiny::conditionalPanel(condition = "input.PlottingSelect.length > 0",
                                                shiny::uiOutput(outputId = "PlotInput")),
                        shiny::actionButton(inputId = "startPlot",
                                            label = "Plot"),
                        shiny::fluidRow(
                          shiny::column(width = 4,
                                        shinyFiles::shinyDirButton(id = "dirPlot",
                                                                   label = "Directory",
                                                                   title = "Directory",
                                                                   buttonType = "default"),
                                        shiny::selectInput(inputId = "ExportFormat",
                                                           label = "Format",
                                                           choices = c("text", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"),
                                                           selected = "pdf",
                                                           multiple = FALSE),
                                        shiny::numericInput(inputId = "heightPlot",
                                                            label = "Height",
                                                            value = 4,
                                                            min = 0,
                                                            step = 0.1)
                          ),
                          shiny::column(width = 4,
                                        shiny::actionButton(inputId = "savePlot",
                                                            label = "Save Plot"),
                                        shiny::numericInput(inputId = "dpi",
                                                            label = "dpi",
                                                            value = 72,
                                                            min = 0,
                                                            step = 50),
                                        shiny::numericInput(inputId = "widthPlot",
                                                            label = "Width",
                                                            value = 4,
                                                            min = 0,
                                                            step = 0.1),
                                        shiny::selectInput(inputId = "exportUnits",
                                                           label = "Unit",
                                                           choices = c("in", "cm", "mm", "tiff"),
                                                           selected = "in",
                                                           multiple = FALSE)
                                        
                          )
                        )
                      ),
                      shiny::mainPanel(
                        shiny::plotOutput((outputId = "PlotOut")
                        )
                      )
                    )
                    ),
                    shiny::tabPanel(title = "Export Data", shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        shinyFiles::shinyDirButton(id = "dir",
                                                   label = "Directory",
                                                   title = "Directory",
                                                   buttonType = "default"),
                        shiny::checkboxGroupInput(inputId = "OutputSelection",
                                                  label = "Choose Output:",
                                                  choices = NULL),
                        shiny::selectInput(inputId = "FileType",
                                           label = "File Type",
                                           choices = c("csv", "rds (R)", "npy (Python)", "mat (Matlab)"),
                                           multiple = F),
                        shiny::actionButton(inputId = "SaveTables",
                                            label = "Save Output")
                      ),
                      shiny::mainPanel(
                        
                      )
                    )
                    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=500*1024^2)
  BehaviourTable <- NULL
  FileList <- NULL
  variableNameList <- NULL
  uniquevariables <- NULL
  InFileLabels <- NULL
  GroupNames <- NULL
  CoordList <- NULL
  ObjectList <- NULL
  AllList <- NULL
  Time <- NULL
  PlotUpdate <- FALSE
  OutputPlot <- NULL
  
  # get files
  shiny::observeEvent(input$FileName, {
    FileList <<- input$FileName
    variableNameList <<- lapply(input$FileName$datapath, function(i) {
      utils::read.csv(file =  i, nrows = 1)[utils::read.csv(file =  i, nrows = 2)[2,]=="likelihood"]
    })
    uniquevariables <<- unlist(unique(variableNameList))
    InFileLabels <<- uniquevariables[apply(sapply(X = variableNameList, function(x){uniquevariables%in%x}), 1, all)]
    shiny::updateSelectInput(session = session,
                             inputId = "ObjectChoices",
                             label = "Objects:",
                             choices = InFileLabels)
  })
  # check for changes in input variables
  shiny::observeEvent({
    input$GroupLabels
    input$FileName
    input$GroupNames}, {
      if(nchar(input$GroupNames)!=0) {
        GroupNames <<- unlist(strsplit(x = gsub(pattern = ", ", replacement = ",", x = input$GroupNames), split = ","))
      } else {
        GroupNames <<- paste0("Group", 1:input$GroupLabels)
      }
      if(length(GroupNames)!=input$GroupLabels) {
        GroupNames <<- paste0("Group",1:input$GroupLabels)
      }
      if(!is.null(input$FileName)) {
        output$GroupChoices <- shiny::renderUI(expr = {lapply(1:input$GroupLabels, function(i) {
          shiny::selectInput(inputId = GroupNames[i],
                             label = GroupNames[i],
                             choices = InFileLabels,
                             multiple = T)
        })
        })
      }
    })
  
  shiny::observeEvent(input$nextTabAnalysis, {
    
    if(!any(is.null(unlist(lapply(1:length(GroupNames), function(x) {
      input[[GroupNames[x]]]
    })))) & !is.null(input$FileName) & input$FrameRate > 0) {
      MouseBodyLabels <- lapply(1:length(GroupNames), function(x) {
        input[[GroupNames[x]]]
      })
      names(MouseBodyLabels) <- GroupNames
      JumpCorr <- any(grepl(pattern = "Jump Correction",x = input$InputSettings))
      IncAll <- any(grepl(pattern = "Include All Parameters",x = input$InputSettings))
      BehaviourTable <<- vector(mode = "list", length = length(FileList$datapath))
      shiny::withProgress(message = 'Loading Data', value = 0, {
        for(i in seq_along(FileList$name)) {
          BehaviourTable[[i]] <<- DeepLabCutLoad(FileName = FileList$datapath[i],
                                                 FrameRate = input$FrameRate,
                                                 MouseLabels = MouseBodyLabels,
                                                 xScale = input$Scaling,
                                                 yScale = input$Scaling,
                                                 ObjectLabels = list(input$ObjectChoices),
                                                 ObjectNumber = length(input$ObjectChoices),
                                                 JumpCorrections = JumpCorr,
                                                 includeAll = IncAll)
          
          BehaviourTable[[i]][[1]][,FileName:=FileList$name[i],]
          if(length(input$ObjectChoices)>0) {
            BehaviourTable[[i]][[2]][,FileName:=FileList$name[i],]  
          }
          if(IncAll) {
            BehaviourTable[[i]][[3]][,FileName:=FileList$name[i],]
          }
          shiny::incProgress(amount = 1/length(BehaviourTable), detail = FileList$name[i])
        }
        CoordList <<- data.table::rbindlist(lapply(X = seq_along(BehaviourTable), FUN = function(i){
          BehaviourTable[[i]][[1]]
        }))
        if(length(input$ObjectChoices)>0) {
          ObjectList <<- data.table::rbindlist(lapply(X = seq_along(BehaviourTable), FUN = function(i){
            BehaviourTable[[i]][[2]]
          })) 
        }
        if(IncAll) {
          AllList <<- data.table::rbindlist(lapply(X = seq_along(BehaviourTable), FUN = function(i){
            BehaviourTable[[i]][[3]]
          })) 
        }
        shiny::showNotification(ui = "Data loaded...", type = "message")
        shiny::updateSelectInput(session = session, inputId = "DataSelect", choices = FileList$name)
        if(length(input$ObjectChoices)==0) {
          FunctionChoices <- c("",
                               "Add Centroid",
                               "Angle Calculation",
                               "Angle Difference",
                               "Distance and Speed",
                               "Vector Length")
        } else {
          FunctionChoices <- c("",
                               "Add Centroid",
                               "Angle Calculation",
                               "Angle Difference",
                               "Distance and Speed",
                               "Object Distance",
                               "Object Angle",
                               "Vector Length",
                               "Zone Entry")
        }
        output$UserFunctions <- shiny::renderUI(expr = shiny::selectInput(inputId = "FunctionSelect",
                                                                          label = "Select Function",
                                                                          choices = FunctionChoices))
        output$Table <- DT::renderDataTable({
          DTSummary <- CoordList[,lapply(X = .SD, function(x){
            Type <- typeof(x)
            Min <- ifelse(Type=="character", "-", round(min(x, na.rm = T), digits = 2))
            Max <- ifelse(Type=="character", "-", round(max(x, na.rm = T), digits = 2))
            Mean <- ifelse(Type=="character", "-", round(mean(x, na.rm = T), digits = 2))
            NA_Count <- ifelse(Type=="character", "-", sum(is.na(x)))
            c(Type, Min, Mean, Max, NA_Count)})]
          DTSummary[,ColumnNames:=c("Type", "Min", "Mean", "Max", "NA Count"),]
          DT::datatable(data = dcast(melt(DTSummary, id.vars = "ColumnNames"), variable ~ ColumnNames),
                        style = "bootstrap",
                        escape = F,
                        options = list(paging = TRUE,
                                       pageLength = 25))
        })
        shiny::updateTabsetPanel(session, "inTabset", selected = "Analyse Data")
        ColumnNamesSelect <- colnames(CoordList)
        ColumnNamesSelect <- unique(gsub(pattern = "_x|_y", replacement = "", x = ColumnNamesSelect[-grep(pattern = "frame|Time", x = ColumnNamesSelect)]))
        shiny::updateSelectInput(session = session,
                                 inputId = "DeleteSelection",
                                 label = "Select Variable",
                                 choices = ColumnNamesSelect)
        TableSelection <- c("Coordinate Table", "Object Table", "Table with extra Parameters")[c(!is.null(CoordList), !is.null(ObjectList), !is.null(AllList))]
        shiny::updateCheckboxGroupInput(session = session, inputId = "OutputSelection", label = "Choose Output", choices = TableSelection, selected = c("Coordinate Table", "Object Table"))
      })} else {
        shiny::showNotification(ui = "Missing Inputs", type = "error")
      }
  })
  
  shiny::observeEvent(input$FunctionSelect, {
    switch (input$FunctionSelect,
            "Add Centroid" = {
              CornerSelection <- gsub(pattern = "_x",
                                      replacement = "",
                                      x = colnames(CoordList)[grepl(pattern = "_x", x = colnames(CoordList))])
              output$FunctionInput <- shiny::renderUI(expr = {list(
                shiny::selectInput(inputId = "CornerNames",
                                   label = "Select Corner Variables",
                                   choices = CornerSelection,
                                   multiple = T),
                shiny::textInput(inputId = "OutputName",
                                 label = "Output Name",
                                 placeholder = "type in variable name"))
              })},
            "Angle Calculation" = {
              AngleSelection <- stats::na.omit(gsub(pattern = "_x",
                                             replacement = "",
                                             x = colnames(CoordList)[grepl(pattern = "_x", x = colnames(CoordList))]))
              output$FunctionInput <- shiny::renderUI(expr = {list(
                shiny::selectInput(inputId = "VectorStart",
                                   label = "Select Vector Start",
                                   choices = AngleSelection,
                                   multiple = F),
                shiny::selectInput(inputId = "VectorEnd",
                                   label = "Select Vector End",
                                   choices = AngleSelection,
                                   multiple = F),
                shiny::textInput(inputId = "OutputName",
                                 label = "Output Name",
                                 placeholder = "type in variable name"))
              })},
            "Angle Difference" = {
              AngleSelectionDiff <- stats::na.omit(colnames(CoordList)[unlist(CoordList[,lapply(X = .SD,
                                                                                         FUN = function(x){
                                                                                           ifelse(test = is.numeric(x),
                                                                                                  yes = max(abs(x), na.rm = T)<=pi,
                                                                                                  no = FALSE)
                                                                                         }),])])
              output$FunctionInput <- shiny::renderUI(expr = {list(
                shiny::selectInput(inputId = "Angle1",
                                   label = "Select Angle 1",
                                   choices = AngleSelectionDiff,
                                   multiple = F),
                shiny::selectInput(inputId = "Angle2",
                                   label = "Select Angle 2",
                                   choices = AngleSelectionDiff,
                                   multiple = F),
                shiny::textInput(inputId = "OutputName",
                                 label = "Output Name",
                                 placeholder = "type in variable name"))
              })},
            "Distance and Speed" = {
              SpeedSelection <- unique(gsub(pattern = "_x|_y",
                                            replacement = "",
                                            x = colnames(CoordList)[grepl(x = colnames(CoordList), pattern = "_x|_y")]))
              output$FunctionInput <- shiny::renderUI(expr = {
                shiny::selectInput(inputId = "SpeedRef",
                                   label = "Select Variable",
                                   choices = SpeedSelection,
                                   multiple = F)
              })},
            "Object Distance" = {
              RefSelection <- unique(gsub(pattern = "_x|_y",
                                          replacement = "",
                                          x = colnames(CoordList)[grepl(x = colnames(CoordList), pattern = "_x|_y")]))
              output$FunctionInput <- shiny::renderUI(expr = {
                shiny::selectInput(inputId = "Ref",
                                   label = "Select Reference Variable",
                                   choices = RefSelection,
                                   multiple = F)
              })},
            "Object Angle" = {
              RefSelection <- unique(gsub(pattern = "_x|_y",
                                          replacement = "",
                                          x = colnames(CoordList)[grepl(x = colnames(CoordList), pattern = "_x|_y")]))
              output$FunctionInput <- shiny::renderUI(expr = {
                shiny::selectInput(inputId = "Ref",
                                   label = "Select Reference Variable",
                                   choices = RefSelection,
                                   multiple = F)
              })},
            "Vector Length" = {
              LengthSelection <- unique(gsub(pattern = "_x|_y",
                                             replacement = "",
                                             x = colnames(CoordList)[grepl(pattern = "_x|_y", x = colnames(CoordList))]))
              output$FunctionInput <- shiny::renderUI(expr = {list(
                shiny::selectInput(inputId = "VectorStart",
                                   label = "Select Vector Start",
                                   choices = LengthSelection,
                                   multiple = F),
                shiny::selectInput(inputId = "VectorEnd",
                                   label = "Select Vector End",
                                   choices = LengthSelection,
                                   multiple = F),
                shiny::textInput(inputId = "OutputName",
                                 label = "Output Name",
                                 placeholder = "type in variable name"))
              })},
            "Zone Entry" = {
              DistanceRefSelection <- colnames(CoordList)[!grepl(x = colnames(CoordList), pattern = "_x|_y")]
              AngleSelectionZone <- stats::na.omit(colnames(CoordList)[unlist(CoordList[,lapply(X = .SD,
                                                                                         FUN = function(x){
                                                                                           ifelse(test = is.numeric(x),
                                                                                                  yes = max(abs(x))<=pi,
                                                                                                  no = FALSE)
                                                                                         }),])])
              output$FunctionInput <- shiny::renderUI(expr = {list(
                shiny::selectInput(inputId = "DistanceRef",
                                   label = "Select Distance Variable",
                                   choices = DistanceRefSelection,
                                   multiple = F),
                shiny::radioButtons(inputId = "AngleInput",
                                    label = "Use Angle",
                                    choices = c("yes", "no"),
                                    selected = "yes"),
                shiny::selectInput(inputId = "AngleRef",
                                   label = "Select pre computed Angle",
                                   choices = AngleSelectionZone,
                                   multiple = F),
                shiny::numericInput(inputId = "AngleRange",
                                    label = "Angle Range",
                                    value = pi*(40/360),
                                    min = 0,
                                    max = 2*pi,
                                    step = pi*(5/360)))
              })})
  })
  
  shiny::observeEvent(input$AnalyseTable, {
    TableUpdate <- F
    switch (input$FunctionSelect,
            "Add Centroid" = {if(!is.null(input$CornerNames) & input$OutputName!="") {
              AddCentroid(CoordTable = CoordList,
                          CornerNames = input$CornerNames,
                          OutputName = input$OutputName,
                          Overwrite = T)
              TableUpdate <- T
            } else {
              shiny::showNotification(ui = "Missing Function Inputs", type = "error")
            }},
            "Angle Calculation" = {if(!is.null(input$VectorStart) & !is.null(input$VectorEnd) & input$OutputName!="") {
              AngleCalc(CoordTable = CoordList,
                        VectorStart = input$VectorStart,
                        VectorEnd = input$VectorEnd,
                        OutputName = input$OutputName,
                        Overwrite = T)
              TableUpdate <- T
            } else {
              shiny::showNotification(ui = "Missing Function Inputs", type = "error")
            }},
            "Angle Difference" = {if(!is.null(input$Angle1) & !is.null(input$Angle2) & input$OutputName!="") {
              AngleDiff(CoordTable = CoordList,
                        Angle1 = input$Angle1,
                        Angle2 = input$Angle2,
                        OutputName = input$OutputName,
                        Overwrite = T)
              TableUpdate <- T
            } else {
              shiny::showNotification(ui = "Missing Function Inputs", type = "error")
            }},
            "Distance and Speed" = {if(!is.null(input$SpeedRef)) {
              DistSpeedCalc(CoordTable = CoordList,
                            SpeedRef = input$SpeedRef,
                            Interval = 1/input$FrameRate,
                            ReferenceColumn = "FileName",
                            Overwrite = T)
              TableUpdate <- T
            } else {
              shiny::showNotification(ui = "Missing Function Inputs", type = "error")
            }},
            "Object Distance" = {if(!is.null(input$Ref)) {
              ObjectDistance(CoordTable = CoordList,
                             ObjectTable = ObjectList,
                             Ref = input$Ref,
                             ReferenceColumn = "FileName",
                             Overwrite = T)
              TableUpdate <- T
            } else {
              shiny::showNotification(ui = "Missing Function Inputs", type = "error")
            }},
            "Object Angle" = {if(!is.null(input$Ref)) {
              ObjectAngle(CoordTable = CoordList,
                          ObjectTable = ObjectList,
                          Ref = input$Ref,
                          ReferenceColumn = "FileName",
                          Overwrite = T)
              TableUpdate <- T
            } else {
              shiny::showNotification(ui = "Missing Function Inputs", type = "error")
            }},
            "Vector Length" = {if(!is.null(input$VectorStart) & !is.null(input$VectorEnd) & input$OutputName!="") {
              VectorLength(CoordTable = CoordList,
                           VectorStart = input$VectorStart,
                           VectorEnd = input$VectorEnd,
                           OutputName = input$OutputName,
                           Overwrite = T)
              TableUpdate <- T
            } else {
              shiny::showNotification(ui = "Missing Function Inputs", type = "error")
            }},
            "Zone Entry" = {if(!is.null(input$Ref) & !is.null(input$AngleRef) & input$AngleRange>0) {
              ZoneEntry(CoordTable = CoordList,
                        DistanceRef = input$DistanceRef,
                        Length = 10, #find if reference is needed or how to get length of every animal
                        AngleInclusion = ifelse(test = input$AngleInput == "yes",
                                                yes = TRUE,
                                                no = FALSE), 
                        AngleRef = input$AngleRef,
                        AngleRange = input$AngleRange,
                        Overwrite = T)
              TableUpdate <- T
            } else {
              shiny::showNotification(ui = "Missing Function Inputs", type = "error")
            }})
    if(TableUpdate) {
      output$Table <- DT::renderDataTable({
        DTSummary <- CoordList[,lapply(X = .SD, function(x){
          Type <- typeof(x)
          Min <- ifelse(Type=="character", "-", round(min(x, na.rm = T), digits = 2))
          Max <- ifelse(Type=="character", "-", round(max(x, na.rm = T), digits = 2))
          Mean <- ifelse(Type=="character", "-", round(mean(x, na.rm = T), digits = 2))
          Na_Count <- ifelse(Type=="character", "-", sum(is.na(x)))
          c(Type, Min, Mean, Max, Na_Count)})]
        DTSummary[,ColumnNames:=c("Type", "Min", "Mean", "Max", "Na Count"),]
        DT::datatable(data = dcast(melt(DTSummary, id.vars = "ColumnNames"), variable ~ ColumnNames),
                      style = "bootstrap",
                      escape = F,
                      options = list(paging = TRUE,
                                     pageLength = 25))
      })
      ColumnNamesSelect <- colnames(CoordList)
      ColumnNamesSelect <- unique(gsub(pattern = "_x|_y", replacement = "", x = ColumnNamesSelect[-grep(pattern = "frame|Time", x = ColumnNamesSelect)]))
      shiny::updateSelectInput(session = session,
                               inputId = "DeleteSelection",
                               label = "Select Variable",
                               choices = ColumnNamesSelect)
    }
  })
  
  shiny::observeEvent({
    input$AnalyseTable
    input$nextTabAnalysis
    input$deleteColumn
  },{
    ColumnNamesSelect <- colnames(CoordList)
    ColumnNamesSelect <- unique(gsub(pattern = "_x|_y", replacement = "", x = ColumnNamesSelect[-grep(pattern = "frame|Time", x = ColumnNamesSelect)]))
    shiny::updateSelectInput(session = session,
                             inputId = "DeleteSelection",
                             label = "Select Variable",
                             choices = ColumnNamesSelect)
  })
  
  shiny::observeEvent(input$deleteColumn, {
    DeleteVariables <- colnames(CoordList)[unlist(lapply(X = input$DeleteSelection, FUN = function(i){
      tmpColname <- which(colnames(CoordList)==i)
      if(length(tmpColname)==0){
        tmpColname <- c(which(colnames(CoordList)==paste0(i, "_x")),which(colnames(CoordList)==paste0(i, "_y"))) 
      }
      return(tmpColname)
    }))]
    if(length(DeleteVariables)>0) {
      CoordList[,eval(DeleteVariables):=NULL,]
      output$Table <- DT::renderDataTable({
        DTSummary <- CoordList[,lapply(X = .SD, function(x){
          Type <- typeof(x)
          Min <- ifelse(Type=="character", "-", round(min(x, na.rm = T), digits = 2))
          Max <- ifelse(Type=="character", "-", round(max(x, na.rm = T), digits = 2))
          Mean <- ifelse(Type=="character", "-", round(mean(x, na.rm = T), digits = 2))
          NA_Count <- ifelse(Type=="character", "-", sum(is.na(x)))
          c(Type, Min, Mean, Max, NA_Count)})]
        DTSummary[,ColumnNames:=c("Type", "Min", "Mean", "Max", "NA Count"),]
        DT::datatable(data = dcast(melt(DTSummary, id.vars = "ColumnNames"), variable ~ ColumnNames),
                      style = "bootstrap",
                      escape = F,
                      options = list(paging = TRUE,
                                     pageLength = 25))})
      ColumnNamesSelect <- colnames(CoordList)
      ColumnNamesSelect <- unique(gsub(pattern = "_x|_y", replacement = "", x = ColumnNamesSelect[-grep(pattern = "frame|Time", x = ColumnNamesSelect)]))
      shiny::updateSelectInput(session = session,
                               inputId = "DeleteSelection",
                               label = "Select Variable",
                               choices = ColumnNamesSelect)
    }
  })
  
  shiny::observeEvent(input$nextTabPlotting,
                      shiny::updateTabsetPanel(session, "inTabset", selected = "Plot Data"))
  shiny::observeEvent({
    input$nextTabPlotting
    input$AnalyseTable
    input$nextTabAnalysis}, {
      AngleCheck <- stats::na.omit(colnames(CoordList)[unlist(CoordList[,lapply(X = .SD,
                                                                         FUN = function(x){
                                                                           ifelse(test = is.numeric(x),
                                                                                  yes = max(abs(x))<=pi,
                                                                                  no = FALSE)
                                                                         }),])])
      if(length(AngleCheck)==0) {
        PlotSelections <- c("Distance", "Length", "Location", "Speed")
      } else {
        PlotSelections <- c("Angle", "Distance", "Length", "Location", "Speed")
      }
      output$PlottingFunctions <- shiny::renderUI(expr = {list(shiny::selectInput(inputId = "PlottingSelect",
                                                                                  label = "Select Plot",
                                                                                  choices = PlotSelections),
                                                              shiny::sliderInput("range", 
                                                                           label = "Time Range:",
                                                                           min = floor(CoordList[,min(Time),]),
                                                                           max = ceiling(CoordList[,max(Time),]),
                                                                           value = c(floor(CoordList[,min(Time),]), ceiling(CoordList[,max(Time),])),
                                                                           step = 1))
      })
      shiny::updateSelectInput(session = session,
                               inputId = "FileChoices",
                               label = "Files:",
                               choices = FileList$name)
    })
  
  shiny::observeEvent(input$FileChoices,{
    shiny::updateSliderInput(session = session,
                      inputId = "range",
                      label = "Time Range:",
                      min = floor(CoordList[FileName %in% input$FileChoices,min(Time),]),
                      max = ceiling(CoordList[FileName %in% input$FileChoices,max(Time),]),
                      value = c(floor(CoordList[FileName %in% input$FileChoices,min(Time),]),
                                ceiling(CoordList[FileName %in% input$FileChoices,max(Time),])), 
                      step = 1)
  })
  
  shiny::observeEvent({
    input$PlottingSelect
  }, {
    switch (input$PlottingSelect,
            "Angle" = {
              AngleSelection <- stats::na.omit(colnames(CoordList)[unlist(CoordList[,lapply(X = .SD,
                                                                                     FUN = function(x){
                                                                                       ifelse(test = is.numeric(x),
                                                                                              yes = max(abs(x))<=pi,
                                                                                              no = FALSE)
                                                                                     }),])])
              CoordSelection <- c("",gsub(pattern = "_x",
                                          replacement = "",
                                          x = colnames(CoordList)[grepl(pattern = "_x", x = colnames(CoordList))]))
              output$PlotInput <- shiny::renderUI(expr = {list(
                shiny::selectInput(inputId = "AngleChoice",
                                   label = "Select Angle",
                                   choices = AngleSelection,
                                   multiple = F),
                shiny::selectInput(inputId = "CoordSelection",
                                   label = "Select Object (for 2D)",
                                   choices = CoordSelection,
                                   multiple = F),
                shiny::radioButtons(inputId = "ColourScheme",
                                    label = "Colour Scheme",
                                    choices = c("dark", "light")),
                shiny::selectInput(inputId = "ObjectHighlight",
                                   label = "Highlight Object",
                                   choices = c("alpha", "colour", "stroke", "none"),
                                   selected = "alpha",
                                   multiple = F))
              })},
            "Distance" = {
              DistRefSelection <- c("",unique(colnames(CoordList)[!grepl(pattern = "_x|_y", x = colnames(CoordList))]))
              CoordSelection <- c("",gsub(pattern = "_x",
                                          replacement = "",
                                          x = colnames(CoordList)[grepl(pattern = "_x", x = colnames(CoordList))]))
              output$PlotInput <- shiny::renderUI(expr = {list(
                shiny::selectInput(inputId = "DistanceRef",
                                   label = "Select Distance Variable",
                                   choices = DistRefSelection,
                                   multiple = F),
                shiny::selectInput(inputId = "CoordSelection",
                                   label = "Select Object (for 2D)",
                                   choices = CoordSelection,
                                   multiple = F),
                shiny::textInput(inputId = "Unit",
                                 label = "Unit",
                                 value = "px"))
              })},
            "Length" = {
              LengthRefSelection <- c("",unique(colnames(CoordList)[!grepl(pattern = "_x|_y", x = colnames(CoordList))]))
              CoordSelection <- c("",gsub(pattern = "_x",
                                          replacement = "",
                                          x = colnames(CoordList)[grepl(pattern = "_x", x = colnames(CoordList))]))
              output$PlotInput <- shiny::renderUI(expr = {list(
                shiny::selectInput(inputId = "LengthRef",
                                   label = "Select Length Variable",
                                   choices = LengthRefSelection,
                                   multiple = F),
                shiny::selectInput(inputId = "CoordSelection",
                                   label = "Select Object (for 2D)",
                                   choices = CoordSelection,
                                   multiple = F),
                shiny::textInput(inputId = "Unit",
                                 label = "Unit",
                                 value = "px"),
                shiny::radioButtons(inputId = "ColourFlip",
                                    label = "Reverse Colour Scheme",
                                    choices = c("yes", "no"),
                                    selected = "yes"))
              })},
            "Location" = {
              CoordSelection <- c("",gsub(pattern = "_x",
                                          replacement = "",
                                          x = colnames(CoordList)[grepl(pattern = "_x", x = colnames(CoordList))]))
              output$PlotInput <- shiny::renderUI(expr = {list(
                shiny::selectInput(inputId = "CoordSelection",
                                   label = "Select Object (for 2D)",
                                   choices = CoordSelection,
                                   multiple = F),
                shiny::radioButtons(inputId = "Density",
                                    label = "Density Map",
                                    choices = c("yes", "no"),
                                    selected = "yes"),
                shiny::numericInput(inputId = "BinNumber",
                                    label = "Number of Bins",
                                    value = 500,
                                    min = 5,
                                    step = 10))
              })},
            "Speed" = {
              SpeedRefSelection <- c("",unique(colnames(CoordList)[!grepl(pattern = "_x|_y", x = colnames(CoordList))]))
              CoordSelection <- c("",gsub(pattern = "_x",
                                          replacement = "",
                                          x = colnames(CoordList)[grepl(pattern = "_x", x = colnames(CoordList))]))
              output$PlotInput <- shiny::renderUI(expr = {list(
                shiny::selectInput(inputId = "SpeedRefCalc",
                                   label = "Select Speed Variable",
                                   choices = SpeedRefSelection,
                                   multiple = F),
                shiny::selectInput(inputId = "CoordSelection",
                                   label = "Select Object (for 2D)",
                                   choices = CoordSelection,
                                   multiple = F),
                shiny::textInput(inputId = "Unit",
                                 label = "Unit",
                                 value = "px/s"))
              })})
  })
  
  shiny::observeEvent(input$startPlot, {
    if(is.null(input$CoordSelection) | (nchar(input$CoordSelection)==0)) {
      CoordInput <- NULL
    } else {
      if(sum(grepl(pattern = paste0(input$CoordSelection, "_x"), x = colnames(CoordList), fixed = T))==1) {
        CoordInput <- input$CoordSelection
      } else {
        CoordInput <- NULL 
      }
    }
    PlotUpdate <<- F
    switch (input$PlottingSelect,
            "Angle" = {if(!is.null(input$FileChoices) &
                          !is.null(input$AngleChoice) &
                          !is.null(input$ColourScheme) &
                          !is.null(input$ObjectHighlight) &
                          !is.null(input$ObjectChoices)) {
              OutputPlot <<- AnglePlot(DataTable = CoordList[FileName %in% input$FileChoices & data.table::between(x = Time, lower = input$range[1], upper = input$range[2]),],
                                       Angle = input$AngleChoice,
                                       CoordRef = CoordInput,
                                       ObjectTable = ObjectList[FileName %in% input$FileChoices,],
                                       colourScheme = input$ColourScheme,
                                       ObjectHighlight = input$ObjectHighlight,
                                       FacetRef = "FileName")
              PlotUpdate <<- T
            } else if(!is.null(input$FileChoices) &
                      !is.null(input$AngleChoice) &
                      !is.null(input$ColourScheme) &
                      is.null(input$ObjectChoices)) {
              OutputPlot <<- AnglePlot(DataTable = CoordList[FileName %in% input$FileChoices & data.table::between(x = Time, lower = input$range[1], upper = input$range[2]),],
                                       Angle = input$AngleChoice,
                                       CoordRef = CoordInput,
                                       colourScheme = input$ColourScheme,
                                       FacetRef = "FileName")
              PlotUpdate <<- T
            } else { 
              shiny::showNotification(ui = "Missing Function Inputs", type = "error")
            }},
            "Distance" = {if(!is.null(input$FileChoices) &
                             !is.null(input$DistanceRef) &
                             !is.null(input$ObjectChoices) &
                             input$Unit!="") {
              OutputPlot <<- DistancePlot(DataTable = CoordList[FileName %in% input$FileChoices & data.table::between(x = Time, lower = input$range[1], upper = input$range[2]),],
                                          Distance = input$DistanceRef,
                                          CoordRef = CoordInput,
                                          ObjectTable = ObjectList[FileName %in% input$FileChoices,],
                                          Unit = input$Unit,
                                          FacetRef = "FileName")
              PlotUpdate <<- T
            } else if(!is.null(input$FileChoices) &
                      !is.null(input$DistanceRef) &
                      is.null(input$ObjectChoices) &
                      input$Unit!="") { 
              OutputPlot <<- DistancePlot(DataTable = CoordList[FileName %in% input$FileChoices & data.table::between(x = Time, lower = input$range[1], upper = input$range[2]),],
                                          Distance = input$DistanceRef,
                                          CoordRef = CoordInput,
                                          Unit = input$Unit,
                                          FacetRef = "FileName")
              PlotUpdate <<- T
            } else {
              shiny::showNotification(ui = "Missing Function Inputs", type = "error")
            }},
            "Length" = {if(!is.null(input$FileChoices) &
                           !is.null(input$LengthRef) &
                           !is.null(input$ObjectChoices) &
                           input$Unit!="" &
                           !is.null(input$ColourFlip)) {
              OutputPlot <<- LengthPlot(DataTable = CoordList[(FileName %in% input$FileChoices) & data.table::between(x = Time, lower = input$range[1], upper = input$range[2]),],
                                        Length = input$LengthRef,
                                        CoordRef = CoordInput,
                                        ObjectTable = ObjectList[FileName %in% input$FileChoices,],
                                        Unit = input$Unit,
                                        ColourFlip = ifelse(input$ColourFlip=="yes", TRUE, FALSE),
                                        FacetRef = "FileName")
              PlotUpdate <<- T
            } else if(!is.null(input$FileChoices) &
                      !is.null(input$LengthRef) &
                      is.null(input$ObjectChoices) &
                      input$Unit!="" &
                      !is.null(input$ColourFlip)) { 
              OutputPlot <<- LengthPlot(DataTable = CoordList[FileName %in% input$FileChoices & data.table::between(x = Time, lower = input$range[1], upper = input$range[2]),],
                                        Length = input$LengthRef,
                                        CoordRef = CoordInput,
                                        Unit = input$Unit,
                                        ColourFlip = ifelse(input$ColourFlip=="yes", TRUE, FALSE),
                                        FacetRef = "FileName")
              PlotUpdate <<- T
            } else {
              shiny::showNotification(ui = "Missing Function Inputs", type = "error")
            }},
            "Location" = {if(!is.null(input$FileChoices) &
                             !is.null(input$ObjectChoices) &
                             !is.null(CoordInput)) {
              OutputPlot <<- LocationPlot(DataTable = CoordList[FileName %in% input$FileChoices & data.table::between(x = Time, lower = input$range[1], upper = input$range[2]),],
                                          CoordRef = CoordInput,
                                          ObjectTable = ObjectList[FileName %in% input$FileChoices,],
                                          Density = ifelse(input$Density=="yes", TRUE, FALSE),
                                          BinNumber = input$BinNumber,
                                          FacetRef = "FileName")
              PlotUpdate <<- T
            } else if(!is.null(input$FileChoices) &
                      is.null(input$ObjectChoices) &
                      !is.null(CoordInput)) { 
              OutputPlot <<- LocationPlot(DataTable = CoordList[FileName %in% input$FileChoices & data.table::between(x = Time, lower = input$range[1], upper = input$range[2]),],
                                          CoordRef = CoordInput,
                                          Density = ifelse(input$Density=="yes", TRUE, FALSE),
                                          BinNumber = input$BinNumber,
                                          FacetRef = "FileName")
              PlotUpdate <<- T
            } else {
              shiny::showNotification(ui = "Missing Function Inputs", type = "error")
            }},
            "Speed" = {if(!is.null(input$FileChoices) &
                          !is.null(input$SpeedRefCalc) &
                          !is.null(input$ObjectChoices) &
                          input$Unit!="") {
              OutputPlot <<- SpeedPlot(DataTable = CoordList[FileName %in% input$FileChoices & data.table::between(x = Time, lower = input$range[1], upper = input$range[2]),],
                                       Speed = input$SpeedRefCalc,
                                       CoordRef = CoordInput,
                                       ObjectTable = ObjectList[FileName %in% input$FileChoices,],
                                       Unit = input$Unit,FacetRef = "FileName")
              PlotUpdate <<- T
            } else if(!!is.null(input$FileChoices) &
                      !is.null(input$SpeedRefCalc) &
                      is.null(input$ObjectChoices) &
                      input$Unit!="") { 
              OutputPlot <<- SpeedPlot(DataTable = CoordList[FileName %in% input$FileChoices & data.table::between(x = Time, lower = input$range[1], upper = input$range[2]),],
                                       Speed = input$SpeedRefCalc,
                                       CoordRef = CoordInput,
                                       ObjectTable = ObjectList[FileName %in% input$FileChoices,],
                                       Unit = input$Unit,FacetRef = "FileName")
              PlotUpdate <<- T
            } else {
              shiny::showNotification(ui = "Missing Function Inputs", type = "error")
            }})
    if(PlotUpdate) {
      output$PlotOut <- shiny::renderPlot(OutputPlot)
    }
  })
  
  
  shinyFiles::shinyDirChoose(
    input,
    'dirPlot',
    roots = c(home = "~"),
    filetypes = c("text", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf")
  )
  
  globalP <- shiny::reactiveValues(plotpath = "~")
  
  dirPlot <- shiny::reactive(input$dirPlot)
  
  output$dirPlot <- shiny::renderText({
    globalP$plotpath
  })
  
  shiny::observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dirPlot
               },
               handlerExpr = {
                 if (!"path" %in% names(dirPlot())) return()
                 home <- normalizePath("~")
                 globalP$plotpath <-
                   file.path(home, paste(unlist(dirPlot()$path[-1]), collapse = .Platform$file.sep))
               })
  
  shiny::observeEvent(input$savePlot, {
    if(!is.null(OutputPlot) & PlotUpdate) {
      ggplot2::ggsave(filename = paste(globalP$plotpath, paste0(input$PlottingSelect,"_", input$CoordSelection, ".",input$ExportFormat), sep = .Platform$file.sep),
                      plot = OutputPlot,
                      device = input$ExportFormat,
                      width = input$widthPlot,
                      height = input$heightPlot,
                      units = input$exportUnits,
                      dpi = input$dpi)
      shiny::showNotification(ui = paste(paste(globalP$plotpath, paste0(input$PlottingSelect,"_", input$CoordSelection, ".",input$ExportFormat), "Saved to")), type = "message")
    }
  })
  
  shinyFiles::shinyDirChoose(
    input,
    'dir',
    roots = c(home = "~"),
    filetypes = c("xls", "txt", "csv", "tsv", "mat", "rds", "rda","pdf", "png", "svg", "eps")
  )
  
  global <- shiny::reactiveValues(datapath = "~")
  
  dir <- shiny::reactive(input$dir)
  
  output$dir <- shiny::renderText({
    global$datapath
  })
  
  shiny::observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
               })
  
  shiny::observeEvent(input$SaveTables, {
    if(length(input$OutputSelection)>0 & nchar(input$FileType)>0) {
      FileFrame <- data.frame(data=c("CoordList", "ObjectList", "AllList"),
                              FileOut=paste(global$datapath, c("CoordList", "ObjectList", "AllList"), sep = .Platform$file.sep),
                              StringLabel=c("Coordinate Table", "Object Table", "Table with extra Parameters"))
      FileFrame <- FileFrame[FileFrame$StringLabel %in% input$OutputSelection,c(1:2)]
      switch (input$FileType,
              "csv" = {
                for(i in seq_along(FileFrame$data)) {
                  data.table::fwrite(x = get(FileFrame$data[i]), file = paste0(FileFrame$FileOut[i], ".csv"))
                }
              },
              "rds (R)" = {
                for(i in seq_along(FileFrame$data)) {
                  saveRDS(object = get(FileFrame$data[i]), file = paste0(FileFrame$FileOut[i], ".rds"))
                }
              },
              "npy (Python)" = {
                shiny::showNotification(ui = "npy is currently not supported", type = "message")
                #for(i in seq_along(FileFrame$data)) {
                #  RcppCNPy::npySave(object = get(FileFrame$data[i]), filename = paste0(FileFrame$FileOut[i], ".npy"))
                #}
              },
              "mat (Matlab)" = {
                shiny::showNotification(ui = "npy is currently not supported", type = "message")
                #for(i in seq_along(FileFrame$data)) {
                #  R.matlab::writeMat(paste0(FileFrame$FileOut[i], ".mat"), get(FileFrame$data[i]))
                #}
              }
      )
    } else {
      shiny::showNotification(ui = "Missing Inputs", type = "error")
    }
    
  })
}

#' Start Analysis Application
#'
#' This function calls the Behaviour user interface which allows loading DeepLabCut output, analysing, plotting, and exporting data.
#' @export
StartApp <- function() {shiny::shinyApp(ui = ui, server = server)}

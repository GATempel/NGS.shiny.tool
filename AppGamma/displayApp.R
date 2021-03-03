# eng:
# This is the third iteration and gamma developer version of a shiny web App
# designed to process and display the results of Illumina NGS data.
# It is meant to provide an easy to understand and use interface for biologists
# who need to process these kind of data without having to need the knowledge
# of advanced R programming.
# This version splits the processing and the visualisation into two distinct
# Apps while providing a combined version as well.
# For more details on shiny web Apps see:
# https://shiny.rstudio.com/tutorial/
# This App was created by Gregor in 2020.
#
# de:
# Dies ist die dritte und damit Gamma Version einer Shiny web App im Entwickler
# Stadium. Ziel der App ist es die Ergebnisse von Illumina NGS zu verarbeiten
# und dann darzustellen. Das einfach zu verstehende und zu benutzende Interface
# ist für Biologen gedacht, welche solche Daten verarbeiten müssen, auch wenn
# sie keine Programmierkenntnisse haben.
# Diese Version teilt die Prozessierung und die Darstellung in zwei seperate
# web Apps. (Eine kombinierte Version existiert auch.)
# Weitere Details zu shiny könner hier:
# https://shiny.rstudio.com/tutorial/
# nachgelesen werden.
# Diese App wurde 2020 von Gregor geschrieben.

#------------------------------------------------------------------------------------>Initialize requirements
# load requirements for the App to work
# shiny is used for interactive webApps
require("shiny")
# sinyFiles is used for the handling of directories within shiny by enabling
# buttons that can be used for selecting and uploading directories
require("shinyFiles")
# shinyBS enables further use of Bootstraps
require("shinyBS")
# shinyjs enables advanced use of java within the shiny App
require("shinyjs")
# readr is used for advanced file reading
require("readr")
# DT is used for a better display of tables
require("DT")
# cooccur is required for the Co-occurance calculations
require("cooccur")
# plotly for the displaying of interactive plotly plots
require("plotly")


# load the internal html files
Error000 <- read_file("./internal_files/html/ErrorV000.html")
Error001 <- read_file("./internal_files/html/ErrorV001.html")
Instructions <- read_file("./internal_files/html/InstrucV.html")

#------------------------------------------------------------------------------------>User Interface
ui <- tagList(
  # initialize ShinyJs to enable Java Script usage within the App
  useShinyjs(),
  # initialize tags to define HTML behavior of certain elements
  tags$head(tags$style(HTML(".tooltip {width: 300px;}"))),
  # define behavior of actual User Interface
  # set the content of each tab as a uiOutput to create a reactive interactive
  # User Interface that changes according to the user input
  navbarPage("Visualization 0.3.0",
             tabPanel("Data Visualization",
                      uiOutput("DataVisual")),
             tabPanel("FAQ",
                      uiOutput("FAQ")),
             tabPanel("SessionInfo",
                      uiOutput("SessionInfo"))
             )

)
#------------------------------------------------------------------------------------>Server Functions
server <- function(input, output, session) {
  #------------------------------------------------------->Data Visualization UI
  output$DataVisual <- renderUI({
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          uiOutput("DVside1")
        ),
        mainPanel(
          uiOutput("DVmain")
        )

      )#end of sidebar Layout
    )#end of FluidPage
  })

  #---------------------------->Define the contents of the sidebar panel
  # first element of the sidebar:
  # the folder selection
  # only after the folder is selected the rest of the sidebar will be available
  output$DVside1 <- renderUI({
    fluidPage(
      fluidRow(
        uiOutput("DVside2")
      ),
      fluidRow(
        "Please select a folder.",
        shinyDirButton("File",
                       "Folder selection",
                       "Select a folder containing a
                       IlluminaAnalysis.RData file")

      )
    )
  })

  output$DVside2 <- renderUI({
    if (Status() == "acceptable"){
      fluidPage(
        fluidRow(
          column(12,
                 # create a select input to enable the choice of data
                 # visualization
                 selectInput("VisC",
                             "What do you want to display?",
                             choices = list(
                               "An abundance barplot" = "BarP",
                               "A heatmap" = "hmap",
                               "A phylogenetic tree" = "ptree",
                               "An overview of the co-occurance" = "CoocOV",
                               "A specific co-occurance table" = "CoocDT"
                             ))
          )
        ),
        fluidRow(
          # create a sub UI dependent on the user input
          uiOutput("SampleSelect")
        )
      )
    }
  })

  output$SampleSelect <- renderUI({
    # render the UI depending on the chosen display variant - all choices
    # except the data Table give the options to choose the samples which should
    # be analyzed as well as the dimensions of the plot within the mainpanel
    if (input$VisC != "CoocDT"){
      fluidPage(
        fluidRow(
          column(12,
                 checkboxGroupInput(
                   "SampleC",
                   "Select the samples you want to analyse",
                   choices = as.list(CompletePhyl
                                     @sam_data
                                     @row.names),
                   inline = TRUE,
                   selected = CompletePhyl@sam_data@row.names
                   )
                 )
        ),
        fluidRow(
          # another sub Ui dependent on the user input
          uiOutput("TopXSelect")
        ),
        fluidRow(
          column(6,
                 # input to define the width of the plot
                 selectInput("PWidth",
                             "Plot width",
                             choices = list("100%" = "100%",
                                            "800px" = "800px",
                                            "1200px" = "1200px",
                                            "1600px" = "1600px"),
                             selected = "100%")
          ),
          column(6,
                 # input to define the height of the plot
                 selectInput("PHeight",
                             "Plot height",
                             choices = list("600px" = "600px",
                                            "900px" = "900px",
                                            "1200px" = "1200px"),
                             selected = "600px")
          )
        )
      )
    } else {
      TRank <- colnames(CompletePhyl@tax_table)
      fluidPage(
        fluidRow(
          column(6,
                 # input that has all but the highest taxonomic rank as an
                 # option
                 selectInput("DTRank",
                             "Select the taxonomic rank by which to
                                    calculate the cooccurance",
                             choices = as.list(TRank[2:length(TRank)]),
                             selected = TRank[length(TRank) - 1])),
          column(6,
                 # sub UI dependent on the input of the DTRank
                 uiOutput("CoocSample"))
        )
      )
    }
  })

  output$CoocSample <- renderUI({
    # render an input that offers all 'species' names as options
    selectInput("SpecSelect",
                "Select the phylogenetic unit you want to inspect",
                choices = as.list(DTRankC()$Co_Occurance$spp.names))
  })

  output$TopXSelect <- renderUI({
    TRank <- colnames(CompletePhyl@tax_table)
    if (input$VisC == "CoocOV")
    {
      fluidPage(
        fluidRow(
          column(6,
                 sliderInput("TopX",
                             "Top X sequences to plot:",
                             max = 400,
                             min = 1,
                             value = c(1, 15) )
          ),
          column(6,
                 selectInput("TRankC",
                             "Select the taxonomic rank by which to
                                    calculate the cooccurance",
                             choices = as.list(TRank[2:length(TRank)]),
                             selected = TRank[length(TRank) - 1])
          )
        )
      )
    } else if (input$VisC == "hmap"){
      fluidPage(
        fluidRow(
          column(6,
                 sliderInput("TopX",
                             "Top X sequences to plot:",
                             max = 400,
                             min = 1,
                             value = c(1, 15) )
          ),
          column(6,
                 selectInput("TRankC",
                             "Select the taxonomic rank to display",
                             choices = as.list(TRank[2:length(TRank)]),
                             selected = TRank[length(TRank) - 1])
          )
        ),
        fluidRow(
          uiOutput("AdditionalOptions")
        )
      )
    } else if (length(CompletePhyl@phy_tree$tip.label) > 20){
      fluidPage(
        fluidRow(
          column(6,
                 numericInput("TopX",
                              "Top X sequences to plot:",
                              value = 20,
                              max = length(CompletePhyl@phy_tree$tip.label),
                              min = 1 )
          ),
          column(6,
                 selectInput("TRankC",
                             "Select the relevant taxonomic rank",
                             choices = as.list(TRank[1:length(TRank)]),
                             selected = TRank[length(TRank) - 1])
          )
        ),
        fluidRow(
          uiOutput("AdditionalOptions")
        )
      )
    } else {
      fluidPage(
        fluidRow(
          column(6,
                 numericInput("TopX",
                              "Top X sequences to plot:",
                              value = length(CompletePhyl
                                             @phy_tree
                                             $tip.label) %/% 4,
                              max = length(CompletePhyl@phy_tree$tip.label),
                              min = 1 )
          ),
          column(6,
                 selectInput("TRankC",
                             "Select the relevant taxonomic rank",
                             choices = as.list(TRank[1:length(TRank)]),
                             selected = TRank[length(TRank) - 1])
          )
        ),
        fluidRow(
          uiOutput("AdditionalOptions")
        )
      )
    }
  })

  output$AdditionalOptions <- renderUI({
    TRank <- colnames(CompletePhyl@tax_table)
    if (input$VisC == "BarP") {
      fluidPage(
        fluidRow(
          column(12,
                 radioButtons("BarMode",
                              "Display Modus",
                              c("total abundance" = "total",
                                "hide others" = "hide_others",
                                "absolute Values" = "absolute",
                                "default" = "default"),
                              selected = "default")
                 )
          )
      )
    } else if (input$VisC == "hmap") {

    } else if (input$VisC == "CoocOV") {}
  })

  sam_choice <- reactive(input$SampleC)
  DTRankC <- reactive(get(input$DTRank))
  CoocTable <- reactive(cooccur::pair(DTRankC()$Co_Occurance,
                                      input$SpecSelect))

  #------------------------------->Define the contents of the main panel
  output$DVmain <- renderUI({
    fluidPage(
      fluidRow(
        # another UI output that is dependent on specific inputs
        uiOutput("DVmain2")
      ),
      hr(),
      fluidRow(
        if(Status() == "Error000"){
          HTML(Error000)
        } else if (Status() == "Error001"){
          HTML(Error001)
        } else {
          HTML(Instructions)
        }
      )
    )

  })

  output$DVmain2 <- renderUI({
    if(Status() == "acceptable"){
      if(input$VisC == "CoocDT"){
        if(nrow(CoocTable()) == 0){
          uiOutput("NoCoocT")
        } else {
          DTOutput("CoocT")
        }
      } else {
        plotlyOutput("plot",
                     width = input$PWidth,
                     height = input$PHeight)
      }
    }
  })

  output$CoocT <- renderDT({
    CoocTable()
  })

  output$NoCoocT <- renderText({
    HTML("The <strong>",
         input$DTRank,
         input$SpecSelect,
         " </strong> shows no Co-occurance. <br>
         Please select another phylogenetic unit
         to inspect or another taxonomic rank.")
  })

  output$plot <- renderPlotly({
    if (input$VisC == "BarP") {
      AppHelper::plot_barplot(genera = get(input$TRankC),
                              samples = input$SampleC,
                              cutoff = input$TopX,
                              modus = input$BarMode)
    } else if (input$VisC == "hmap") {
      AppHelper::plot_heatmap(genera = get(input$TRankC),
                              samples = input$SampleC,
                              lowercutoff = input$TopX[1],
                              uppercutoff = input$TopX[2])
    }
  })


  #------------------------------------>Data Visualization pure Server functions
  # Define the root for the ShinyDirButton "Files"
  shinyDirChoose(input,
                 "File",
                 roots = c(home = "~"))

  # assign the path of the chosen directory from the "Files" DirButton to the
  # reactive variable DirChoice
  DirChoice <- reactive(as.character(parseDirPath(c(home = "~"),
                                                  input$File)))

  # create Status reactive variable to check the condition of the supplied
  # directory input and its contents
  Status <- reactive({
    if (is.list(input$File)){
      if (file.exists(paste0(DirChoice(),
                             "/IlluminaAnalysis.RData"))){
        # load the file into the global environment
        load(paste0(DirChoice(),
                    "/IlluminaAnalysis.RData"),
             envir = .GlobalEnv)
        "acceptable"
      } else {
        # Error 001 means that the selected folder does not contain the
        # required IlluminaAnalysis.RData file
        "Error001"
      }
    } else {
      # Error 000 means that there is no folder selected yet
      "Error000"
    }
  })


}
#------------------------------------------------------------------------------------>App
shinyApp(ui, server)

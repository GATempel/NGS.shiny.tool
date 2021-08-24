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
# NGS.shiny.helper for a variety of helper functions
require("NGS.shiny.helper")
# phyloseq options used in the generation of the tree
require("phyloseq")
# shinycssloader for the generation on loading animations
require("shinycssloaders")


# load the internal html files
Error000 <- read_file("./internal_files/html/ErrorV000.html")
Error001 <- read_file("./internal_files/html/ErrorV001.html")
Instructions <- read_file("./internal_files/html/InstrucV.html")
NoTree <- read_file("./internal_files/html/NoTreeV.html")
OldTree <- read_file("./internal_files/html/OldTreeV.html")

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
  # data visualisation contains a basic siebar and a main panel with the sidebar
  # containing the selectable options to define what to display within the main
  # panel
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

  # second sidebar element
  # is anly enabled if a folder containing a "IlluminaAnalysis.RData" file was
  # selected - this element enables the selection of what to display in the main
  # panel
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
    # the selection of which Samples to visualize is disabled for the Co-occurance
    # and the phylogenetic tree
    # be analyzed as well as the dimensions of the plot within the mainpanel
    if (!(input$VisC %in% c("CoocDT", "CoocOV"))){
      fluidPage(
        fluidRow(
          # generates a checkbox Input enabling the selection of samples that are to
          # be included in the visualisation - the choices presented will depend
          # on the loaded IlluminaAnalysis.RData file in the previously selected
          # folder and the CompletePhyl object contained within that file
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
          # a subUI that will enable the choice of how many of the most abundant
          # sequence are to be included in the plot - the subUI will vary depending
          # on what the user chose to display
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
    # if the chosen visualization is the Co-occurance datatable the sidebar will
    # contain the choice to select a taxonomic rank and depending on this choice
    # another subUI element containing the choice of which member of the chosen
    # taxonomic rank to inspect
    } else if (input$VisC == "CoocDT") {
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

    } else {
      fluidPage(
        fluidRow(
          uiOutput("TopXSelect")
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
          column(12,
                 selectInput("TRankC",
                             "Select the taxonomic rank by which to
                                    calculate the cooccurance",
                             choices = as.list(TRank[2:length(TRank)]),
                             selected = TRank[length(TRank) - 1])
          )
        ),
        fluidRow(
          uiOutput("AdditionalOptions"))
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
    } else if (input$VisC == "ptree"){
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
                             choices = c(as.list(TRank[2:length(TRank)]),
                                         "Individual ASV" = "taxa_names"),
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
    } else if (input$VisC == "ptree") {
      fluidPage(
        fluidRow(
          column(12,
                 checkboxGroupInput(
                   "treeOptions",
                   "Additional Options",
                   c("Subset tree to specific taxa" = "Subtree",
                     "Hide labels" = "nolabel",
                     "Indicate Abundance" = "Abund")
                 )
                )
        ),
        fluidRow(
          uiOutput("AdditionalOptions2")
        )
      )
    } else if (input$VisC == "CoocOV") {
      chosenRank <- get(input$TRankC)
      fluidPage(
        fluidRow(
          column(12,
                 sliderInput("TopX",
                             "Top X sequences to plot:",
                             max = nrow(chosenRank$Abundance),
                             min = 1,
                             value = c(1, 15) )
          )
        )
      )
    }
  })

  output$AdditionalOptions2 <- renderUI({
    TRank <- colnames(CompletePhyl@tax_table)
    options_chosen <- input$treeOptions
    if("Subtree" %in% options_chosen)
    {
      rankOptions <- (1:length(TRank))
      names(rankOptions) <- TRank
      rankOptions <- rankOptions[-1]
      fluidPage(
        fluidRow(
          column(12,
                 selectInput("SubRank",
                             "Select the taxonomic rank to subset",
                             choices = as.list(rankOptions),
                             selected = TRank[length(TRank) - 1]))
        ),
        fluidRow(
          uiOutput("SubOption")
        )
      )
    }
  })

  output$SubOption <- renderUI({
    TRank <- colnames(CompletePhyl@tax_table)
    Sub_Rank <- get(TRank[as.integer(input$SubRank)])
    fluidPage(
      fluidRow(
        column(12,
               selectInput("SubUnit",
                           "Select the taxonomic unit to subset to",
                           choices = as.list(Sub_Rank$Abundance$Name),
                           selectize = TRUE))

      )
    )
  })

  Sub_Rank <- reactive(input$SubRank)
  sam_choice <- reactive(input$SampleC)
  DTRankC <- reactive(get(input$DTRank))
  CoocTable <- reactive(cooccur::pair(DTRankC()$Co_Occurance,
                                      input$SpecSelect))

  #------------------------------->Define the contents of the main panel
  # the main visualization output - it contains a subUI that is only displayed if
  # a folder containing a usable IlluminaAnalysis.RData file has been selected
  # and will otherwise display error messages depending on what the selected folder
  # does contain
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

  # the subUI for the mainpanel that will be enabled if the IlluminaAnalysis.RData
  # file exists within the selected folder the contents of the main panel will
  # depend on the chosen visualization option
  output$DVmain2 <- renderUI({
    if(Status() == "acceptable"){
      if(input$VisC == "CoocDT"){
        if(nrow(CoocTable()) == 0){
          uiOutput("NoCoocT")
        } else {
          DTOutput("CoocT")
        }
      # enable a plotly output for the heatmap and the barplot visualization
      } else if (input$VisC %in% c("hmap", "BarP"))
      {
        shinycssloaders::withSpinner(
          plotlyOutput("plotlyplot",
                       width = input$PWidth,
                       height = input$PHeight))
      # enable a plot (ggplot) output for the phylogenetic tree option
      } else if (input$VisC == "ptree")
      {
        # enable (ggplot) plot output only if the tree was not skipped
        # first check if skipedTree object exists
        if (exists("skipedTree"))
        {
          # only if the object exists its value can be checked
          # if tree was not skipped create the corresponding plot output
          if (!skipedTree)
          {
            shinycssloaders::withSpinner(
              plotOutput("plot",
                         width = input$PWidth,
                         height = input$PHeight)
            )

          } else
          {
            HTML(NoTree)
          }

        } else
        {
          HTML(OldTree)
        }
      # enable a plot (ggplot) output for the Cooccurance overview option
      } else if (input$VisC == "CoocOV")
      {
        shinycssloaders::withSpinner(plotOutput("plot",
                                                width = input$PWidth,
                                                height = input$PHeight))

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

  output$plotlyplot <- renderPlotly({
    if (input$VisC == "BarP") {
      NGS.shiny.helper::plot_barplot(genera = get(input$TRankC),
                                     samples = input$SampleC,
                                     cutoff = input$TopX,
                                     modus = input$BarMode)
    } else if (input$VisC == "hmap") {
      NGS.shiny.helper::plot_heatmap(genera = get(input$TRankC),
                                     samples = input$SampleC,
                                     lowercutoff = input$TopX[1],
                                     uppercutoff = input$TopX[2])
    }
  })

  output$NoTree <- renderUI({
    if (exists("skipedTree"))
    {
      HTML(NoTree)
    } else
    {
      HTML(OldTree)
    }
  })

  output$plot <- renderPlot({
    if (input$VisC == "ptree") {
      options_chosen <- input$treeOptions
      ChosenSamples <- input$SampleC

      if (length(ChosenSamples > 0))
      {
        treecontent <- CompletePhyl
        oldDF <- as(sample_data(treecontent), "data.frame")
        newDF <- subset(oldDF, Samplename %in% ChosenSamples)
        phyloseq::sample_data(treecontent) <- phyloseq::sample_data(newDF)

        if ("Subtree" %in% options_chosen)
        {
          oldMat <- as(phyloseq::tax_table(treecontent), "matrix")
          old_DF <- data.frame(oldMat)
          new_DF <- subset(old_DF, old_DF[as.integer(input$SubRank)] == input$SubUnit)
          newMat <- as(new_DF, "matrix")
          phyloseq::tax_table(treecontent) <- phyloseq::tax_table(newMat)
        }
        treecontent <- phyloseq::prune_taxa(
          names(
            sort(
              phyloseq::taxa_sums(treecontent),
              decreasing = TRUE
            ))[1:input$TopX],
          treecontent)

        if(is.null(phy_tree(treecontent, FALSE))) {
          # create empty ggplot that only contains text
          ggplot() +
            annotate("text",
                     x = 1,
                     y = 1,
                     size = 6,
                     label = "The taxonomic unit selected for subsetting creates
                     \na phylogenetic tree with only one branch.
                     \nPlease select another unit to display a proper
                     \nphylogenetic tree.") +
            theme_void()
        } else
        {
          if ("nolabel" %in% options_chosen & "Abund" %in% options_chosen)
          {
            phyloseq::plot_tree(treecontent,
                                size = "abundance",
                                ladderize = TRUE,
                                color = "Sample")
          } else if ("Abund" %in% options_chosen)
          {
            phyloseq::plot_tree(treecontent,
                                label.tips = input$TRankC,
                                size = "abundance",
                                ladderize = TRUE,
                                color = "Sample")
          } else if ("nolabel" %in% options_chosen)
          {
            phyloseq::plot_tree(treecontent,
                                ladderize = TRUE,
                                color = "Sample")
          } else
          {
            phyloseq::plot_tree(treecontent,
                                label.tips = input$TRankC,
                                ladderize = TRUE,
                                color = "Sample")
          }

        }

      }

    }
    else if (input$VisC == "CoocOV")
    {
      selected_Rank <- get(input$TRankC)
      selected_names <- selected_Rank$Sorted_Names[input$TopX[1]:input$TopX[2]]
      sel_Occur <- selected_Rank$Occurance
      sel_Occur <- dplyr::filter(sel_Occur, Name %in% selected_names)
      sel_Occur <- dplyr::select(sel_Occur, where(is.numeric))
      sel_Cooccur <- cooccur::cooccur(sel_Occur, spp_names = TRUE)
      plot(sel_Cooccur) + theme(legend.position = "bottom") +
        ggtitle(paste0(input$TRankC, " Co-occurence Matrix"))
    }
  })


  #------------------------------------>Data Visualization pure Server functions
  # define the drivers object depending on the operating system
  if (!is.null(Sys.info()))
  {
    if (Sys.info()["sysname"] == "Linux") {
      rootdrive <- c(home = "~")
    } else if (Sys.info()["sysname"] == "Windows")
    {
      posdrives <- paste0(letters, ":/")
      names(posdrives) <- posdrives
      rootdrive <- posdrives[dir.exists(posdrives)]
    } else
    {

    }
  }


  # Define the root for the ShinyDirButton "Files"
  shinyDirChoose(input,
                 "File",
                 roots = rootdrive)

  # assign the path of the chosen directory from the "Files" DirButton to the
  # reactive variable DirChoice
  DirChoice <- reactive(as.character(parseDirPath(rootdrive,
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

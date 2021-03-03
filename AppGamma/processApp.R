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
  # AppHelper for the main IlluminaAnalysis function
require("AppHelper")

# load html files into variables
Error000 <- read_file("./internal_files/html/Error000.html")
Error001 <- read_file("./internal_files/html/Error001.html")
Error002 <- read_file("./internal_files/html/Error002.html")
Error003 <- read_file("./internal_files/html/Error003.html")
Error004 <- read_file("./internal_files/html/Error004.html")
FAQ001 <- read_file("./internal_files/html/FAQ001.html")
FAQ002 <- read_file("./internal_files/html/FAQ002.html")

# create element containing the names of each file in the taxa_trainer folder
tax_trainer <- list.files(path = "./internal_files/taxa_trainer")
# name the elements in that element by the string they each have before the
# first "_" - will only work if the files in the taxa_trainer folder have
# unique names before the "_" in their names
names(tax_trainer) <- sapply(stringr::str_split(tax_trainer,
                                       "_"),
                             '[', 1)

#------------------------------------------------------------------------------------>User Interface
ui <- tagList(
  # initialize ShinyJs to enable Java Script usage within the App
  useShinyjs(),
  # initialize tags to define HTML behavior of certain elements
  tags$head(tags$style(HTML(".tooltip {width: 300px;}"))),
  # define behavior of actual User Interface
  # set the content of each tab as a uiOutput to create a reactive interactive
  # User Interface that changes according to the user input
  navbarPage("Processing 0.3.0",
             tabPanel("Data Analysis",
                      uiOutput("DataAnalysis")),
             tabPanel("FAQ",
                      fluidPage(
                        column(10,
                               offset = 1,
                               uiOutput("FAQ"))
                      )),
             tabPanel("SessionInfo",
                      uiOutput("SessionInfo"))
             )

)
#------------------------------------------------------------------------------------>Server Functions
server <- function(input, output, session) {
  #----------------------------------------------------------------->Data Analysis UI
  output$DataAnalysis <- renderUI({
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          uiOutput("DAside1")
        ),
        mainPanel(
          uiOutput("DAmain")
        )

      )#end of sidebar Layout
    )#end of FluidPage
  })

  #----------------------------------------->Define the contents of the sidebar panel
  # first element of the sidebar:
  # the folder selection
  # only after the folder is selected the rest of the sidebar will be available
  output$DAside1 <- renderUI({
    fluidPage(
      fluidRow(
        # anther uiOutput to create an interactive element
        uiOutput("DAside2")
      ),
      fluidRow(
        "Please select a folder.",
        shinyDirButton("Files",
                       "Folder selection",
                       "Select a folder containing fastq or fastq.gz files")

      )
    )
  })

  # second element of the sidebar
  # it depends on the entry of the first sidebar element
  output$DAside2 <- renderUI({
    # render the UI depending on the Status()
    if (Status() == "Error000"){
      # Error 000 - no fastq.gz files
      HTML(Error000)
    } else if (Status() == "Error001") {
      # Error 001 - no forward sequences
      HTML(Error001)
    } else if (Status() == "Error002"){
      # Error 002 - no reverse sequences
      HTML(Error002)
    } else if (Status() == "Error003"){
      # Error 003 - less forward than reverse sequences
      HTML(Error003)
    } else if (Status() == "Error004"){
      # Error 004 - more forward than reverse sequences
      HTML(Error004)
    } else if (Status() == "acceptable") {
      # render possible User Input options
      uiOutput("UserInput")
    }
  })

  # render the user Input section
  output$UserInput <- renderUI({
    fluidPage(
      fluidRow(
        column(6,
               # slider input to determine the left and right cutoff of the
               # forward sequences - all sequences longer than the chosen max
               # value will be trimmed down to that value - after that all
               # sequences will be trimmed from the 5'-end by the chosen
               # minimal value
               sliderInput("ForwardSlider",
                           "Forward Sequence Size",
                           min = 0,
                           max = 300,
                           value = c(10, 270))
               ),
        column(6,
               # numeric input to determine the allowed expected errors within
               # each forward sequence - the higher the number, the more
               # sequences will be accepted and for further processing
               numericInput("EEforward",
                            "Expected Errors Forward",
                            value = 2))
      ),
      fluidRow(
        column(6,
               # slider input to determine the left and right cutoff of the
               # reverse sequences - all sequences longer than the chosen max
               # value will be trimmed down to that value - after that all
               # sequences will be trimmed from the 5'-end by the chosen
               # minimal value
               sliderInput("ReverseSlider",
                           "Reverse Sequence Size",
                           min = 0,
                           max = 300,
                           value = c(10, 220))
               ),
        column(6,
               # numeric input to determine the allowed expected errors within
               # each reverse sequence - the higher the number, the more
               # sequences will be accepted and for further processing
               numericInput("EEreverse",
                            "Expected Errors Reverse",
                            value = 2))


      ),
      fluidRow(
        column(6,
               # a Ui output because the input for the merged sequences is
               # dependent on the input of the forard and the reverse slider
               uiOutput("mergedSeq")),
        column(6,
               selectInput("trainer",
                           "Taxa trainer:",
                           choices = as.list(tax_trainer)
                           ))



      ),
      fluidRow(
        column(6,
               # checkbox to activate resorting of the Samples
               checkboxInput("resort",
                             "Resort Sample names",
                             value = FALSE)
               ),
        column(6,
               # Start Button to start the calculations
               actionButton("Start",
                            "START")
               )

      )
    )
  })

  # render the merged sequence slider
  # it needs to be rendered in it own UI output because the values depend
  # on the input of the previous sliders
  # sequences below the lower threshold of the slider will be discarded as will
  # be all sequences above the chosen maximum length
  output$mergedSeq <- renderUI({
    sliderInput("mergedSeq",
                "Merged Sequence Size",
                min = 0,
                max = input$ForwardSlider[2] -
                      input$ForwardSlider[1] +
                      input$ReverseSlider[2] -
                      input$ReverseSlider[1],
                value = c(0,
                          (input$ForwardSlider[2] -
                          input$ForwardSlider[1] +
                          input$ReverseSlider[2] -
                          input$ReverseSlider[1])
                          ))
  })

  #---------------------------------------------------->define contents of main panel
  output$DAmain <- renderUI({
    fluidPage(
      fluidRow(
        verbatimTextOutput("DirCtext")
      ),
      fluidRow(
        column(6,
               fluidRow("Help:"),
               fluidRow(br()),
               fluidRow(uiOutput("Tooltip"))),
        column(5,
               fluidRow("Status:"),
               fluidRow(br()),
               fluidRow(textOutput("Status")),
               offset = 1
               )
      )
    )
  })

  # render the path of the selected folder as text
  output$DirCtext <- renderText({
    paste0("Selected Folder: ",
    if (is.list(input$Files) == T){
      DirChoice()
    } else {
      HTML("NONE")
    })
  })

  #------->Tooltip Section
  initialTooltip <- paste0("Use the button on the left hand side to select a ",
                           "folder. The App requires files that are ",
                           "of the following format: <br>",
                           "<strong>SAMPLENAME_L001_R1_001.fastq.gz</strong>",
                           "<br> and<br>",
                           "<strong>SAMPLENAME_L001_R2_001.fastq.gz</strong>.",
                           "<br> This is the standard format of the ",
                           "Illumina file system. Within the folder the ",
                           "amount of R1 and R2 files needs to be equal.")
  defaultTooltip <- paste0("Use the input options on the left hand side to ",
                           "determine the conditions of the data analysis ",
                           "algorithm. Hover over the options to display a ",
                           "short tooltip here. If things are still unclear ",
                           "consider using the FAQ tab. <br><br> Once you are ",
                           "finished adjusting the options, press the START ",
                           "button to initialize the algorithm.")
  output$Tooltip <- renderText({
    initialTooltip
  })


  # Tooltip of the Files Button - will be displayed on mouseover
  onevent("mouseenter",
          "Files",
          output$Tooltip <- renderText({
            "Press this button to select a folder."
          }))
  onevent("mouseleave",
          "Files",
          output$Tooltip <- renderText({
            if (Status() == "acceptable"){
              defaultTooltip
            } else {
              initialTooltip
            }
          }))
    # resets the tooltip to the default on input of the Files Button
    # this is needed because with the input the sidebar changes and the mouse
    # leaves the eventfield without registering because of the creation of the
    # folder selection window
  observeEvent(input$Files,
               {
                 output$Tooltip <- renderText({
                   if (Status() == "acceptable"){
                     defaultTooltip
                   } else {
                     initialTooltip
                   }
                 })
               })
  # Tooltip of the forward Slider
  onevent("mouseenter",
          "ForwardSlider",
          output$Tooltip <- renderText({
            "This slider determines the length of the forward sequences. <br>
            <br>First all seqences longer than then given maximum value of the
            slider will be truncated down to that value. Then the sequences
            will be trimmed on the 5'-end by as many bases as determined by the
            given minimum value of the slider"
          }))
  onevent("mouseleave",
          "ForwardSlider",
          output$Tooltip <- renderText({
            defaultTooltip
          }))

  # Tooltip of the reverse Slider
  onevent("mouseenter",
          "ReverseSlider",
          output$Tooltip <- renderText({
            "This slider determines the length of the reverse sequences. <br>
            <br>First all seqences longer than then given maximum value of the
            slider will be truncated down to that value. Then the sequences
            will be trimmed on the 5'-end by as many bases as determined by the
            given minimum value of the slider"
          }))
  onevent("mouseleave",
          "ReverseSlider",
          output$Tooltip <- renderText({
            defaultTooltip
          }))

  # Tooltip of the merged sequence Slider
  onevent("mouseenter",
          "mergedSeq",
          output$Tooltip <- renderText({
            "This slider determines which of the merged sequences are accepted.
            <br><br>All merged sequences that are longer than the given maximum
            value or shorter than the given minimum value, will be discarded."
          }))
  onevent("mouseleave",
          "mergedSeq",
          output$Tooltip <- renderText({
            defaultTooltip
          }))

  # Tooltip of the resort button
  onevent("mouseenter",
          "resort",
          output$Tooltip <- renderText({
            "EXPERIMENTAL feature <br><br>IF you check this option the
            SAMPLENAMEs will be resorted. It only works with SAMPLENAMEs that
            are of the form letters followed by digits and will attempt to
            create new unique names which all have the same amount of digits
            after the letters. The goal is an easier overview of the samples
            downstream. If the SAMPLENAMEs have letters after the digits it will
            result in an App chrashing error.<br><br>Use at you own discretion."
          }))
  onevent("mouseleave",
          "resort",
          output$Tooltip <- renderText({
            defaultTooltip
          }))

  # Tooltip of the EEforward numeric Input
  onevent("mouseenter",
          "EEforward",
          output$Tooltip <- renderText({
            "This input determines the maximum amount of accepted expected
            errors within each forward sequence. Sequences with more expected
            errors will be discarded. <br><br>Have a look at the FAQ if you
            need more information on expected errors and what they are."
          }))
  onevent("mouseleave",
          "EEforward",
          output$Tooltip <- renderText({
            defaultTooltip
          }))

  # Tooltip of the EEforward numeric Input
  onevent("mouseenter",
          "EEreverse",
          output$Tooltip <- renderText({
            "This input determines the maximum amount of accepted expected
            errors within each reverse sequence. Sequences with more expected
            errors will be discarded. <br><br>Have a look at the FAQ if you
            need more information on expected errors and what they are."
          }))
  onevent("mouseleave",
          "EEreverse",
          output$Tooltip <- renderText({
            defaultTooltip
          }))

  # Tooltip of the taxa trainer choice
  onevent("mouseenter",
          "trainer",
          output$Tooltip <- renderText({
            "Choose which trainer file you want to use as a reference for the
            taxanomy assignment.<br><br>More information on what the different
            trainer files are and how you can use your own, is provided in the
            FAQ."
          }))
  onevent("mouseleave",
          "trainer",
          output$Tooltip <- renderText({
            defaultTooltip
          }))

  # Tooltip of the taxa trainer choice
  onevent("mouseenter",
          "Start",
          output$Tooltip <- renderText({
            "Press this to start the algorithm that will calculate the amplicon
            sequence variants within your provided Illumina fastq.gz files. As
            a part of the agorithm the sequences will be moved to a new folder.
            These ASVs will be taxonomically identified and then a Co-occurance
            will be calculated. The total process can be time consuming.<br>
            <br>Once the button is pressed the algorithm will execute and
            changing the options on the left hand side will have no more
            influence on the result. Please make sure that all the options are
            set to the values you wish them to be before starting the
            algorithm."
          }))
  onevent("mouseleave",
          "Start",
          output$Tooltip <- renderText({
            defaultTooltip
          }))

  #---------------------------------------------->Data Analysis pure Server functions
  # Define the root for the ShinyDirButton "Files"
  shinyDirChoose(input,
                 "Files",
                 roots = c(home = "~"))

  # assign the path of the chosen directory from the "Files" DirButton to the
  # reactive variable DirChoice
  DirChoice <- reactive(as.character(parseDirPath(c(home = "~"), input$Files)))

  # determine the Status depending on the contents of the choosen directory
  Status <- reactive({
    if (is.list(input$Files)){
      # create a list for all the fastq.gz files within the chosen directory
      # that have the forward sequence pattern assigned by the Illumina itself
      listR1 <- list.files(DirChoice(), pattern = "R1_001.fastq.gz")
      # create a similar list for the reverse sequences
      listR2 <- list.files(DirChoice(), pattern = "R2_001.fastq.gz")

      if (length(listR1) == 0 & length(listR2) == 0){
        # Error 000 - no fastq.gz files
        "Error000"
      }else if (length(listR1) == 0) {
        # Error 001 - no forward sequences
        "Error001"
      } else if (length(listR2) == 0){
        # Error 002 - no reverse sequences
        "Error002"
      } else if (length(listR1) < length(listR2)){
        # Error 003 - less forward than reverse sequences
        "Error003"
      } else if (length(listR1) > length(listR2)){
        # Error 004 - more forward than reverse sequences
        "Error004"
      } else {
        # determine the folder as acceptable
        "acceptable"
      }
    } else {
      "empty"
    }
  })

  # execute the following function if an input (click) of the Start button is
  # observed
  observeEvent(input$Start,
               {withCallingHandlers({
                 folderpath <- parseDirPath(c(home = "~"), input$Files)
                 truF <- input$ForwardSlider[2]
                 trilF <- input$ForwardSlider[1]
                 truR <-  input$ReverseSlider[2]
                 trilR <- input$ReverseSlider[1]
                 minSeq <- input$mergedSeq[1]
                 maxSeq <- input$mergedSeq[2]
                 EEF <- input$EEforward
                 EER <- input$EEreverse
                 Ttrainer <- paste0("./internal_files/taxa_trainer/",
                                    input$trainer)
                 rsort <- input$resort
                 AppHelper::illumina_analysis(
                   input_path = folderpath,
                   trunc_forw = truF,
                   trim_left_forw = trilF,
                   trunc_rev = truR,
                   trim_left_rev = trilR,
                   seq_length = minSeq : maxSeq,
                   EEforward = EEF,
                   EEreverse = EER,
                   tax_trainer = Ttrainer,
                   resort = rsort)
               },
               message = function(mess){
                 shinyjs::html(id = "Status",
                               html = paste0(mess$message,
                                             br()),
                               add = T)}
               )})

  #--------------------------------------------------------------------------->FAQ UI
  output$FAQ <- renderUI({
    bsCollapse(id = "FAQCollapse",
               open = NULL,
               bsCollapsePanel(title = "Q1: What are the taxa trainers
                               (hitdb, rdp, silva)?",
                               value = "FAQ1",
                               HTML(FAQ001)),
               bsCollapsePanel(title = "Q2: Why can I not see/access the options of
                               the data analysis algorithm?",
                               value = "FAQ2",
                               HTML(FAQ002)))
  })

}
#------------------------------------------------------------------------------------>App
shinyApp(ui, server)

library(shiny)
library(DT)

test <- read_csv("test.csv")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        selectInput("SubsetChoice",
                    "Select by which column to subset",
                    choices = c("name" = 1, "rooms" = 2, "color" = 3))
      ),
      fluidRow(
        uiOutput("Subset2")
      )
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

server <- function(input, output, session)
{
  output$Subset2 <- renderUI ({
    x <- test[as.integer(input$SubsetChoice)]

    fluidPage(
      fluidRow(
        selectInput("SubsetChoice2",
                    "Select the item to subset by",
                    choices = x,
                    selected = 1)
      )
    )
  })

  output$table <- renderDT({
    subtable <- test
    subtable <- subset(subtable, test[as.integer(input$SubsetChoice)] == input$SubsetChoice2)
  })
}

shinyApp(ui, server)

oldM <- as(tax_table(CompletePhyl), "matrix")
testy <- NULL
for (i in 1:ncol(test))
{
  testy <- c(testy, i)
  names(testy)[i] <- colnames(test)[i]
}

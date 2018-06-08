library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(DT)
library(MCDM)

ui <- tagList(
  useShinyjs(),
  inlineCSS(
    "
    #loading-content {
    position: absolute;
    background: #FFFFFF;
    opacity: 0.9;
    z-index: 100;
    left: 0;
    right: 0;
    height: 100%;
    text-align: center;
    }
    "
  ),
  div(
    id = "loading-content",
    h2("Loading...")
  ),
  hidden(
    div(
      id = "app-content",
      navbarPage(
        "Multiple Criteria Decision Making",
        theme = shinytheme("flatly"),
        tabPanel(
          "Data",
          icon = icon("database"),
          sidebarLayout(
            sidebarPanel(
              radioButtons(
                "source",
                "Select source of data",
                choices = c("Example dataset" = "example",
                            "Upload dataset" = "upload")
              ),
              conditionalPanel(
                condition = "input.source == 'example'",
                helpText("This is a fabricated dataset")
              ),
              conditionalPanel(
                condition = "input.source == 'upload'",
                fileInput(
                  "upload_data",
                  "Please upload your data",
                  multiple = FALSE,
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"
                  )
                ),
                helpText("File should be in .csv format")
              ),
              actionButton(
                "use",
                "Use dataset"
              ),
              conditionalPanel(
                condition = "input.use",
                br(),
                pickerInput(
                  "alternative",
                  "Select column containing alternatives",
                  choices = NULL
                ),
                pickerInput(
                  "attribute_max",
                  "'The higher, the better' attribute(s)",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    'live-search' = TRUE,
                    'actions-box' = TRUE
                  )
                ),
                pickerInput(
                  "attribute_min",
                  "The lower, the better' attribute(s)",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    'live-search' = TRUE,
                    'actions-box' = TRUE
                  )
                ),
                actionButton(
                  "show",
                  "Show dataset"
                )
              )
            ),
            mainPanel(
              DT::dataTableOutput("tab_dataset")
            )
          )
        ),
        tabPanel(
          "Analysis",
          icon = icon("line-chart"),
          sidebarLayout(
            sidebarPanel(
              icon("sliders", "fa-2x"),
              uiOutput("setting_panel")
            ),
            mainPanel(
              
            )
          )
        ),
        tabPanel(
          "About",
          icon = icon("support"),
          wellPanel(
            includeMarkdown("README.md")
          )
        )
      )
    )
  )
  )

server <- function(input, output, session) {
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")
  
  observe({
    if (input$source == "example") {
      enable("use")
    } else if (input$source == "upload" & !is.null(input$upload_data)) {
      enable("use")
    } else {
      disable("use")
    }
  })
  
  rawdata <- eventReactive(input$use, {
    if (input$source == "example") {
      read_csv("./data/dummy.csv")
    } else if (input$source == "upload") {
      read_csv(input$upload_data$datapath)
    }
  })
  
  observeEvent(input$use, {
    updatePickerInput(
      session = session,
      inputId = "alternative",
      choices = names(rawdata())
    )
    
    updatePickerInput(
      session = session,
      inputId = "attribute_max",
      choices = names(rawdata())
    )
    
    updatePickerInput(
      session = session,
      inputId = "attribute_min",
      choices = names(rawdata())
    )
  })
  
  observe({
    toggleState(id = "show", condition = !is.null(input$alternative) & {!is.null(input$attribute_max) | !is.null(input$attribute_min)})
  })
  
  tab_dataset <- eventReactive(input$show, {
    rawdata() %>% 
      select(one_of(input$alternative, input$attribute_max, input$attribute_min))
  })
  
  output$tab_dataset <- DT::renderDataTable({
    tab_dataset() %>%
      datatable(
        rownames = FALSE,
        style = 'bootstrap',
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brt",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 300,
          scroller = TRUE,
          buttons =
            list(
              list(
                extend = "copy"
              ),
              list(
                extend = "collection",
                buttons = c("csv", "excel"),
                text = "Download"
              )
            )
        )
      )
  },
  server = FALSE
  )
  
  observeEvent(input$show, {
    output$setting_panel <- renderUI({
      tagList(
        lapply(c(input$attribute_max, input$attribute_min), function(i){
          numericInput(inputId = paste0("attr_", i), label = paste("Weight for", i),
                       min = 0, max = 1, value = 1/length(c(input$attribute_max, input$attribute_min)))
        }),
        pickerInput(
          "method",
          "Method",
          choices = c(
            "Multi-MOORA" = "MMOORA",
            "RIM" = "RIM",
            "TOPSIS Linear" = "TOPSISLinear",
            "TOPSIS Vector" = "TOPSISVector",
            "VIKOR" = "VIKOR",
            "WASPAS" = "WASPAS",
            "Meta Ranking" = "MetaRanking"
          )
        ),
        actionButton(
          "apply",
          "Apply"
        )
      )
      
    })
  })
}

shinyApp(ui = ui, server = server)

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(tidyverse)
library(DT)
library(formattable)
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
                choices = c(
                  "Example dataset" = "example",
                  "Upload dataset" = "upload"
                )
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
                    "live-search" = TRUE,
                    "actions-box" = TRUE
                  )
                ),
                pickerInput(
                  "attribute_min",
                  "The lower, the better' attribute(s)",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    "live-search" = TRUE,
                    "actions-box" = TRUE
                  )
                ),
                actionButton(
                  "show",
                  "Show dataset"
                )
              )
            ),
            mainPanel(
              withSpinner(DT::dataTableOutput("tab_dataset"), type = 5, color = "#34495e")
            )
          )
        ),
        tabPanel(
          "Analysis",
          icon = icon("line-chart"),
          sidebarLayout(
            sidebarPanel(
              uiOutput("setting_panel")
            ),
            mainPanel(
              withSpinner(DT::dataTableOutput("tab_res"), type = 5, color = "#34495e")
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
    toggleState(id = "show", condition = !is.null(input$alternative) & {
      !is.null(input$attribute_max) | !is.null(input$attribute_min)
    })
  })

  dataset <- eventReactive(input$show, {
    cost <- style(
      "background-color" = csscolor("darkred"),
      color = "white",
      display = "block",
      "border-radius" = "4px",
      "padding" = "0 4px"
    )
    
    benefit <- style(
      "background-color" = csscolor("seagreen"),
      color = "white",
      display = "block",
      "border-radius" = "4px",
      "padding" = "0 4px"
    )
    
    if (!is.null(input$attribute_max) & !is.null(input$attribute_min)) {
      res <- rawdata() %>%
        select(one_of(input$alternative, input$attribute_max, input$attribute_min)) %>% 
        formattable(
          list(area(col = isolate(input$attribute_max)) ~ formatter("span", style = benefit),
               area(col = isolate(input$attribute_min)) ~ formatter("span", style = cost))
        )
    } else if  (is.null(input$attribute_min)) {
      res <- rawdata() %>%
        select(one_of(input$alternative, input$attribute_max, input$attribute_min)) %>% 
        formattable(
          list(area(col = isolate(input$attribute_max)) ~ formatter("span", style = benefit))
        )
    } else if (is.null(input$attribute_max)) {
      res <- rawdata() %>%
        select(one_of(input$alternative, input$attribute_max, input$attribute_min)) %>% 
        formattable(
          list(area(col = isolate(input$attribute_min)) ~ formatter("span", style = cost))
        )
    }
    return(res)
  })

  output$tab_dataset <- DT::renderDataTable({
    dataset() %>%
      as.datatable(
        rownames = FALSE,
        caption = "Columns with red colour define attributes which are desirable in high values, whereas columns with green colour define attributes which are undesirable in high values.",
        style = "bootstrap",
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
        map(
          c(input$attribute_max, input$attribute_min),
          ~ numericInput(
            inputId = paste0("weight_", .x), label = paste("Weight for", .x),
            min = 0, max = 1, value = 1 / length(c(input$attribute_max, input$attribute_min))
          )
        ),
        helpText("The sum of weights should be equal to 1"),
        pickerInput(
          "method",
          "Method",
          choices = c(
            # "Meta Ranking" = "MetaRanking",
            "Multi-MOORA" = "MMOORA",
            # "RIM" = "RIM",
            "TOPSIS Linear" = "TOPSISLinear",
            "TOPSIS Vector" = "TOPSISVector"
            # "VIKOR" = "VIKOR",
            # "WASPAS" = "WASPAS"
          ),
          selected = "TOPSISVector"
        ),
        actionButton(
          "apply",
          "Apply"
        )
      )
    })
  })

  res <- eventReactive(input$apply, {
    if (input$method == "MetaRanking") {
      "a"
    } else if (input$method == "MMOORA") {
      cb <- c(
        rep("max", length(input$attribute_max)),
        rep("min", length(input$attribute_min))
      )
      
      w <- map_dbl(
        c(input$attribute_max, input$attribute_min),
        ~ input[[paste0("weight_", .x)]]
      )
      
      res <- dataset() %>%
        as.data.frame() %>%
        `rownames<-`(.[, input$alternative]) %>%
        select_if(is.numeric) %>%
        as.matrix() %>%
        MMOORA(w, cb) %>%
        as_tibble() %>%
        rename(Alternative = Alternatives,
               "Ratio System" = RatioSystem,
               "Ranking (Ratio System)" = Ranking,
               "Reference Point" = ReferencePoint,
               "Ranking (Reference Point)" = Ranking.1,
               "Multiplicative Form" = MultiplicativeForm,
               "Ranking (Multipicative Form)" = Ranking.2,
               "Overal Ranking (Multi MOORA)" = MultiMooraRanking) %>% 
        mutate(Alternative = pull(dataset()[, input$alternative])) %>% 
        mutate_if(is_double, .funs = funs(round(., 2)))
    } else if (input$method == "RIM") {
      
    } else if (input$method == "TOPSISLinear") {
      cb <- c(
        rep("max", length(input$attribute_max)),
        rep("min", length(input$attribute_min))
      )
      
      w <- map_dbl(
        c(input$attribute_max, input$attribute_min),
        ~ input[[paste0("weight_", .x)]]
      )
      
      res <- dataset() %>%
        as.data.frame() %>%
        `rownames<-`(.[, input$alternative]) %>%
        select_if(is.numeric) %>%
        as.matrix() %>%
        TOPSISLinear(w, cb) %>%
        as_tibble() %>%
        rename(Alternative = Alternatives,
               "R index" = R) %>%
        mutate(Alternative = pull(dataset()[, input$alternative])) %>% 
        mutate_if(is_double, .funs = funs(round(., 2)))
    } else if (input$method == "TOPSISVector") {
      cb <- c(
        rep("max", length(input$attribute_max)),
        rep("min", length(input$attribute_min))
      )

      w <- map_dbl(
        c(input$attribute_max, input$attribute_min),
        ~ input[[paste0("weight_", .x)]]
      )

      res <- dataset() %>%
        as.data.frame() %>%
        `rownames<-`(.[, input$alternative]) %>%
        select_if(is.numeric) %>%
        as.matrix() %>%
        TOPSISVector(w, cb) %>%
        as_tibble() %>%
        rename(Alternative = Alternatives,
               "R index" = R) %>%
        mutate(Alternative = pull(dataset()[, input$alternative])) %>% 
        mutate_if(is_double, .funs = funs(round(., 2)))
    } else if (input$method == "VIKOR") {
      
    } else if (input$method == "WASPAS")
    return(res)
  })
  
  output$tab_res <- DT::renderDataTable({
    res() %>%
      datatable(
        rownames = FALSE,
        style = "bootstrap",
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
}

shinyApp(ui = ui, server = server)

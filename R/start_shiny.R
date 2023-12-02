#' Call Our Shiny App as a Function
#'
#' @title start_shiny
#' @description
#' This function starts a shiny app in which we use the "ctgov" trials database
#' to create visualizations and conduct data analysis.
#' @importFrom ctrialsgov ctgov_get_latest_snapshot ctgov_load_duckdb_file
#' @importFrom ctrialsgov ctgov_query_endpoint
#' @importFrom DBI dbConnect
#' @importFrom duckdb duckdb
#' @importFrom dplyr tbl select rename
#' @importFrom shiny shinyApp fluidPage titlePanel sidebarLayout sidebarPanel
#' @importFrom shiny textInput selectInput mainPanel tabsetPanel tabPanel
#' @importFrom shiny plotOutput renderPlot h3 h5
#' @importFrom wordcloud2 wordcloud2Output renderWordcloud2
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom utils head
#' @returns A shiny app that allows us to explore the contents in the
#'          "ctgov" trials database.
#' @export
start_shiny = function() {

  # download the latest database snapshot
  ctrialsgov::ctgov_get_latest_snapshot(db_path = "ctgov.duckdb",
                                        db_derived_path = "ctgov-derived.duckdb")

  con = dbConnect(
    duckdb(
      # need to change file path
      file.path("ctgov.duckdb"),
      read_only = TRUE
    )
  )
  studies = tbl(con, "studies")
  conditions = tbl(con, "conditions")
  interventions = tbl(con, "interventions")
  countries = tbl(con, "countries")

  # need to change file path
  ctgov_load_duckdb_file(file.path("ctgov-derived.duckdb"))
  endpoints = ctgov_query_endpoint()

  shinyApp(
    # Define UI
    ui <- fluidPage(

      # Application title
      titlePanel("Clinical Trials Query"),

      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          h5("Authors: Houmin Xing, Yi Ren, Yiran Liu"),
          textInput("brief_title_kw", h3("Brief title keywords")),
          selectInput("sponsor_type", label = h3("Select Sponsor Type"),
                      choices = list("Federal" = "FED",
                                     "Individual" = "INDIV",
                                     "Industry" = "INDUSTRY",
                                     "Network" = "NETWORK",
                                     "NIH" = "NIH",
                                     "Other" = "OTHER",
                                     "Other gov" = "OTHER_GOV",
                                     "Unknown" = "Unknown",
                                     "ALL_SPONSORS" = 1),
                      selected = 1),
          selectInput("status_type", label = h3("Select Overall Status"),
                      choices = list("Terminated" = "Terminated",
                                     "Completed" = "Completed",
                                     "Active, not recruiting" = "Active, not recruiting",
                                     "Unknown status" = "Unknown status",
                                     "Not yet recruiting" = "Not yet recruiting",
                                     "No longer available" = "No longer available",
                                     "Withdrawn" = "Withdrawn",
                                     "Recruiting" = "Recruiting",
                                     "Enrolling by invitation" = "Enrolling by invitation",
                                     "Temporarily not available" = "Temporarily not available",
                                     "Suspended" = "Suspended",
                                     "Withheld" = "Withheld" ,
                                     "Available" = "Available",
                                     "Approved for marketing"  = "Approved for marketing",
                                     "ALL_STATUS" = 1),
                      selected = 1)
        ),


        mainPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel("Phases Plot", plotOutput("distPlot")),
            tabPanel("Endpoint Met", plotOutput("endpointPlot")),
            tabPanel("Conditions", plotOutput("conditionPlot")),
            tabPanel("Study Types", plotOutput("studyTypePlot")),
            tabPanel("Word Cloud", wordcloud2Output("wordCloudPlot")),
            tabPanel("Start Year", plotOutput("startYearPlot")),
            tabPanel("Interventions", plotOutput("interventionsPlot")),
            tabPanel("World Map", plotOutput("worldMapPlot"))
          ),
          dataTableOutput("trial_table")
        )
      )
    ),

    # Define server logic
    server <- function(input, output) {

      output$distPlot <- renderPlot({
        create_phase_histogram_plot(studies, input$sponsor_type, input$status_type,
                                    input$brief_title_kw)
      })
      output$endpointPlot <- renderPlot({
        create_endpoint_histogram(studies, input$sponsor_type, input$status_type, endpoints,
                                  input$brief_title_kw)
      })
      output$conditionPlot <- renderPlot({
        create_condition_histogram(studies, conditions,
                                   input$sponsor_type, input$status_type, input$brief_title_kw)
      })
      output$studyTypePlot <- renderPlot({
        create_study_type_histogram_plot(studies, input$sponsor_type, input$status_type,
                                         input$brief_title_kw)
      })
      output$wordCloudPlot <- renderWordcloud2({
        create_word_cloud_plot(studies, input$sponsor_type, input$status_type,
                               input$brief_title_kw)
      })
      output$startYearPlot <- renderPlot({
        create_start_year_histogram_plot(studies, input$sponsor_type, input$status_type,
                                         input$brief_title_kw)
      })
      output$interventionsPlot <- renderPlot({
        create_interventions_pie_chart(studies, interventions,
                                       input$sponsor_type, input$status_type, input$brief_title_kw)
      })
      output$worldMapPlot <- renderPlot({
        create_world_map(studies, input$sponsor_type, input$status_type,
                         input$brief_title_kw, countries)
      })

      output$trial_table = renderDataTable({
        si = trimws(unlist(strsplit(input$brief_title_kw, ",")))

        title_kw_search(studies, input$sponsor_type, input$status_type, si) |>
          select(nct_id, brief_title, start_date, completion_date) |>
          rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
                 `Start Date` = start_date, `Completion Date` = completion_date) |>
          head(1000)
      })

    }
  )
}

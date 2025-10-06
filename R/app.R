#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#' @importFrom shiny tabsetPanel tabPanel reactiveVal observeEvent showModal modalDialog removeModal updateTabsetPanel req
#' @importFrom shiny actionButton textInput hr tags shinyApp
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL


#' Launch Kolada Shiny App
#'
#' This function launches the Shiny application to explore Kolada KPIs and Municipalities.
#'
#' @export
run_app <- function() {

  # UI
  ui <- fluidPage(
    titlePanel("Kolada KPI Hub"),
    sidebarLayout(
      sidebarPanel(
        # Section: Load all KPIs
        tags$h4("KPI Library"),
        actionButton("load", "Load"),
        textInput("search", "Search Keyword", ""),
        hr(),

        # Section: Load only the filtered KPIs
        tags$h4("KPI Selector"),
        textInput("kpi_ids", "Enter KPI IDs:", ""),
        actionButton("filter", "Filter"),
        hr(),

        # Section: Load all municipalities
        tags$h4("Municipality"),
        actionButton("load_muni", "Load"),
        textInput("search_muni", "Search Keyword"),
        hr(),

        # Section: Load selected municipalities
        tags$h4("Municipality Selector"),
        textInput("muni_ids", "Enter Municipality IDs:", ""),
        actionButton("filter_muni", "Filter")
      ),
      mainPanel(
        tabsetPanel(
          id = "kpi_tab",
          tabPanel("KPI", DTOutput("kpi_table")),
          tabPanel("KPI Selector", DTOutput("selected_table")),
          tabPanel("Municipality", DTOutput("muni_table")),
          tabPanel("Municipality Selector", DTOutput("muni_selector"))
        )
      )
    )
  )

  # Server
  server <- function(input, output, session) {
    kpi_data <- reactiveVal(NULL)
    muni_data <- reactiveVal(NULL)

    observeEvent(input$load, {
      showModal(modalDialog("Loading KPIs...", footer = NULL))
      kpi <- get_kpi()
      removeModal()
      kpi_data(kpi)
      updateTabsetPanel(session, "kpi_tab", selected = "KPI")
    })

    observeEvent(input$filter, {
      req(input$kpi_ids)
      ids <- strsplit(input$kpi_ids, ",")[[1]] %>% trimws()
      showModal(modalDialog("Loading KPIs...", footer = NULL))
      kpi <- get_kpi_by_Ids(ids)
      removeModal()
      kpi_data(kpi)
      updateTabsetPanel(session, "kpi_tab", selected = "KPI Selector")
    })

    observeEvent(input$load_muni, {
      showModal(modalDialog("Loading Municipalities...", footer = NULL))
      municipality <- get_municipality()
      removeModal()
      muni_data(municipality)
      updateTabsetPanel(session, "kpi_tab", selected = "Municipality")
    })

    observeEvent(input$filter_muni, {
      req(input$muni_ids)
      ids <- strsplit(input$muni_ids, ",")[[1]] %>% trimws()
      showModal(modalDialog("Loading Municipalities...", footer = NULL))
      municipality <- get_municipality_by_Ids(ids)
      removeModal()
      muni_data(municipality)
      updateTabsetPanel(session, "kpi_tab", selected = "Municipality Selector")
    })

    output$kpi_table <- renderDT({
      req(kpi_data())
      data <- kpi_data()
      if(input$search != "") {
        data <- data %>%
          filter(grepl(input$search, .data$title, ignore.case = TRUE) |
                   grepl(input$search, .data$id, ignore.case = TRUE))
      }
      datatable(data, options = list(pageLength = 10), rownames = FALSE)
    })

    output$selected_table <- renderDT({
      req(kpi_data())
      datatable(kpi_data(), options = list(pageLength = 10), rownames = FALSE)
    })

    output$muni_table <- renderDT({
      req(muni_data())
      data <- muni_data()
      if(input$search_muni != "") {
        data <- data %>%
          filter(grepl(input$search_muni, .data$title, ignore.case = TRUE) |
                   grepl(input$search_muni, .data$id, ignore.case = TRUE)
                 )
      }
      datatable(data, options = list(pageLength = 10), rownames = FALSE)
    })

    output$muni_selector <- renderDT({
      req(muni_data())
      datatable(muni_data(), options = list(pageLength = 10), rownames = FALSE)
    })
  }

  # Run the app
  shinyApp(ui, server)
}

mod_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .sidebar-button {
      background-color: green;
      color: white;
      font-weight: bold;
      font-size: 16px;
      height: 40px;
      width: 100%;
      border: 2px solid black;
    }
    .sidebar-button-red {
      background-color: red;
      color: white;
      font-weight: bold;
      font-size: 16px;
      height: 40px;
      width: 100%;
      border: 2px solid black;
    }")),
    fluidRow(
      column(6, selectInput(ns("year"), "Year", choices = NULL, multiple = TRUE)),
      column(6, selectInput(ns("month"), "Month", choices = NULL, multiple = TRUE))),
    fluidRow(
      column(6, selectInput(ns("prim_cat"), "Primary Category", choices = NULL, multiple = TRUE)),
      column(6, selectInput(ns("sec_cat"), "Secondary Category", choices = NULL, multiple = TRUE))),
    fluidRow(
      column(12,
        div(style = "display: flex; justify-content: space-between; gap: 10px;",
          actionButton(ns("apply"), "Apply Filters", class = "sidebar-button", style = "flex: 1;"),
          actionButton(ns("reset"), "Reset", class = "sidebar-button-red", style = "flex: 1;")
        )
      )
    )
  )
}
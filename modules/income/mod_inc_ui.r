mod_inc_ui <- function(id) {
ns <- NS(id)
tagList(
  tags$style(HTML("
    .reactable .rt-th:first-child {
      background-color: #3563bf !important;
      color: white !important;
    }    
    .reactable .rt-td, .reactable .rt-th {
      border-left: none !important;
      border-right: none !important;
    }
    .sidebar-button {
      background-color: green;
      color: white;
      font-weight: bold;
      font-size: 16px;
      height: 40px;
      border: 2px solid black;
    }
    .sidebar-button-red {
      background-color: red;
      color: white;
      font-weight: bold;
      font-size: 16px;
      height: 40px;
      border: 2px solid black;
    }
  ")),
fluidRow(
      column(2, airDatepickerInput(ns("modal_date"), "Date", value = NULL, multiple = FALSE, todayButton = TRUE)),
      column(2, selectizeInput(ns("modal_prim_cat"), "Category", choices = c("", "Έσοδα", "Έκτακτα Έσοδα")), selected = NULL, multiple = FALSE, options = list(create = TRUE)),
      column(2, uiOutput(ns("sec_cat_ui"))),
      column(2, numericInput(ns("modal_income"), "Income", value = "", min = 0)),
      column(2, selectInput(ns("action_selector"), "Choose Action:",
        choices = c("", "Add new row", "Delete rows", "Copy row", "Change info", "Change date", "Change category", "Save"),
        selected = NULL
      ))),
      fluidRow(
      div(
      style = "display: flex; align-items: center; gap: 150px;",
      actionButton(ns("perform_action"), "Go", icon = icon("play"), class = "sidebar-button"),
      actionButton(ns("clear_selection"), "Deselect All", class = "sidebar-button-red")
    )),
fluidRow(reactableOutput(ns("data_table")))
    )
}

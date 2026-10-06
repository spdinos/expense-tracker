mod_bank_ui <- function(id) {
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
  column(3,
      airDatepickerInput(ns("modal_date"), "Date", value = NULL, multiple = FALSE, todayButton = TRUE),
      selectInput(ns("modal_prim_cat"), "Primary Category", choices = c("", "Αποταμίευση")),
      uiOutput(ns("sec_cat_ui")),
      numericInput(ns("modal_balance"), "Bank Balance", value = "", min =0),
      selectInput(ns("action_selector"), "Choose Action:",
        choices = c("", "Add new row", "Delete rows", "Change info", "Change date", "Change category", "Save"),
        selected = NULL
      ),
      actionButton(ns("perform_action"), "Go", icon = icon("play"), class = "sidebar-button"),
      actionButton(ns("clear_selection"), "Deselect All", class = "sidebar-button-red")
    ),
column(9, reactableOutput(ns("data_table")))))
}

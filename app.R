library(shiny)
library(DT)
library(RPostgres)
library(dplyr)
library(data.table)
library(lubridate)
library(shinyjs)
library(tidyr)
library(shinyWidgets)
library(reactable)
library(htmlwidgets)
library(bslib)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(jsonlite)
library(rlang)

source("formulas/formula.r")

result <- get_data()

expenses <- as.data.table(result$Expenses)
budget <- as.data.table(result$Budget)
dropdown_df  <- as.data.table(result$Dropdown)
bank <- as.data.table(result$Bank)
income <- as.data.table(result$Income)
monthly_summary <- as.data.table(result$Monthly_summary)
savings_outcome <- as.data.table(result$Savings_outcome)

source("modules/filters/mod_filters_ui.r")
source("modules/filters/mod_filters_server.r")
source("modules/main/mod_main_ui.r")
source("modules/main/mod_main_server.r")
source("modules/drop_down/mod_drp_ui.r")
source("modules/drop_down/mod_drp_server.r")
source("modules/expenses/mod_exp_ui.r")
source("modules/expenses/mod_exp_server.r")
source("modules/income/mod_inc_ui.r")
source("modules/income/mod_inc_server.r")
source("modules/bank/mod_bank_ui.r")
source("modules/bank/mod_bank_server.r")
source("modules/budget/mod_budget_ui.r")
source("modules/budget/mod_budget_server.r")
source("app_ui.r", local = TRUE)
source("app_server.r", local = TRUE)

# =========================
# UI / SERVER
# =========================

source("app_ui.r", local = TRUE)
source("app_server.r", local = TRUE)


shinyApp(
  ui = ui,
  server = server
)
server <- function(input, output, session) {
# --- project management server code ---
    expense_df <- reactiveVal(expenses)
    income_df <- reactiveVal(income)
    drop_down_df <- reactiveVal(dropdown_df)
    budget_df <- reactiveVal(budget)
    bank_df <- reactiveVal(bank)
    monthly_df <- reactiveVal(monthly_summary)
    savings_df <- reactiveVal(savings_outcome)

filters_module_expenses <- mod_filters_server("filters_expense", base_df = reactive(expense_df()))
filter_map_expenses <- filters_module_expenses$filter_map

filters_module_income <- mod_filters_server("filters_income", base_df = reactive(income_df()))
filter_map_income <- filters_module_income$filter_map

filters_module_budget <- mod_filters_server("filters_budget", base_df = reactive(budget_df()))
filter_map_budget <- filters_module_budget$filter_map

filters_module_dropdown <- mod_filters_server("filters_dropdown", base_df = reactive(drop_down_df()))
filter_map_dropdown <- filters_module_dropdown$filter_map

filters_module_summary <- mod_filters_server("filters_summary", base_df = reactive(monthly_df()))
filter_map_summary <- filters_module_summary$filter_map

filtered_dropdown <- reactive({
  df <- drop_down_df()
  filters <- filters_module_dropdown$applied_filters()


  for (id in names(filters)) {
    col <- filter_map_dropdown[[id]]
    val <- filters[[id]]

    if (!is.null(val) && length(val) > 0 && col %in% names(df)) {
      df <- df[df[[col]] %in% val, ]
    }
  }
  df
})   

filtered_expense <- reactive({
  df <- expense_df()
  filters <- filters_module_expenses$applied_filters()


  for (id in names(filters)) {
    col <- filter_map_expenses[[id]]
    val <- filters[[id]]

    if (!is.null(val) && length(val) > 0 && col %in% names(df)) {
      df <- df[df[[col]] %in% val, ]
    }
  }
  df
})

filtered_income <- reactive({
  df <- income_df()
  filters <- filters_module_income$applied_filters()


  for (id in names(filters)) {
    col <- filter_map_income[[id]]
    val <- filters[[id]]

    if (!is.null(val) && length(val) > 0 && col %in% names(df)) {
      df <- df[df[[col]] %in% val, ]
    }
  }
  df
})   

filtered_bank <- reactive({
  df <- bank_df()
  filters <- filters_module_expenses$applied_filters()


  for (id in names(filters)) {
    col <- filter_map_expenses[[id]]
    val <- filters[[id]]

    if (!is.null(val) && length(val) > 0 && col %in% names(df)) {
      df <- df[df[[col]] %in% val, ]
    }
  }
  df
})   

filtered_budget <- reactive({
  df <- budget_df()
  filters <- filters_module_budget$applied_filters()


  for (id in names(filters)) {
    col <- filter_map_budget[[id]]
    val <- filters[[id]]

    if (!is.null(val) && length(val) > 0 && col %in% names(df)) {
      df <- df[df[[col]] %in% val, ]
    }
  }
  df
})   

filtered_monthly <- reactive({
  df <- monthly_df()
  filters <- filters_module_summary$applied_filters()


  for (id in names(filters)) {
    col <- filter_map_expenses[[id]]
    val <- filters[[id]]

    if (!is.null(val) && length(val) > 0 && col %in% names(df)) {
      df <- df[df[[col]] %in% val, ]
    }
  }
  df
})   

filtered_savings <- reactive({
  df <- savings_df()
  filters <- filters_module_summary$applied_filters()


  for (id in names(filters)) {
    col <- filter_map_expenses[[id]]
    val <- filters[[id]]

    if (!is.null(val) && length(val) > 0 && col %in% names(df)) {
      df <- df[df[[col]] %in% val, ]
    }
  }
  df
})   

mod_main_server(
"main",
filtered_monthly,
filtered_savings
)

mod_drp_server(
"drop_down",
drop_down_df,
filtered_dropdown
)

mod_exp_server(
"expenses",
expense_df,
filtered_expense,
dropdown_df
)

mod_inc_server(
"income",
income_df,
filtered_income,
dropdown_df
)

mod_bank_server(
"bank",
bank_df,
filtered_bank
)

mod_budget_server(
"budget",
budget_df,
filtered_budget
)
  observeEvent(input$refresh_data, {
    showModal(modalDialog(
      title = "Refresh query",
      "Database will be refreshed. Have you saved the updated data in Access? If not, please save them first and then push refresh, else any updated will be lost.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_refresh", "Proceed Anyway"))))      
    })

  observeEvent(input$confirm_refresh, {
    removeModal()
    
    # 1. Get fresh data from Access (via get_data_fun function)
    result <- get_data()
    expenses <- as.data.table(result$Expenses)
    budget <- as.data.table(result$Budget)
    dropdown_df <- as.data.table(result$Dropdown)
    bank <- as.data.table(result$Bank)
    income <- as.data.table(result$Income)
    monthly_summary <- as.data.table(result$Monthly_summary)

    # 3. Replace master and display datasets directly
    expense_df(expenses)
    income_df(income)
    drop_down_df(dropdown_df)
    budget_df(budget)
    bank_df(bank)
    monthly_df(monthly_summary)
    showNotification("✅ Data refreshed from Access and display updated.", type = "message", duration = 3)
  })
}


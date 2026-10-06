server <- function(input, output, session) {

  # =========================
  # AUTHENTICATION
  # =========================

  authenticated <- reactiveVal(FALSE)
  login_failed <- reactiveVal(FALSE)


  # =========================
  # LOGIN / APP UI
  # =========================

  output$app_content <- renderUI({

    if (!authenticated()) {

      fluidPage(

        tags$head(
          tags$style(
            HTML("
              body {
                background-color: #f5f5f5;
              }

              .login-container {
                max-width: 400px;
                margin: 100px auto;
                padding: 30px;
                border-radius: 10px;
                box-shadow: 0 4px 20px rgba(0,0,0,0.15);
                background: white;
              }

              .login-title {
                text-align: center;
                margin-bottom: 30px;
              }

              .login-button {
                width: 100%;
              }

              .login-error {
                color: #d9534f;
                text-align: center;
                margin-top: 15px;
              }
            ")
          )
        ),

        div(
          class = "login-container",

          h2(
            "Financial Management",
            class = "login-title"
          ),

          textInput(
            "login_username",
            "Username"
          ),

          passwordInput(
            "login_password",
            "Password"
          ),

          actionButton(
            "login_button",
            "Login",
            class = "btn-primary login-button"
          ),

          if (login_failed()) {
            div(
              class = "login-error",
              "Incorrect username or password."
            )
          }
        )
      )

    } else {

      main_dashboard_ui()

    }

  })


  # =========================
  # LOGIN
  # =========================

  observeEvent(input$login_button, {

    username <- Sys.getenv("APP_USERNAME")
    password <- Sys.getenv("APP_PASSWORD")

    if (
      nzchar(username) &&
      nzchar(password) &&
      identical(input$login_username, username) &&
      identical(input$login_password, password)
    ) {

      login_failed(FALSE)
      authenticated(TRUE)

    } else {

      login_failed(TRUE)

    }

  })


  # =========================
  # MASTER DATA
  # =========================

  expense_df <- reactiveVal(expenses)
  income_df <- reactiveVal(income)
  drop_down_df <- reactiveVal(dropdown_df)
  budget_df <- reactiveVal(budget)
  bank_df <- reactiveVal(bank)
  monthly_df <- reactiveVal(monthly_summary)
  savings_df <- reactiveVal(savings_outcome)


  # =========================
  # FILTER MODULES
  # =========================

  filters_module_expenses <- mod_filters_server(
    "filters_expense",
    base_df = reactive(expense_df())
  )

  filter_map_expenses <- filters_module_expenses$filter_map


  filters_module_income <- mod_filters_server(
    "filters_income",
    base_df = reactive(income_df())
  )

  filter_map_income <- filters_module_income$filter_map


  filters_module_budget <- mod_filters_server(
    "filters_budget",
    base_df = reactive(budget_df())
  )

  filter_map_budget <- filters_module_budget$filter_map


  filters_module_dropdown <- mod_filters_server(
    "filters_dropdown",
    base_df = reactive(drop_down_df())
  )

  filter_map_dropdown <- filters_module_dropdown$filter_map


  filters_module_summary <- mod_filters_server(
    "filters_summary",
    base_df = reactive(monthly_df())
  )

  filter_map_summary <- filters_module_summary$filter_map


  # =========================
  # FILTERED DATA
  # =========================

  filtered_dropdown <- reactive({

    req(authenticated())

    df <- drop_down_df()
    filters <- filters_module_dropdown$applied_filters()

    for (id in names(filters)) {

      col <- filter_map_dropdown[[id]]
      val <- filters[[id]]

      if (
        !is.null(val) &&
        length(val) > 0 &&
        col %in% names(df)
      ) {

        df <- df[df[[col]] %in% val, ]

      }
    }

    df
  })


  filtered_expense <- reactive({

    req(authenticated())

    df <- expense_df()
    filters <- filters_module_expenses$applied_filters()

    for (id in names(filters)) {

      col <- filter_map_expenses[[id]]
      val <- filters[[id]]

      if (
        !is.null(val) &&
        length(val) > 0 &&
        col %in% names(df)
      ) {

        df <- df[df[[col]] %in% val, ]

      }
    }

    df
  })


  filtered_income <- reactive({

    req(authenticated())

    df <- income_df()
    filters <- filters_module_income$applied_filters()

    for (id in names(filters)) {

      col <- filter_map_income[[id]]
      val <- filters[[id]]

      if (
        !is.null(val) &&
        length(val) > 0 &&
        col %in% names(df)
      ) {

        df <- df[df[[col]] %in% val, ]

      }
    }

    df
  })


  filtered_bank <- reactive({

    req(authenticated())

    df <- bank_df()
    filters <- filters_module_expenses$applied_filters()

    for (id in names(filters)) {

      col <- filter_map_expenses[[id]]
      val <- filters[[id]]

      if (
        !is.null(val) &&
        length(val) > 0 &&
        col %in% names(df)
      ) {

        df <- df[df[[col]] %in% val, ]

      }
    }

    df
  })


  filtered_budget <- reactive({

    req(authenticated())

    df <- budget_df()
    filters <- filters_module_budget$applied_filters()

    for (id in names(filters)) {

      col <- filter_map_budget[[id]]
      val <- filters[[id]]

      if (
        !is.null(val) &&
        length(val) > 0 &&
        col %in% names(df)
      ) {

        df <- df[df[[col]] %in% val, ]

      }
    }

    df
  })


  filtered_monthly <- reactive({

    req(authenticated())

    df <- monthly_df()
    filters <- filters_module_summary$applied_filters()

    for (id in names(filters)) {

      # IMPORTANT: summary filter map
      col <- filter_map_summary[[id]]
      val <- filters[[id]]

      if (
        !is.null(val) &&
        length(val) > 0 &&
        !is.null(col) &&
        col %in% names(df)
      ) {

        df <- df[df[[col]] %in% val, ]

      }
    }

    df
  })


  filtered_savings <- reactive({

    req(authenticated())

    df <- savings_df()
    filters <- filters_module_summary$applied_filters()

    for (id in names(filters)) {

      # IMPORTANT: summary filter map
      col <- filter_map_summary[[id]]
      val <- filters[[id]]

      if (
        !is.null(val) &&
        length(val) > 0 &&
        !is.null(col) &&
        col %in% names(df)
      ) {

        df <- df[df[[col]] %in% val, ]

      }
    }

    df
  })


  # =========================
  # APP MODULES
  # =========================

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


  # =========================
  # REFRESH
  # =========================

  observeEvent(input$refresh_data, {

    req(authenticated())

    showModal(
      modalDialog(
        title = "Refresh query",

        "Database will be refreshed. Any unsaved changes will be lost.",

        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "confirm_refresh",
            "Proceed Anyway"
          )
        )
      )
    )

  })


  observeEvent(input$confirm_refresh, {

    req(authenticated())

    removeModal()

    result <- get_data()

    expenses <- as.data.table(result$Expenses)
    budget <- as.data.table(result$Budget)
    dropdown_new <- as.data.table(result$Dropdown)
    bank <- as.data.table(result$Bank)
    income <- as.data.table(result$Income)
    monthly_summary <- as.data.table(result$Monthly_summary)
    savings_outcome_new <- as.data.table(result$Savings_outcome)

    expense_df(expenses)
    income_df(income)
    drop_down_df(dropdown_new)
    budget_df(budget)
    bank_df(bank)
    monthly_df(monthly_summary)
    savings_df(savings_outcome_new)

    showNotification(
      "✅ Data refreshed from database.",
      type = "message",
      duration = 3
    )

  })

}
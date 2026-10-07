
mod_main_server <- function(id, monthly_df, savings_df, savings_outcome) {
  moduleServer(id, function(input, output, session) {
  ns <- NS(id)


income_categories <- c(
  "Έσοδα",
  "Έκτακτα Έσοδα"
)

investment_categories <- c(
  "Επενδυτικό",
  "Χρηματιστήριο"
)

investment_sale_categories <- c(
  "Πώληση Επενδυτικού",
  "Πώληση Χρηματιστηρίου"
)

excluded_categories <- c(
  income_categories,
  "Αποταμίευση"
)




output$total_expenses <- renderValueBox({

  df <- monthly_df()

  total <- sum(
    df$Amount[
      !df$`Primary Category` %in% excluded_categories
    ],
    na.rm = TRUE
  )

  valueBox(
    paste0(
      "€",
      format(total, big.mark = ",")
    ),
    "Total Expenses",
    icon = icon("money-bill-wave"),
    color = "red"
  )
})

output$total_income <- renderValueBox({
  total <- sum(monthly_df()$Amount[monthly_df()$`Primary Category`  %in% excluded_categories], na.rm = TRUE)
  valueBox(paste0("€", format(total, big.mark = ",")), "Total Income", icon = icon("coins"), color = "olive")
})

output$total_savings <- renderValueBox({
  total <- sum(savings_comparison()$`Actual savings`, na.rm = TRUE)
  valueBox(paste0("€", format(total, big.mark = ",")), "Total Savings", icon = icon("coins"), color = "green")
})

output$total_investment_gain <- renderValueBox({

  df <- financial_assets() %>% arrange(Year, Month_num)

  gain_for <- function(balance, additions, sales) {
    idx <- which(!is.na(balance))
    if (length(idx) < 2) return(0)

    first <- min(idx)
    last_i <- max(idx)
    flows <- (first + 1):last_i

    balance[last_i] - balance[first] -
      sum(additions[flows], na.rm = TRUE) +
      sum(sales[flows],     na.rm = TRUE)
  }

  total <-
    gain_for(df$`Investment accounts`, df$`Investment additions`, df$`Investment sales`) +
    gain_for(df$`Stock market`,        df$`Stock additions`,      df$`Stock sales`)

  valueBox(
    paste0("€", format(round(total), big.mark = ","), ""),
    "Total Investment gains",
    icon  = icon("coins"),
    color = "orange"
  )
})

output$total_investment <- renderValueBox({

  df <- savings_df() %>%
        filter(`Primary Category` == "Αποταμίευση" & !is.na(`Bank balance`) & `Secondary Category` == "Επενδυτικό") %>%
        group_by(Year, Month_num) %>%
        summarize(total_investment = sum(`Bank balance`, na.rm = TRUE)) %>%
        arrange(Year, Month_num)

  total_investment <- last(df$total_investment)

  valueBox(paste0("€", format(total_investment, big.mark = ",")), "Invest Balance", icon = icon("university"), color = "aqua")
})

output$bank_balance <- renderValueBox({

  latest_int <- monthly_df() %>% 
                filter(!is.na(`Bank balance`)) %>%
                group_by(Year, Month, Month_num, `Primary Category`) %>%
                summarize(`Bank balance` = sum(`Bank balance`, na.rm = TRUE)) %>%
                arrange(Year, Month_num)
  latest <-  tail(latest_int$`Bank balance`, 1)
  valueBox(paste0("€", format(latest, big.mark = ",")), "Bank Balance", icon = icon("university"), color = "aqua")
})

output$total_bank_gain <- renderValueBox({

df <- savings_df() %>% 
        filter(!is.na(`Bank balance`)) %>%
        group_by(Year, Month_num) %>%
        summarize(`Bank balance` = sum(`Bank balance`, na.rm = TRUE), .groups = "drop") %>%
        arrange(Year, Month_num) %>%
        mutate(
            bank_gain = round(`Bank balance` - lag(`Bank balance`),2)
        ) %>%
        arrange(Year, Month_num) %>%
        group_by(Year) %>%
        summarize(bank_gain = sum(bank_gain, na.rm = TRUE), .groups = "drop") %>%
        arrange(Year)

  total_bank_gain <- last(df$bank_gain)

  valueBox(paste0("€", format(total_bank_gain, big.mark = ",")), "Total bank gain", icon = icon("university"), color = "orange")
})

output$total_stock_gain <- renderValueBox({

df <- savings_outcome %>% 
filter(
    `Primary Category` == "Αποταμίευση",
    grepl("Χρηματιστ",`Secondary Category`)
  ) %>%
  
  group_by(Year, Month_num) %>%
  
  summarize(
    total_invested = sum(
      Amount[`Secondary Category` == "Χρηματιστήριο"],
      na.rm = TRUE
    ),
    
    total_invested_sell = sum(
      Amount[grepl("Πώληση Χρηματιστηρίου", `Secondary Category`)],
      na.rm = TRUE
    ),
    
    total_invest_balance = sum(
      `Bank balance`[`Secondary Category` == "Χρηματιστήριο"],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(Year, Month_num) %>%
  
  mutate(
    total_invested_sell = cumsum(total_invested_sell),
    total_invested = cumsum(total_invested),
    
    total_invested_amount =
      total_invested - total_invested_sell,
    
    total_invest_gains =
      round(
        total_invest_balance - total_invested_amount,
        2
      )
  )
  selected_data <- savings_df() %>% filter(`Primary Category` == "Αποταμίευση")

latest_selected_date <- selected_data %>%
  transmute(
    Year = as.numeric(Year),
    Month_num = as.numeric(Month_num)
  ) %>%
  arrange(Year, Month_num) %>%
  slice_tail(n = 1)


latest_gains <- df %>%
  mutate(
    Year = as.numeric(Year),
    Month_num = as.numeric(Month_num)
  ) %>%
  filter(
    Year == latest_selected_date$Year,
    Month_num == latest_selected_date$Month_num
  ) %>%
  pull(total_invest_gains)


  valueBox(
    paste0("€", format(round(latest_gains), big.mark = ","), ""),
    "Total stock gains",
    icon  = icon("coins"),
    color = "orange"
  )
})

output$total_investment_gain <- renderValueBox({

df <- savings_outcome %>% 
  filter(
    `Primary Category` == "Αποταμίευση",
    `Secondary Category` != "Τραπεζικοί Λογαριασμοί"
  ) %>%
  
  group_by(Year, Month_num) %>%
  
  summarize(
    total_invested = sum(
      Amount[`Secondary Category` == "Επενδυτικό"],
      na.rm = TRUE
    ),
    
    total_invested_sell = sum(
      Amount[grepl("Πώληση Επενδυτικού", `Secondary Category`)],
      na.rm = TRUE
    ),
    
    total_invest_balance = sum(
      `Bank balance`[`Secondary Category` == "Επενδυτικό"],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  arrange(Year, Month_num) %>%
  
  mutate(
    total_invested_sell = cumsum(total_invested_sell),
    total_invested = cumsum(total_invested),
    
    total_invested_amount =
      total_invested - total_invested_sell,
    
    total_invest_gains =
      round(
        total_invest_balance - total_invested_amount,
        2
      )
  )
  selected_data <- savings_df() %>% filter(`Primary Category` == "Αποταμίευση")

latest_selected_date <- selected_data %>%
  transmute(
    Year = as.numeric(Year),
    Month_num = as.numeric(Month_num)
  ) %>%
  arrange(Year, Month_num) %>%
  slice_tail(n = 1)


latest_gains <- df %>%
  mutate(
    Year = as.numeric(Year),
    Month_num = as.numeric(Month_num)
  ) %>%
  filter(
    Year == latest_selected_date$Year,
    Month_num == latest_selected_date$Month_num
  ) %>%
  pull(total_invest_gains)


  valueBox(
    paste0("€", format(round(latest_gains), big.mark = ","), ""),
    "Total Investment gains",
    icon  = icon("coins"),
    color = "orange"
  )
})

output$total_stock <- renderValueBox({

  df <- savings_df() %>%
        filter(`Primary Category` == "Αποταμίευση" & !is.na(`Bank balance`) & `Secondary Category` == "Χρηματιστήριο") %>%
        group_by(Year, Month_num) %>%
        summarize(total_stock = sum(`Bank balance`, na.rm = TRUE)) %>%
        arrange(Year, Month_num)

  total_stock <- last(df$total_stock)

  valueBox(paste0("€", format(total_stock, big.mark = ",")), "Stock Balance", icon = icon("university"), color = "aqua")
})

expense_vs_budget <- reactive({
  req(input$theme)
  if(input$theme == "Date / Primary Category") {

  group_cols <- c("Date", "Primary Category")

  monthly_df() %>%
  mutate(Month = factor(Month, levels = month.name, ordered = TRUE)) %>%
    filter(!`Primary Category`  %in% excluded_categories, !is.na(Amount)) %>% 
    group_by(across(all_of(group_cols))) %>%
    summarize(Amount = sum(Amount,na.rm = TRUE),
              Budget = sum(Budget, na.rm = TRUE),
              Max_Amount = pmax(Amount, Budget, na.rm = TRUE)) %>%
    mutate(color = ifelse(Amount > Budget, "Over Budget", "Within Budget"))
    } else if(input$theme == "Date") {
  group_cols <- c("Date")

  monthly_df() %>%
  mutate(Month = factor(Month, levels = month.name, ordered = TRUE)) %>%
    filter(!`Primary Category`  %in% excluded_categories, !is.na(Amount)) %>% 
    group_by(across(all_of(group_cols))) %>%
    summarize(Amount = sum(Amount,na.rm = TRUE),
              Budget = sum(Budget, na.rm = TRUE),
              Max_Amount = pmax(Amount, Budget, na.rm = TRUE))  %>%
    mutate(color = ifelse(Amount > Budget, "Over Budget", "Within Budget"))

    } else if(input$theme == "Year") {
  group_cols <- c("Year")

  monthly_df() %>%
  mutate(Month = factor(Month, levels = month.name, ordered = TRUE)) %>%
    filter(!`Primary Category`  %in% excluded_categories, !is.na(Amount)) %>% 
    group_by(across(all_of(group_cols))) %>%
    summarize(Amount = sum(Amount,na.rm = TRUE),
              Budget = sum(Budget, na.rm = TRUE),
              Max_Amount = pmax(Amount, Budget, na.rm = TRUE))  %>%
    mutate(color = ifelse(Amount > Budget, "Over Budget", "Within Budget"))
    }
})

observe({

key_columns <- setdiff(names(expense_vs_budget()), c("Amount"))
updateSelectInput(session, "key_expense_vs_budget", choices = c("", key_columns))
updateSelectInput(session, "x_expense_vs_budget", choices = c(key_columns), selected = ifelse(input$theme == "Year", "Year", "Date"))

})

output$expense_vs_budget <- renderPlotly({
  req(input$x_expense_vs_budget)

key_par <- if(is.null(input$key_expense_vs_budget) || length(input$key_expense_vs_budget) == 0) {
  if(input$theme == "Year") {
    "Year"} else {
  "Date"}
} else {
  c(input$key_expense_vs_budget)
}

x_par <- if(is.null(input$x_expense_vs_budget) || length(input$x_expense_vs_budget) == 0) {
  "Year"
} else {
  input$x_expense_vs_budget
}

bar <- bar_plot(expense_vs_budget(), x = x_par, y = "Max_Amount", fill = "color", title = "Expense vs Budget", key = key_par, source = "bar_plot")

})


clicked_bar <- reactive({
  click <- event_data("plotly_click", source = "bar_plot")
  req(click)
  click
})

filtered_by_click <- reactive({
  req(clicked_bar())  # Ensure a click exists
  data <- expense_vs_budget()
  click <- clicked_bar()

  # 1️⃣ Determine key columns
  key_cols <- unique(c(input$key_expense_vs_budget))
  if (length(key_cols) == 0) return(data)  # Nothing to filter on

  # 2️⃣ Convert click$key to character
  key_str <- as.character(click$key)

  # 3️⃣ Split click$key if multiple columns
  selected_values <- if(length(key_cols) == 1) {
    key_str
  } else {
    strsplit(key_str, "\\.")[[1]]
  }

  # 4️⃣ Safety check: lengths must match
  if(length(selected_values) != length(key_cols)) {
    warning("Clicked key length does not match number of key columns. Truncating to match.")
    selected_values <- selected_values[seq_along(key_cols)]
  }

  # 5️⃣ Convert selected values to correct type matching the data
  for(i in seq_along(key_cols)) {
    col_type <- class(data[[key_cols[i]]])
    if(col_type %in% c("numeric", "integer")) {
      selected_values[i] <- as.numeric(selected_values[i])
    } else if(col_type == "factor") {
      selected_values[i] <- factor(selected_values[i], levels = levels(data[[key_cols[i]]]))
    } else {
      selected_values[i] <- as.character(selected_values[i])
    }
  }

  # 6️⃣ Build dynamic filter expression
  filter_expr <- purrr::map2(key_cols, selected_values, 
                             ~ rlang::expr(.data[[!!.x]] == !!.y))

  combined_expr <- purrr::reduce(filter_expr, ~ rlang::expr((!!.x) & (!!.y)))

  # 7️⃣ Apply filter
  dplyr::filter(data, !!combined_expr)
})

output$data_table <- renderReactable({
  full_data_for_table <- filtered_by_click()
  columns <- names(full_data_for_table)
  
  static_header_style <- list(
    whiteSpace = "normal",
    overflow = "hidden",
    textAlign = "left",
    verticalAlign = "bottom",
    backgroundColor = "#3563bf",
    color = "white",
    fontFamily = "Calibri",
    fontSize = "12px",
    fontWeight = "bold"
  )

  # Build a named list of colDefs
  static_columns <- lapply(columns, function(name) {
    if (name == "Amount" | name == "Budget") {
      colDef(
        name = name,
        headerVAlign = "bottom",
        headerStyle = static_header_style,
        footer = function(values) {
          paste0("Total: ", sum(values, na.rm = TRUE))
        }
      )
    } else {
      colDef(name = name, headerVAlign = "bottom", headerStyle = static_header_style)
    }
  })
  names(static_columns) <- columns  # Name the list elements

  reactable(
    full_data_for_table,
    selection = "multiple",
    columns = static_columns,
    pagination = TRUE,
    compact = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    wrap = TRUE,
    bordered = TRUE,
    searchable = TRUE,
    defaultPageSize = 50,
    onClick = "select",
    theme = reactableTheme(
      borderColor = "#3563bf",
      style = list(
        fontFamily = "Calibri",
        fontSize = "12px"
      ),
      rowSelectedStyle = list(
        backgroundColor = "#cce0ff"
      ),
      searchInputStyle = list(
        height = "40px",
        width = "300px"
      )
    )
  )
})

# ============================================================
# INCOME VS EXPENSE PLOT
# ============================================================

output$income_vs_expense <- renderPlotly({

  df <- monthly_df()

  # ----------------------------------------------------------
  # DATE VIEW
  # ----------------------------------------------------------

if (input$theme_inc_vs_exp == "Date") {

  df <- df %>%
    group_by(Date) %>%
    summarize(

      Income = sum(
        Amount[
          `Primary Category` %in% income_categories
        ],
        na.rm = TRUE
      ),

      Expense = sum(
        Amount[
          !`Primary Category` %in% excluded_categories
        ],
        na.rm = TRUE
      ),

      .groups = "drop"
    ) %>%

    mutate(
      Difference = Income - Expense
    )

  x_par <- "Date"

} else {

    df <- df %>%
      group_by(Year) %>%
      summarize(

        Income = sum(
          Amount[`Primary Category`  %in% excluded_categories],
          na.rm = TRUE
        ),

        Expense = sum(
          Amount[!`Primary Category`  %in% excluded_categories],
          na.rm = TRUE
        ),

        .groups = "drop"
      ) %>%
      mutate(
        Difference = Income - Expense
      )

    x_par <- "Year"
  }


  # ==========================================================
  # LONG FORMAT
  # ==========================================================

  plot_df <- df %>%
    pivot_longer(
      cols = c(
        Income,
        Expense,
        Difference
      ),
      names_to = "Parameter",
      values_to = "Amount"
    ) %>%
    mutate(

      # Difference bar always points upwards
      Plot_Amount = if_else(
        Parameter == "Difference",
        abs(Amount),
        Amount
      ),

      # Determine bar colour
      Bar_Type = case_when(

        Parameter == "Income" ~
          "Income",

        Parameter == "Expense" ~
          "Expense",

        Parameter == "Difference" &
          Amount >= 0 ~
          "Positive Difference",

        Parameter == "Difference" &
          Amount < 0 ~
          "Negative Difference"
      ),

      # Used when clicking a bar
      click_key = as.character(
        .data[[x_par]]
      ),

      # Hover text
      hover_text = case_when(

        Parameter == "Difference" &
          Amount >= 0 ~

          paste0(
            "Income higher by: €",
            format(
              abs(Amount),
              big.mark = ",",
              digits = 2,
              nsmall = 2
            )
          ),

        Parameter == "Difference" &
          Amount < 0 ~

          paste0(
            "Expense higher by: €",
            format(
              abs(Amount),
              big.mark = ",",
              digits = 2,
              nsmall = 2
            )
          ),

        TRUE ~

          paste0(
            Parameter,
            ": €",
            format(
              Amount,
              big.mark = ",",
              digits = 2,
              nsmall = 2
            )
          )
      )
    )


  # ==========================================================
  # PLOT
  # ==========================================================

  p <- ggplot(
    plot_df,
    aes(
      x = .data[[x_par]],
      y = Plot_Amount,
      fill = Bar_Type,
      text = hover_text,
      key = click_key
    )
  ) +

    geom_col(
      position = position_dodge(
        width = 0.8
      ),
      width = 0.7
    ) +

    scale_fill_manual(
      values = c(
        "Income" = "#3498DB",
        "Expense" = "#F39C12",
        "Positive Difference" = "#2ECC71",
        "Negative Difference" = "#E74C3C"
      ),
      name = NULL
    ) +

    labs(
      title = "Income vs Expense",
      x = NULL,
      y = "Amount (€)"
    ) +

    theme_minimal() +

    theme(

      legend.position = "top",

      plot.title = element_text(
        face = "bold"
      ),

      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 0.5
      )
    )


  # ==========================================================
  # CONVERT TO PLOTLY
  # ==========================================================

plotly_plot <- ggplotly(
  p,
  tooltip = "text",
  source = "income_expense_plot"
) %>%
  layout(
    hovermode = "closest",

    autosize = TRUE,

    margin = list(
      l = 45,
      r = 10,
      b = 70,
      t = 45
    ),

    xaxis = list(
      automargin = TRUE
    ),

    yaxis = list(
      automargin = TRUE
    ),

    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.2
    )
  ) %>%
  config(
    responsive = TRUE,
    displayModeBar = FALSE
  )

plotly_plot <- event_register(
  plotly_plot,
  "plotly_click"
)

plotly_plot
})

# ============================================================
# CAPTURE BAR CLICK
# ============================================================

clicked_income_expense <- reactive({

  click <- event_data(
    "plotly_click",
    source = "income_expense_plot"
  )

  req(click)

  click
})

# ============================================================
# TABLE DATA BASED ON CLICK
# ============================================================

income_expense_table_data <- reactive({

  click <- clicked_income_expense()

  req(click$key)

  data <- monthly_df()

  selected_key <- as.character(
    click$key
  )


  # ==========================================================
  # DATE MODE
  #
  # Show transactions for selected date
  # ==========================================================

if (input$theme_inc_vs_exp == "Date") {

  selected_date <- selected_key

  result <- data %>%

    filter(
      Date == selected_date
    ) %>%

    # Sum each category
    group_by(
      Year,
      Month,
      `Primary Category`
    ) %>%

    summarise(
      Amount = sum(Amount, na.rm = TRUE),
      .groups = "drop"
    ) %>%

    # Calculate total expenses
    mutate(
      Total_Expenses = sum(
        Amount[!`Primary Category`  %in% excluded_categories],
        na.rm = TRUE
      ),

      `% of Expenses` = if_else(
        !`Primary Category`  %in% excluded_categories &
          Total_Expenses > 0,

        round(Amount / Total_Expenses * 100, 1),

        NA_real_
      )
    ) %>%

    arrange(
      `Primary Category`  %in% excluded_categories,
      desc(Amount)
    ) %>%

    select(
      Year,
      Month,
      `Primary Category`,
      Amount,
      `% of Expenses`
    )

  return(result)
}

  # ==========================================================
  # YEAR MODE
  #
  # Show MONTHLY summary
  # ==========================================================
selected_year <- as.numeric(selected_key)

result <- data %>%

  filter(
    Year == selected_year
  ) %>%

  group_by(
    Month_num,
    Month
  ) %>%

  summarize(

    Income = sum(
      Amount[
        `Primary Category`  %in% income_categories
      ],
      na.rm = TRUE
    ),

    Expense = sum(
      Amount[
        !`Primary Category`  %in% excluded_categories
      ],
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%

  mutate(
    Difference = Income - Expense
  ) %>%

  arrange(
    Month_num
  ) %>%

  select(
    Month,
    Income,
    Expense,
    Difference
  )
  result
})

# ============================================================
# INCOME / EXPENSE DETAIL TABLE
# ============================================================

output$income_expense_table <- renderReactable({

  df <- income_expense_table_data()

  req(
    nrow(df) > 0
  )


  # ==========================================================
  # DATE MODE
  # ==========================================================

if (input$theme_inc_vs_exp == "Date") {

  # ==========================================================
  # Calculate income / expense totals
  # ==========================================================

  total_income <- sum(
    df$Amount[
      df$`Primary Category`  %in% excluded_categories
    ],
    na.rm = TRUE
  )

  total_expense <- sum(
    df$Amount[
      !df$`Primary Category`  %in% excluded_categories
    ],
    na.rm = TRUE
  )

  # Absolute difference
  total_difference <- abs(
    total_income - total_expense
  )

  # Determine colour
  difference_color <- if (
    total_income >= total_expense
  ) {
    "#007d00"
  } else {
    "red"
  }


  # ==========================================================
  # Reactable
  # ==========================================================

  reactable(

    df,

    selection = "multiple",

    pagination = TRUE,

    defaultPageSize = 20,

    compact = TRUE,

    highlight = TRUE,

    resizable = TRUE,

    wrap = TRUE,

    bordered = TRUE,

    searchable = TRUE,

    columns = list(

      Amount = colDef(

        format = colFormat(
          prefix = "€",
          separators = TRUE,
          digits = 2
        ),

        footer = function(values) {

          div(

            style = list(
              color = difference_color,
              fontWeight = "bold"
            ),

            paste0(
              "Difference: €",
              format(
                total_difference,
                big.mark = ",",
                nsmall = 2
              )
            )
          )
        }
      ),
      `% of Expenses` = colDef(
        format = colFormat(
          suffix = "%"
      ))

    ),

    theme = reactableTheme(

      borderColor = "#3563bf",

      style = list(
        fontFamily = "Calibri",
        fontSize = "12px"
      ),

      headerStyle = list(
        backgroundColor = "#3563bf",
        color = "white",
        fontWeight = "bold"
      ),

      rowSelectedStyle = list(
        backgroundColor = "#cce0ff"
      )
    )
  )

  # ==========================================================
  # YEAR MODE
  #
  # Monthly summary table
  # ==========================================================

  } else {

    reactable(

      df,

      pagination = FALSE,

      compact = TRUE,

      highlight = TRUE,

      resizable = TRUE,

      bordered = TRUE,

      columns = list(

        # --------------------------------------------------
        # Month
        # --------------------------------------------------

        Month = colDef(
          name = "Month",
          minWidth = 100
        ),


        # --------------------------------------------------
        # Income
        # --------------------------------------------------

        Income = colDef(

          name = "Income",

          align = "right",

          format = colFormat(
            prefix = "€",
            separators = TRUE,
            digits = 2
          ),

          footer = function(values) {

            paste0(
              "€",
              format(
                sum(
                  values,
                  na.rm = TRUE
                ),
                big.mark = ",",
                nsmall = 2
              )
            )
          }
        ),


        # --------------------------------------------------
        # Expense
        # --------------------------------------------------

        Expense = colDef(

          name = "Expense",

          align = "right",

          format = colFormat(
            prefix = "€",
            separators = TRUE,
            digits = 2
          ),

          footer = function(values) {

            paste0(
              "€",
              format(
                sum(
                  values,
                  na.rm = TRUE
                ),
                big.mark = ",",
                nsmall = 2
              )
            )
          }
        ),


        # --------------------------------------------------
        # Difference
        # --------------------------------------------------

        Difference = colDef(

          name = "Difference",

          align = "right",

          format = colFormat(
            prefix = "€",
            separators = TRUE,
            digits = 2
          ),

          style = function(value) {

            if (is.na(value)) {
              return(NULL)
            }

            if (value > 0) {

              list(
                color = "green",
                fontWeight = "bold"
              )

            } else if (value < 0) {

              list(
                color = "red",
                fontWeight = "bold"
              )

            } else {

              list(
                fontWeight = "bold"
              )
            }
          },

          footer = function(values) {

            total <- sum(
              values,
              na.rm = TRUE
            )

            div(
              style = list(
                color = if (
                  total >= 0
                ) "green" else "red",
                fontWeight = "bold"
              ),

              paste0(
                "€",
                format(
                  total,
                  big.mark = ",",
                  nsmall = 2
                )
              )
            )
          }
        )
      ),


      # ====================================================
      # TABLE THEME
      # ====================================================

      theme = reactableTheme(

        borderColor = "#3563bf",

        style = list(
          fontFamily = "Calibri",
          fontSize = "12px"
        ),

        headerStyle = list(
          backgroundColor = "#3563bf",
          color = "white",
          fontWeight = "bold"
        )
      )
    )
  }
})

financial_assets <- reactive({

  df <- savings_df() %>%
    mutate(
      Year = as.numeric(Year),
      Month_num = as.numeric(Month_num),
      Date = make_date(Year, Month_num, 1),

      # Protect against accidental spaces
      `Primary Category` = trimws(`Primary Category`),
      `Secondary Category` = trimws(`Secondary Category`)
    )

  df %>%
    group_by(
      Year,
      Month_num,
      Date
    ) %>%
    summarise(

      # =========================
      # BALANCES
      # =========================

      `Bank accounts` = {
        x <- `Bank balance`[
          `Primary Category` == "Αποταμίευση" &
          `Secondary Category` == "Τραπεζικοί Λογαριασμοί"
        ]

        if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
      },

      `Investment accounts` = {
        x <- `Bank balance`[
          `Primary Category` == "Αποταμίευση" &
          `Secondary Category` == "Επενδυτικό"
        ]

        if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
      },

      `Stock market` = {
        x <- `Bank balance`[
          `Primary Category` == "Αποταμίευση" &
          `Secondary Category` == "Χρηματιστήριο"
        ]

        if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
      },


      # =========================
      # TRANSACTIONS
      # =========================

      `Investment additions` = sum(
        Amount[
          `Primary Category` == "Αποταμίευση" &
          `Secondary Category` == "Επενδυτικό"
        ],
        na.rm = TRUE
      ),

      `Investment sales` = sum(
        Amount[
          `Primary Category` == "Αποταμίευση" &
          `Secondary Category` == "Πώληση Επενδυτικού"
        ],
        na.rm = TRUE
      ),

      `Stock additions` = sum(
        Amount[
          `Primary Category` == "Αποταμίευση" &
          `Secondary Category` == "Χρηματιστήριο"
        ],
        na.rm = TRUE
      ),

      `Stock sales` = sum(
        Amount[
          `Primary Category` == "Αποταμίευση" &
          `Secondary Category` == "Πώληση Χρηματιστηρίου"
        ],
        na.rm = TRUE
      ),

      .groups = "drop"
    ) %>%

    arrange(Date) %>%

    mutate(

      # =========================
      # TOTAL ASSETS
      # =========================

      # Only calculate total when ALL 3 balances exist
    `Total financial assets` =
      coalesce(`Bank accounts`, 0) +
      coalesce(`Investment accounts`, 0) +
      coalesce(`Stock market`, 0) -
      coalesce(`Investment sales`, 0) -
      coalesce(`Stock sales`, 0) +
      coalesce(`Investment additions`, 0) +
      coalesce(`Stock additions`, 0),

      # =========================
      # PREVIOUS MONTH
      # =========================

      Previous_Date = lag(Date),

      Consecutive_Month =
        !is.na(Previous_Date) &
        Previous_Date == (Date %m-% months(1)),


      # =========================
      # WEALTH CHANGE
      # =========================

      `Actual wealth change` = if_else(
        Consecutive_Month &
          !is.na(`Total financial assets`) &          
          `Total financial assets` != 0 &
          !is.na(lag(`Total financial assets`)),

        `Total financial assets` -
          lag(`Total financial assets`),

        NA_real_
      ),


      # =========================
      # INVESTMENT ACCOUNT GAIN
      # =========================

      `Investment gain` = if_else(
        Consecutive_Month &
          !is.na(`Investment accounts`) &
          !is.na(lag(`Investment accounts`)),

        `Investment accounts` -
          lag(`Investment accounts`) -
          `Investment additions` +
          `Investment sales`,

        NA_real_
      ),


      # =========================
      # STOCK MARKET GAIN
      # =========================

      `Stock gain` = if_else(
        Consecutive_Month &
          !is.na(`Stock market`) &
          !is.na(lag(`Stock market`)),

        `Stock market` -
          lag(`Stock market`) -
          `Stock additions` +
          `Stock sales`,

        NA_real_
      ),


      # =========================
      # TOTAL INVESTMENT GAIN
      # =========================

      `Investment gain total` = 
      coalesce(`Investment gain`, 0) +
      coalesce(`Stock gain`, 0)
    )
})


output$savings <- renderPlotly({

 df <- savings_comparison() %>%
  select(
    Date,
    `Theoretical savings`,
    `Actual savings`
  ) %>%
  
  mutate(
    Date = factor(
      format(Date, "%Y-%m"),
      levels = unique(format(Date, "%Y-%m"))
    )
  ) %>%
  
  pivot_longer(
    cols = c(
      `Theoretical savings`,
      `Actual savings`
    ),
    names_to = "Parameter",
    values_to = "Amount"
  ) %>%
  
  filter(
    !is.na(Amount),
    is.finite(Amount)
  )

req(nrow(df) > 0)

scatter_plot(
  df,
  Date,
  Amount,
  Parameter,
  "Actual vs Theoretical Savings",
  data_labels = FALSE
)

})

output$investment_gain_plot <- renderPlotly({

  df <- financial_assets() %>%
    select(
      Date,
      `Investment gain`,
      `Stock gain`,
      `Investment gain total`
    ) %>%

    pivot_longer(
      cols = c(
        `Investment gain`,
        `Stock gain`,
        `Investment gain total`
      ),
      names_to = "Parameter",
      values_to = "Amount"
    ) %>%

    filter(
      !is.na(Amount),
      is.finite(Amount)
    ) %>%
  
  mutate(
    Date = factor(
      format(Date, "%Y-%m"),
      levels = unique(format(Date, "%Y-%m"))
    )
  ) 

  req(nrow(df) > 0)

  scatter_plot(
    df,
    Date,
    Amount,
    Parameter,
    "Investment Gains / Losses",
  data_labels = FALSE
  )

})

output$financial_assets_plot <- renderPlotly({

  df <- financial_assets() %>%
    select(
      Date,
      `Bank accounts`,
      `Investment accounts`,
      `Stock market`,
      `Total financial assets`
    ) %>%
    pivot_longer(
      cols = c(
        `Bank accounts`,
        `Investment accounts`,
        `Stock market`,
        `Total financial assets`
      ),
      names_to = "Parameter",
      values_to = "Amount"
    ) %>%
    filter(
      !is.na(Date),
      !is.na(Amount),
      is.finite(Amount)
    ) %>%
  
  mutate(
    Date = factor(
      format(Date, "%Y-%m"),
      levels = unique(format(Date, "%Y-%m"))
    )
  ) 

  req(nrow(df) > 0)

  scatter_plot(
    df,
    Date,
    Amount,
    Parameter,
    "Financial Assets",
  data_labels = FALSE
  )

})

savings_comparison <- reactive({

  # -------------------------
  # CASH FLOW
  # -------------------------

  cashflow <- savings_df() %>%
    group_by(
      Year,
      Month_num
    ) %>%
    summarise(

      Income = sum(
        Amount[
          `Primary Category` %in% income_categories
        ],
        na.rm = TRUE
      ),

      Expense = sum(
        Amount[
          !`Primary Category` %in% excluded_categories
        ],
        na.rm = TRUE
      ),

      .groups = "drop"
    ) %>%

    mutate(
      Date = make_date(
        Year,
        Month_num,
        1
      ),

      `Theoretical savings` =
        Income - Expense
    )


  # -------------------------
  # JOIN ASSETS
  # -------------------------

  cashflow %>%

    left_join(
      financial_assets(),
      by = c(
        "Year",
        "Month_num",
        "Date"
      )
    ) %>%

    arrange(Date) %>%

    mutate(

      `Actual savings` =
        `Actual wealth change` -
        `Investment gain total`,

      Difference =
        `Actual savings` -
        `Theoretical savings`
    )

})


yoy_data <- reactive({

  df <- monthly_df()

  df %>%
    group_by(
      Year,
      Month_num,
      Month
    ) %>%
    summarise(

      Income = sum(
        Amount[`Primary Category`  %in% income_categories],
        na.rm = TRUE
      ),

      Expenses = sum(
        Amount[!`Primary Category`  %in% excluded_categories],
        na.rm = TRUE
      ),

      .groups = "drop"
    ) %>%

    mutate(
      Savings = Income - Expenses
    ) %>%

    arrange(
      Year,
      Month_num
    )
})

output$yoy_plot <- renderPlotly({

  df <- yoy_data()

  req(input$yoy_parameter)

  parameter <- input$yoy_parameter

  df <- df %>%
    mutate(
      Value = .data[[parameter]],
      Year = as.character(Year),

      hover_text = paste0(
        Month,
        " ",
        Year,
        "<br>",
        parameter,
        ": €",
        format(
          Value,
          big.mark = ",",
          nsmall = 2
        )
      )
    )

  p <- plot_ly(
    data = df,

    x = ~Month_num,
    y = ~Value,

    color = ~Year,

    type = "scatter",
    mode = "lines+markers",

    customdata = ~Month_num,

    text = ~hover_text,
    hoverinfo = "text",

    source = "yoy_plot"
  ) %>%

    layout(

      title = paste(
        parameter,
        "- Year over Year"
      ),

      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = 1:12,
        ticktext = month.abb
      ),

      yaxis = list(
        title = "Amount (€)"
      ),

      hovermode = "closest"
    )

  event_register(
    p,
    "plotly_click"
  )
})

selected_yoy_month <- reactiveVal(NULL)

observeEvent(
  event_data(
    "plotly_click",
    source = "yoy_plot"
  ),
  {

    click <- event_data(
      "plotly_click",
      source = "yoy_plot"
    )

    req(click)

    month_selected <- as.integer(
      click$customdata
    )

    selected_yoy_month(
      month_selected
    )
  },
  ignoreInit = TRUE
)

yoy_table_data <- reactive({

  req(
    selected_yoy_month()
  )

  month_selected <- selected_yoy_month()

  df <- yoy_data() %>%

    filter(
      Month_num == month_selected
    ) %>%

    arrange(
      Year
    ) %>%

    select(
      Year,
      Month,
      Income,
      Expenses,
      Savings
    ) %>%

    mutate(

      `Income YoY %` = if_else(
        !is.na(lag(Income)) &
          lag(Income) != 0,

        (Income / lag(Income) - 1) * 100,

        NA_real_
      ),

      `Expenses YoY %` = if_else(
        !is.na(lag(Expenses)) &
          lag(Expenses) != 0,

        (Expenses / lag(Expenses) - 1) * 100,

        NA_real_
      ),

      `Savings YoY %` = if_else(
        !is.na(lag(Savings)) &
          lag(Savings) != 0,

        (Savings / lag(Savings) - 1) * 100,

        NA_real_
      )
    )

  df
})

output$yoy_table <- renderReactable({

  df <- yoy_table_data()

  req(
    nrow(df) > 0
  )

  reactable(

    df,

    pagination = FALSE,
    bordered = TRUE,
    highlight = TRUE,
    compact = TRUE,
    resizable = TRUE,

    columns = list(

      Year = colDef(
        name = "Year"
      ),

      Month = colDef(
        name = "Month"
      ),

      Income = colDef(
        name = "Income",
        align = "right",
        format = colFormat(
          prefix = "€",
          separators = TRUE,
          digits = 2
        )
      ),

      Expenses = colDef(
        name = "Expenses",
        align = "right",
        format = colFormat(
          prefix = "€",
          separators = TRUE,
          digits = 2
        )
      ),

      Savings = colDef(

        name = "Savings",
        align = "right",

        format = colFormat(
          prefix = "€",
          separators = TRUE,
          digits = 2
        ),

        style = function(value) {

          if (is.na(value))
            return(NULL)

          list(
            color = if (value >= 0)
              "green"
            else
              "red",

            fontWeight = "bold"
          )
        }
      ),

      `Income YoY %` = colDef(
        align = "right",
        format = colFormat(
          suffix = "%",
          digits = 1
        )
      ),

      `Expenses YoY %` = colDef(

        align = "right",

        format = colFormat(
          suffix = "%",
          digits = 1
        ),

        style = function(value) {

          if (is.na(value))
            return(NULL)

          list(
            color = if (value <= 0)
              "green"
            else
              "red"
          )
        }
      ),

      `Savings YoY %` = colDef(

        align = "right",

        format = colFormat(
          suffix = "%",
          digits = 1
        ),

        style = function(value) {

          if (is.na(value))
            return(NULL)

          list(
            color = if (value >= 0)
              "green"
            else
              "red"
          )
        }
      )
    ),

    theme = reactableTheme(

      borderColor = "#3563bf",

      style = list(
        fontFamily = "Calibri",
        fontSize = "12px"
      ),

      headerStyle = list(
        backgroundColor = "#3563bf",
        color = "white",
        fontWeight = "bold"
      )
    )
  )
})

yoy_category_data <- reactive({

  req(
    selected_yoy_month()
  )

  month_selected <- selected_yoy_month()


  # ==========================================================
  # FILTER SELECTED MONTH
  # ==========================================================

  df <- monthly_df() %>%

    filter(
      Month_num == month_selected,
      !`Primary Category`  %in% excluded_categories
    ) %>%

    group_by(
      Year,
      `Primary Category`
    ) %>%

    summarise(
      Amount = sum(
        Amount,
        na.rm = TRUE
      ),
      .groups = "drop"
    )


  req(
    nrow(df) > 0
  )


  # ==========================================================
  # YEARS AVAILABLE
  # ==========================================================

  years <- sort(
    unique(df$Year)
  )


  # ==========================================================
  # PIVOT YEARS TO COLUMNS
  # ==========================================================

  result <- df %>%

    tidyr::pivot_wider(

      names_from = Year,

      values_from = Amount,

      values_fill = 0
    )


  # ==========================================================
  # CALCULATE LATEST YEAR VS PREVIOUS YEAR
  # ==========================================================

  if (length(years) >= 2) {

    latest_year <- max(years)

    previous_year <- years[
      length(years) - 1
    ]


    latest_col <- as.character(
      latest_year
    )

    previous_col <- as.character(
      previous_year
    )


    # Dynamic column names
    change_euro_name <- paste0(
      latest_year,
      " vs ",
      previous_year,
      " €"
    )

    change_percent_name <- paste0(
      latest_year,
      " vs ",
      previous_year,
      " %"
    )


    result <- result %>%

      mutate(

        !!change_euro_name :=
          .data[[latest_col]] -
          .data[[previous_col]],


        !!change_percent_name :=
          if_else(

            .data[[previous_col]] != 0,

            (
              .data[[latest_col]] /
                .data[[previous_col]] -
                1
            ) * 100,

            NA_real_
          )
      )


    # ========================================================
    # SORT BY LATEST YEAR EXPENSE
    # ========================================================

    result <- result %>%

      arrange(
        desc(
          .data[[latest_col]]
        )
      )

  }


  result
})

output$yoy_category_table <- renderReactable({

  df <- yoy_category_data()

  req(
    nrow(df) > 0
  )


  # ==========================================================
  # Identify year columns
  # ==========================================================

  year_columns <- names(df)[
    grepl(
      "^[0-9]{4}$",
      names(df)
    )
  ]


  # ==========================================================
  # Dynamically create definitions for year columns
  # ==========================================================

  column_definitions <- list(

    `Primary Category` = colDef(

      name = "Category",

      minWidth = 160,

      footer = "Total",

      style = list(
        fontWeight = "bold"
      )
    )
  )


  # ==========================================================
  # Year columns
  # ==========================================================

  for (year in year_columns) {

    column_definitions[[year]] <- colDef(

      name = year,

      align = "right",

      format = colFormat(
        prefix = "€",
        separators = TRUE,
        digits = 2
      ),

      footer = function(values) {

        paste0(
          "€",
          format(
            sum(
              values,
              na.rm = TRUE
            ),
            big.mark = ",",
            nsmall = 2
          )
        )
      }
    )
  }


  # ==========================================================
  # Change €
  # ==========================================================

# ==========================================================
# Identify dynamic change columns
# ==========================================================

change_euro_col <- names(df)[
  grepl(
    " vs .* €$",
    names(df)
  )
]

change_percent_col <- names(df)[
  grepl(
    " vs .* %$",
    names(df)
  )
]


# ==========================================================
# Change € - dynamic column
# ==========================================================

if (length(change_euro_col) == 1) {

  column_definitions[[change_euro_col]] <- colDef(

    name = change_euro_col,

    align = "right",

    format = colFormat(
      prefix = "€",
      separators = TRUE,
      digits = 2
    ),

    style = function(value) {

      if (is.na(value)) {
        return(NULL)
      }

      # Increased expenses = bad
      if (value > 0) {

        list(
          color = "red",
          fontWeight = "bold"
        )

      } else if (value < 0) {

        list(
          color = "green",
          fontWeight = "bold"
        )

      } else {

        list(
          fontWeight = "bold"
        )
      }
    },

    footer = function(values) {

      total <- sum(
        values,
        na.rm = TRUE
      )

      div(

        style = list(
          color = if (total > 0) {

            "red"

          } else if (total < 0) {

            "green"

          } else {

            "black"
          },

          fontWeight = "bold"
        ),

        paste0(
          "€",
          format(
            total,
            big.mark = ",",
            nsmall = 2
          )
        )
      )
    }
  )
}


# ==========================================================
# Change % - dynamic column
# ==========================================================

if (length(change_percent_col) == 1) {

  column_definitions[[change_percent_col]] <- colDef(

    name = change_percent_col,

    align = "right",

    format = colFormat(
      suffix = "%",
      digits = 1
    ),

    style = function(value) {

      if (is.na(value)) {
        return(NULL)
      }

      # Increased expenses = bad
      if (value > 0) {

        list(
          color = "red",
          fontWeight = "bold"
        )

      } else if (value < 0) {

        list(
          color = "green",
          fontWeight = "bold"
        )

      } else {

        list(
          fontWeight = "bold"
        )
      }
    }
  )
}

  # ==========================================================
  # Table
  # ==========================================================

  reactable(

    df,

    pagination = FALSE,

    bordered = TRUE,

    highlight = TRUE,

    compact = TRUE,

    resizable = TRUE,

    searchable = FALSE,

    columns = column_definitions,

    defaultColDef = colDef(
      minWidth = 100
    ),

    theme = reactableTheme(

      borderColor = "#3563bf",

      style = list(
        fontFamily = "Calibri",
        fontSize = "12px"
      ),

      headerStyle = list(
        backgroundColor = "#3563bf",
        color = "white",
        fontWeight = "bold"
      )
    )
  )
})

monthly_financials <- reactive({

  df <- monthly_df()

  df %>%
    group_by(
      Year,
      Month_num
    ) %>%

    summarise(

      # Regular income ONLY
      Regular_Income = sum(
        Amount[
          `Primary Category` == "Έσοδα"
        ],
        na.rm = TRUE
      ),

      # Exceptional income ONLY
      Exceptional_Income = sum(
        Amount[
          `Primary Category` == "Έκτακτα Έσοδα"
        ],
        na.rm = TRUE
      ),

      # Everything except the two income categories
      Expenses = sum(
        Amount[
          !`Primary Category` %in% excluded_categories
        ],
        na.rm = TRUE
      ),

      .groups = "drop"
    ) %>%

    mutate(

      # Total actual income
      Income =
        Regular_Income +
        Exceptional_Income,

      # Actual savings including exceptional income
      Savings =
        Income -
        Expenses,

      # Savings from normal income only
      Regular_Savings =
        Regular_Income -
        Expenses,

      Savings_Rate = if_else(
        Income > 0,
        Savings / Income * 100,
        NA_real_
      ),

      Regular_Savings_Rate = if_else(
        Regular_Income > 0,
        Regular_Savings / Regular_Income * 100,
        NA_real_
      ),

      Date = as.Date(
        sprintf(
          "%04d-%02d-01",
          Year,
          Month_num
        )
      )
    ) %>%

    arrange(Date)
})

rolling_12_comparison <- reactive({

  df <- monthly_financials()

  req(
    nrow(df) >= 12
  )


  # ==========================================================
  # Latest available month
  # ==========================================================

  latest_date <- max(
    df$Date,
    na.rm = TRUE
  )


  # ==========================================================
  # Latest 12 months
  # ==========================================================

  current_start <- latest_date %m-% months(11)

  current <- df %>%

    filter(
      Date >= current_start,
      Date <= latest_date
    )


  # ==========================================================
  # Previous 12 months
  # ==========================================================

  previous_end <- current_start %m-% months(1)

  previous_start <- previous_end %m-% months(11)

  previous <- df %>%

    filter(
      Date >= previous_start,
      Date <= previous_end
    )


  # ==========================================================
  # Function to calculate period statistics
  # ==========================================================

  calculate_period <- function(x) {

    income <- sum(
      x$Income,
      na.rm = TRUE
    )

    expenses <- sum(
      x$Expenses,
      na.rm = TRUE
    )

    savings <- income - expenses

    savings_rate <- if (
      income > 0
    ) {

      savings / income * 100

    } else {

      NA_real_
    }


    tibble(

      Income = income,

      Expenses = expenses,

      Savings = savings,

      `Savings Rate` = savings_rate
    )
  }


  current_values <- calculate_period(
    current
  )

  previous_values <- calculate_period(
    previous
  )


  # ==========================================================
  # Convert into comparison table
  # ==========================================================

  result <- tibble(

    Metric = c(
      "Income",
      "Expenses",
      "Savings",
      "Savings Rate"
    ),

    `Latest 12M` = c(
      current_values$Income,
      current_values$Expenses,
      current_values$Savings,
      current_values$`Savings Rate`
    ),

    `Previous 12M` = c(
      previous_values$Income,
      previous_values$Expenses,
      previous_values$Savings,
      previous_values$`Savings Rate`
    )
  ) %>%

    mutate(

      Change = case_when(

        Metric == "Savings Rate" ~

          `Latest 12M` -
          `Previous 12M`,

        `Previous 12M` != 0 ~

          (
            `Latest 12M` /
              `Previous 12M` -
              1
          ) * 100,

        TRUE ~
          NA_real_
      )
    )


  result
})

output$rolling_12_summary <- renderReactable({

  df <- rolling_12_comparison()

  reactable(

    df,

    pagination = FALSE,

    bordered = TRUE,

    highlight = TRUE,

    compact = TRUE,

    columns = list(

      Metric = colDef(

        name = "Metric",

        style = list(
          fontWeight = "bold"
        )
      ),


      `Latest 12M` = colDef(

        name = "Latest 12 Months",

        align = "right",

        cell = function(value, index) {

          metric <- df$Metric[index]

          if (
            metric == "Savings Rate"
          ) {

            paste0(
              format(
                value,
                nsmall = 1,
                digits = 3
              ),
              "%"
            )

          } else {

            paste0(
              "€",
              format(
                value,
                big.mark = ",",
                nsmall = 2
              )
            )
          }
        }
      ),


      `Previous 12M` = colDef(

        name = "Previous 12 Months",

        align = "right",

        cell = function(value, index) {

          metric <- df$Metric[index]

          if (
            metric == "Savings Rate"
          ) {

            paste0(
              format(
                value,
                nsmall = 1,
                digits = 3
              ),
              "%"
            )

          } else {

            paste0(
              "€",
              format(
                value,
                big.mark = ",",
                nsmall = 2
              )
            )
          }
        }
      ),


      Change = colDef(

        name = "Change",

        align = "right",

        cell = function(value, index) {

          metric <- df$Metric[index]

          if (is.na(value)) {
            return("-")
          }


          label <- if (
            metric == "Savings Rate"
          ) {

            paste0(
              ifelse(
                value > 0,
                "+",
                ""
              ),

              format(
                value,
                nsmall = 1,
                digits = 3
              ),

              " pp"
            )

          } else {

            paste0(
              ifelse(
                value > 0,
                "+",
                ""
              ),

              format(
                value,
                nsmall = 1,
                digits = 3
              ),

              "%"
            )
          }


          # ================================================
          # Determine whether change is good or bad
          # ================================================

          good_change <- if (
            metric == "Expenses"
          ) {

            value <= 0

          } else {

            value >= 0
          }


          div(

            style = list(

              color = if (
                good_change
              ) {
                "green"
              } else {
                "red"
              },

              fontWeight = "bold"
            ),

            label
          )
        }
      )
    ),

    theme = reactableTheme(

      borderColor = "#3563bf",

      style = list(
        fontFamily = "Calibri",
        fontSize = "12px"
      ),

      headerStyle = list(
        backgroundColor = "#3563bf",
        color = "white",
        fontWeight = "bold"
      )
    )
  )
})

rolling_12_data <- reactive({

  df <- monthly_financials() %>%
    arrange(Date)

  req(
    nrow(df) >= 12
  )

  df %>%

    mutate(

      Rolling_Regular_Income =
        zoo::rollsum(
          Regular_Income,
          k = 12,
          fill = NA,
          align = "right"
        ),

      Rolling_Exceptional_Income =
        zoo::rollsum(
          Exceptional_Income,
          k = 12,
          fill = NA,
          align = "right"
        ),

      Rolling_Income =
        Rolling_Regular_Income +
        Rolling_Exceptional_Income,

      Rolling_Expenses =
        zoo::rollsum(
          Expenses,
          k = 12,
          fill = NA,
          align = "right"
        ),

      Rolling_Savings =
        Rolling_Income -
        Rolling_Expenses,

      Rolling_Regular_Savings =
        Rolling_Regular_Income -
        Rolling_Expenses,

      Rolling_Savings_Rate =
        if_else(
          Rolling_Income > 0,
          Rolling_Savings /
            Rolling_Income * 100,
          NA_real_
        ),

      Rolling_Regular_Savings_Rate =
        if_else(
          Rolling_Regular_Income > 0,
          Rolling_Regular_Savings /
            Rolling_Regular_Income * 100,
          NA_real_
        )
    ) %>%

    filter(
      !is.na(Rolling_Income)
    )
})

output$rolling_12_plot <- renderPlotly({

  df <- rolling_12_data()


plot_df <- df %>%

  select(
    Date,
    Rolling_Regular_Income,
    Rolling_Expenses,
    Rolling_Regular_Savings
  ) %>%

  pivot_longer(

    cols = c(
      Rolling_Regular_Income,
      Rolling_Expenses,
      Rolling_Regular_Savings
    ),

    names_to = "Parameter",
    values_to = "Amount"

  ) %>%

  mutate(

    Parameter = recode(
      Parameter,

      Rolling_Regular_Income =
        "Regular Income",

      Rolling_Expenses =
        "Expenses",

      Rolling_Regular_Savings =
        "Regular Savings"
    ),

    hover_text = paste0(

      format(
        Date,
        "%b %Y"
      ),

      "<br>",

      Parameter,

      ": €",

      format(
        Amount,
        big.mark = ",",
        nsmall = 2
      )
    )
  )

  p <- ggplot(
  plot_df,
  aes(
    x = Date,
    y = Amount,
    color = Parameter,
    text = hover_text
  )
) +

  geom_line(
    linewidth = 1
  ) +

  geom_point(
    size = 2
  ) +

  labs(
    title = "Rolling 12-Month Financial Performance",
    subtitle = "Each point represents the previous 12 months",
    x = NULL,
    y = "Amount (€)",
    color = NULL
  ) +

  theme_minimal() +

  theme(
    legend.position = "top",

    plot.title = element_text(
      face = "bold"
    ),

    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      size = 8
    )
  )


ggplotly(
  p,
  tooltip = "text"
) %>%

  layout(
    autosize = TRUE,

    margin = list(
      l = 50,
      r = 10,
      b = 60,
      t = 70
    ),

    xaxis = list(
      automargin = TRUE
    ),

    yaxis = list(
      automargin = TRUE
    ),

    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = 1.08,
      yanchor = "bottom"
    ),

    hovermode = "closest"
  ) %>%

  config(
    responsive = TRUE,
    displayModeBar = FALSE
  )
})

})}

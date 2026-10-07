theme_plot <- function(){
              theme(
       plot.title = element_text(size = 12, color = "red", face = "bold", hjust = 0.5),
       axis.text = element_text(size = 8),
       axis.title = element_text(size = 10, color = "red", face = "bold"),
       legend.position = "bottom",
       panel.grid = element_blank(),
       panel.background = element_rect(fill = "#f3f3f3"),
       plot.background = element_rect(fill = "#e9ffdd")
       )
}

format_sql_text <- function(value) {
  if (is.na(value) || is.null(value) || as.character(value) == "") {
    return("NULL")
  } else {
    # Escape single quotes by doubling them
    val_escaped <- gsub("'", "''", as.character(value))
    return(paste0("'", val_escaped, "'"))
  }
}

parse_flexible_date <- function(date_str) {
  date_str <- trimws(date_str)
  if (nchar(date_str) == 0) return("")
  
  parts <- unlist(strsplit(date_str, "/"))
  today_year <- format(Sys.Date(), "%Y")
  
  if (length(parts) == 2) {
    # Format: d/m → assume current year
    date_str <- paste0(parts[1], "/", parts[2], "/", today_year)
  } else if (length(parts) == 3 && nchar(parts[3]) <= 2) {
    # Format: d/m/yy → convert to yyyy
    current_century <- substr(format(Sys.Date(), "%Y"), 1, 2)
    year_full <- paste0(current_century, sprintf("%02d", as.integer(parts[3])))
    date_str <- paste0(parts[1], "/", parts[2], "/", year_full)
  }
  
  return(format(as.Date(date_str, format = "%d/%m/%Y"), "%d/%m/%Y"))
}

empty_like <- function(df) {df[0, ]}

connect_financial_db <- function() {

  DBI::dbConnect(
  RPostgres::Postgres(),
  host = "aws-1-eu-central-1.pooler.supabase.com",
  port = 5432,
  dbname = "postgres",
  user = "postgres.gcvpukeosfmicildrxsu",
  password = Sys.getenv("SUPABASE_DB_PASSWORD"),
  sslmode = "require"
)
}

get_data <- function(){
conn <- connect_financial_db()
# You can query any table
Income <- dbReadTable(conn, "Income", stringsAsFactors = FALSE) %>% mutate(Date = dmy(Date),
                                                                          Year = year(Date),
                                                                          Month = month(Date))
Budget <- dbReadTable(conn, "Budget", stringsAsFactors = FALSE)
Dropdown <- dbReadTable(conn, "Drop_Down_List", stringsAsFactors = FALSE)
Expenses <- dbReadTable(conn, "Expenses", stringsAsFactors = FALSE) %>% mutate(Date = dmy(Date),
                                                                          Year = year(Date),
                                                                          Month = month(Date))
Bank <- dbReadTable(conn, "Bank", stringsAsFactors = FALSE) %>% mutate(Date = dmy(Date))

dbDisconnect(conn)

names(Expenses) <- gsub("\\.", " ", names(Expenses))
names(Budget) <- gsub("\\.", " ", names(Budget))
names(Dropdown) <- gsub("\\.", " ", names(Dropdown))
names(Bank) <- gsub("\\.", " ", names(Bank))
names(Income) <- gsub("\\.", " ", names(Income))

drop_down_cat <- Dropdown %>% select(`Primary Category`,`Secondary Category`) %>% distinct() %>% filter(`Primary Category` == "Έκτακτα έξοδα" | `Primary Category` == "Πάγια έξοδα" | `Primary Category` == "Προτεραιότητες" | `Primary Category` == "Έξοδα καλοπέρασης")

Years <- data.frame(
  Year = rep(min(Expenses$Year):(max(Expenses$Year) + 1), times = nrow(drop_down_cat))) %>% 
arrange(Year)

expenses_clean <- Expenses %>%
  mutate(Date = as.Date(Date),
        Month = month(Date, label = TRUE, abbr = FALSE, locale = "EN")) %>%
  group_by(Year, Month, `Primary Category`) %>%
  summarize(Amount = sum(Expense, na.rm = TRUE), .groups = "drop")

  income_clean <- Income %>%
  mutate(Date = as.Date(Date),
          Month = month(Date, label = TRUE, abbr = FALSE, locale = "EN")) %>%
  group_by(Year, Month, `Primary Category`) %>%
  summarize(Amount = sum(Income, na.rm = TRUE), .groups = "drop")

  budget_clean <- Budget %>% 
                  pivot_longer(cols=-c(1:3), names_to = "Month", values_to = "Budget") %>% 
                  filter(Budget != "-") %>% mutate(Budget = as.numeric(Budget)) %>% select(-ID)

  bank_clean <- Bank %>% 
                    arrange(Year) %>%
                    mutate(`Actual savings` = `Bank balance` - lag(`Bank balance`),
                            Year = as.numeric(Year),
                            Date = dmy(Date)) %>%
                    rename(Month_num = Month) %>%
                    mutate(Month = month.name[Month_num]) %>%
                    select(-ID)

monthly_summary_per_cat <- rbind(expenses_clean, income_clean)

monthly_summary_per_cat <- monthly_summary_per_cat %>%
    full_join(budget_clean, by = c("Year", "Month", "Primary Category")) %>%         
    mutate(
      Month_num = match(Month, month.name), 
      Date = make_date(Year, Month_num)
    )

monthly_summary_per_cat <- bind_rows(
  monthly_summary_per_cat,
  bank_clean
) %>%
          arrange(Date) %>%
    mutate(Date = as.factor(format(Date, "%Y-%m")))

expenses_clean_bank <- Expenses %>%
  mutate(Date = as.Date(Date),
        Month = month(Date, label = TRUE, abbr = FALSE, locale = "EN")) %>%
  group_by(Year, Month, `Primary Category`, `Secondary Category`) %>%
  summarize(Amount = sum(Expense, na.rm = TRUE), .groups = "drop")

  income_clean_bank <- Income %>%
  mutate(Date = as.Date(Date),
          Month = month(Date, label = TRUE, abbr = FALSE, locale = "EN")) %>%
  group_by(Year, Month, `Primary Category`, `Secondary Category`) %>%
  summarize(Amount = sum(Income, na.rm = TRUE), .groups = "drop")

  savings_outcome <- rbind(expenses_clean_bank, income_clean_bank)

savings_outcome <- savings_outcome %>%
    full_join(budget_clean, by = c("Year", "Month", "Primary Category")) %>%         
    mutate(
      Month_num = match(Month, month.name), 
      Date = make_date(Year, Month_num)
    )

savings_outcome <- bind_rows(
  savings_outcome,
  bank_clean
) %>%
          arrange(Date) %>%
    mutate(Date = as.factor(format(Date, "%Y-%m")))

return(list(Income = Income,
              Expenses = Expenses,
              Budget = Budget,
              Dropdown = Dropdown,
              Bank = Bank,
              Monthly_summary = monthly_summary_per_cat,
              Savings_outcome = savings_outcome
              ))
}

bar_plot <- function(
    data,
    x,
    y,
    fill,
    title,
    key,
    source = "bar_plot"
) {

  # =========================
  # SYMBOLS
  # =========================

  y_sym <- rlang::sym(y)

  if (length(x) > 1) {

    data$..x <- interaction(
      data[, x],
      drop = TRUE
    )

    x_aes <- rlang::sym("..x")

  } else {

    x_aes <- rlang::sym(x)

  }

  fill_aes <- rlang::sym(fill)


  if (length(key) > 1) {

    data$..key <- interaction(
      data[, key],
      drop = TRUE
    )

    key_aes <- rlang::sym("..key")

  } else {

    key_aes <- rlang::sym(key)

  }


  # =========================
  # GGPLOT
  # =========================

  p <- ggplot(
    data,
    aes(
      x = !!x_aes,
      y = !!y_sym,
      fill = !!fill_aes,
      group = !!fill_aes,
      key = !!key_aes
    )
  ) +

    geom_bar(
      stat = "identity",
      position = "stack"
    )


  # =========================
  # FACET
  # =========================

  if ("Primary Category" %in% names(data)) {

    p <- p +
      facet_wrap(
        ~ `Primary Category`,
        scales = "free_y"
      )

  }


  # =========================
  # THEME
  # =========================

  p <- p +

    labs(
      title = title,
      x = NULL,
      y = NULL
    ) +

    theme_minimal(
      base_size = 10
    ) +

    theme(

      plot.title = element_text(
        size = 13,
        face = "bold"
      ),

      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 8
      ),

      axis.text.y = element_text(
        size = 8
      ),

      legend.position = "bottom",

      legend.text = element_text(
        size = 8
      ),

      legend.title = element_text(
        size = 9
      ),

      panel.spacing = unit(
        5,
        "pt"
      ),

      plot.margin = margin(
        5,
        5,
        5,
        5
      )
    )


  # =========================
  # PLOTLY
  # =========================

  p <- ggplotly(
    p,
    source = source,
    tooltip = c(
      "x",
      "y",
      "fill"
    )
  )


  # =========================
  # RESPONSIVE LAYOUT
  # =========================

  p <- p %>%

    layout(

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
        x = 0,
        y = -0.2
      )
    ) %>%

    config(
      responsive = TRUE,
      displayModeBar = FALSE
    ) %>%

    event_register(
      "plotly_click"
    )


  p
}

# Create the plot
scatter_plot <- function(
    data,
    x,
    y,
    z,
    title,
    data_labels = TRUE,
    source = "scatter_plot"
) {

  # =========================
  # SYMBOLS
  # =========================

  x <- sym(deparse(substitute(x)))
  y <- sym(deparse(substitute(y)))
  z <- sym(deparse(substitute(z)))

  y_name <- as_string(y)


  # =========================
  # BASE PLOT
  # =========================

  p <- ggplot(
    data,
    aes(
      x = !!x,
      y = !!y,
      color = !!z
    )
  ) +

    geom_point(
      size = 2
    ) +

    geom_line(
      aes(
        group = !!z
      ),
      linewidth = 0.8,
      linetype = "dotted"
    ) +

    labs(
      title = title,
      x = NULL,
      y = NULL
    ) +

    theme_minimal(
      base_size = 10
    ) +

    theme_plot() +

    theme(

      plot.title = element_text(
        size = 13,
        face = "bold"
      ),

      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 8
      ),

      axis.text.y = element_text(
        size = 8
      ),

      legend.position = "bottom",

      legend.text = element_text(
        size = 8
      ),

      plot.margin = margin(
        5,
        5,
        5,
        5
      )
    )


  # =========================
  # DATA LABELS
  # =========================

  if (data_labels) {

    y_values <- data[[y_name]]

    y_values <- y_values[
      !is.na(y_values) &
      is.finite(y_values)
    ]

    if (length(y_values) > 0) {

      y_range <- max(y_values) - min(y_values)

      if (
        !is.finite(y_range) ||
        y_range == 0
      ) {
        y_range <- max(abs(y_values))

        if (
          !is.finite(y_range) ||
          y_range == 0
        ) {
          y_range <- 1
        }
      }


      p <- p +

        geom_text(
          aes(
            label = round(!!y, 1)
          ),
          nudge_y = 0.02 * y_range,
          size = 2.5,
          show.legend = FALSE
        )

    }

  }


  # =========================
  # PLOTLY
  # =========================

    p <- ggplotly(
      p,
      source = source,
      tooltip = c("x", "y", "colour")
    ) %>%

    layout(

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
        y = -0.2,
        title = list(
          text = ""
        )
      )
    ) %>%

    config(
      responsive = TRUE,
      displayModeBar = FALSE
    )


  p
}

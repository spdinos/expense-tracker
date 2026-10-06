
mod_filters_server <- function(id, base_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    applied_filters <- reactiveVal(NULL)

    filter_map <- list(
      year = "Year",
      month = "Month",
      prim_cat = "Primary Category",
      sec_cat = "Secondary Category"
    )

    normalize_filter <- function(x) {
      if (is.null(x) || length(x) == 0 || all(x == "")) return(NULL)
      x
    }

    observeEvent(base_df(), {
      df <- base_df()
      req(df)

      for (input_id in names(filter_map)) {
        col <- filter_map[[input_id]]
        choices <- sort(unique(df[[col]]))
        updateSelectInput(session, input_id, choices = choices, selected = character(0))
      }
    }, once = TRUE)

    observeEvent(input$apply, {
      df <- base_df()
      req(df)

      current_filters <- lapply(names(filter_map), function(id) normalize_filter(input[[id]]))
      names(current_filters) <- names(filter_map)
      applied_filters(current_filters)

      # Update choices based on filtered data
      for (input_id in names(filter_map)) {
        col <- filter_map[[input_id]]

        filtered_df <- df
        for (other_id in setdiff(names(filter_map), input_id)) {
          other_col <- filter_map[[other_id]]
          val <- current_filters[[other_id]]
          if (!is.null(val) && length(val) > 0) {
            filtered_df <- filtered_df[filtered_df[[other_col]] %in% val, ]
          }
        }

        choices <- sort(unique(filtered_df[[col]]))
        selected <- intersect(current_filters[[input_id]], choices)
        updateSelectInput(session, input_id, choices = choices, selected = selected)
      }
    })

    observeEvent(input$reset, {
      df <- base_df()
      req(df)

      for (input_id in names(filter_map)) {
        col <- filter_map[[input_id]]
        choices <- sort(unique(df[[col]]))
        updateSelectInput(session, input_id, choices = choices, selected = character(0))
      }

      applied_filters(NULL)
    })

    return(list(
      applied_filters = applied_filters,
      filter_map = filter_map
    ))
  })
}
# 1. LOAD LIBRARIES
library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(DT)
library(plotly)
library(randomForest)
library(shinyjs)

setwd("C:\\Users\\AKBAR RAZAN\\OneDrive\\Documents\\coolyeah\\DATMIN\\FP")
# 2. DATA LOADING & CLEANING
data_raw <- read.csv("mushrooms.csv", stringsAsFactors = FALSE)

# Clean internal names
colnames(data_raw) <- make.names(gsub("-", ".", colnames(data_raw)))
colnames(data_raw) <- gsub("\\.", "_", colnames(data_raw))

# Define recoding maps
recode_maps <- list(
  class = c("e"="Edible", "p"="Poisonous"),
  cap_shape = c("b"="Bell", "c"="Conical", "x"="Convex", "f"="Flat", "k"="Knobbed", "s"="Sunken"),
  cap_surface = c("f"="Fibrous", "g"="Grooves", "y"="Scaly", "s"="Smooth"),
  cap_color = c("n"="Brown", "b"="Buff", "c"="Cinnamon", "g"="Gray", "r"="Green", "p"="Pink", "u"="Purple", "e"="Red", "w"="White", "y"="Yellow"),
  bruises = c("t"="Bruises", "f"="No"),
  odor = c("a"="Almond", "l"="Anise", "c"="Creosote", "y"="Fishy", "f"="Foul", "m"="Musty", "n"="None", "p"="Pungent", "s"="Spicy"),
  gill_attachment = c("a"="Attached", "d"="Descending", "f"="Free", "n"="Notched"),
  gill_spacing = c("c"="Close", "w"="Crowded", "d"="Distant"),
  gill_size = c("b"="Broad", "n"="Narrow"),
  gill_color = c("k"="Black", "n"="Brown", "b"="Buff", "h"="Chocolate", "g"="Gray", "r"="Green", "o"="Orange", "p"="Pink", "u"="Purple", "e"="Red", "w"="White", "y"="Yellow"),
  stalk_shape = c("e"="Enlarging", "t"="Tapering"),
  stalk_root = c("b"="Bulbous", "c"="Club", "u"="Cup", "e"="Equal", "z"="Rhizomorphs", "r"="Rooted", "?"="Missing"),
  stalk_surface_above_ring = c("f"="Fibrous", "y"="Scaly", "k"="Silky", "s"="Smooth"),
  stalk_surface_below_ring = c("f"="Fibrous", "y"="Scaly", "k"="Silky", "s"="Smooth"),
  stalk_color_above_ring = c("n"="Brown", "b"="Buff", "c"="Cinnamon", "g"="Gray", "o"="Orange", "p"="Pink", "e"="Red", "w"="White", "y"="Yellow"),
  stalk_color_below_ring = c("n"="Brown", "b"="Buff", "c"="Cinnamon", "g"="Gray", "o"="Orange", "p"="Pink", "e"="Red", "w"="White", "y"="Yellow"),
  veil_type = c("p"="Partial", "u"="Universal"),
  veil_color = c("n"="Brown", "o"="Orange", "w"="White", "y"="Yellow"),
  ring_number = c("n"="None", "o"="One", "t"="Two"),
  ring_type = c("c"="Cobwebby", "e"="Evanescent", "f"="Flaring", "l"="Large", "n"="None", "p"="Pendant", "s"="Sheathing", "z"="Zone"),
  spore_print_color = c("k"="Black", "n"="Brown", "b"="Buff", "h"="Chocolate", "r"="Green", "o"="Orange", "u"="Purple", "w"="White", "y"="Yellow"),
  population = c("a"="Abundant", "c"="Clustered", "n"="Numerous", "s"="Scattered", "v"="Several", "y"="Solitary"),
  habitat = c("g"="Grasses", "l"="Leaves", "m"="Meadows", "p"="Paths", "u"="Urban", "w"="Waste", "d"="Woods")
)

# Function to recode values
do_recode <- function(vec, map) {
  vec <- as.character(vec)
  valid_map <- map[names(map) %in% unique(vec)]
  if(length(valid_map) > 0) vec <- recode(vec, !!!valid_map)
  return(vec)
}

# Apply recoding to all columns using a loop
data_clean <- data_raw
for (col_name in names(recode_maps)) {
  data_clean[[col_name]] <- do_recode(data_clean[[col_name]], recode_maps[[col_name]])
}
data_clean <- data_clean %>% mutate(across(everything(), as.factor))

# Create display names for UI
internal_names <- colnames(data_clean)
display_names <- tools::toTitleCase(gsub("_", " ", internal_names))
names(display_names) <- internal_names

# Train model
top_10_features <- c("odor", "spore_print_color", "gill_color", "gill_size", "stalk_root", "habitat", "population", "stalk_color_below_ring", "cap_color", "stalk_surface_above_ring")
data_train <- data_clean %>% select(class, all_of(top_10_features))

set.seed(123)
rf_model <- randomForest(class ~ ., data = data_train, ntree = 500, mtry = 3, importance = TRUE, na.action = na.roughfix)
features_chart_opts <- setdiff(internal_names, "class")

# Default values for prediction inputs
default_safe_values <- list(
  odor = "Almond", spore_print_color = "Black", gill_color = "White",
  gill_size = "Broad", stalk_root = "Club", habitat = "Meadows",
  population = "Scattered", stalk_color_below_ring = "White",
  cap_color = "Yellow", stalk_surface_above_ring = "Smooth"
)

preset_poison_values <- list(
  odor = "Foul", spore_print_color = "Chocolate", gill_color = "Buff",
  gill_size = "Narrow", stalk_root = "Bulbous", habitat = "Woods",
  population = "Several", stalk_color_below_ring = "White",
  cap_color = "Brown", stalk_surface_above_ring = "Silky"
)

# 3. UI
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "zephyr"),
  useShinyjs(),
  
  # Header
  div(class="p-3 mb-3 border-bottom d-flex align-items-center justify-content-between",
      h3(tagList(bs_icon("virus"), "Mushroom Analytics"), class="text-primary fw-bold m-0"),
      span("Edibility Prediction & Data Exploration", class="text-muted")
  ),
  
  navset_pill_list(
    widths = c(2, 10),
    
    # Tab: Home
    nav_panel("Home", icon = bs_icon("house-door-fill"),
              div(class = "container-fluid",
                  layout_columns(
                    col_widths = c(9, 3), 
                    card(
                      height = "250px", card_header("Dataset Overview", class = "bg-primary text-white"), 
                      card_body(markdown("**Mushroom Classification**\n\nThis dashboard analyzes mushroom characteristics to determine edibility using data from the UCI Machine Learning Repository."))
                    ),
                    card(
                      height = "250px", card_header("Source", class = "bg-primary text-white"), 
                      card_body(markdown("**UCI Machine Learning Repository**\n\n[Mushroom Data Set](https://archive.ics.uci.edu/dataset/73/mushroom)\n\nDonor: Jeff Schlimmer (1987)"))
                    )
                  ),
                  br(),
                  card(
                    card_header("Variable Explanation Dictionary", class = "bg-primary text-white"),
                    height = "500px", 
                    card_body(
                      style = "overflow-y: scroll;",
                      markdown("
            * **Class:** Indicates whether a mushroom is edible or poisonous.
            * **Cap Shape:** Describes the overall shape of the mushroom cap.
            * **Cap Surface:** Shows the texture found on the surface of the cap.
            * **Cap Color:** Indicates the visible color of the mushroom cap.
            * **Bruises:** States whether the mushroom shows bruising when handled.
            * **Odor:** Describes the smell produced by the mushroom.
            * **Gill Attachment:** Explains how the gills connect to the stalk.
            * **Gill Spacing:** Describes how close or far apart the gills are.
            * **Gill Size:** Indicates whether the gills are broad or narrow.
            * **Gill Color:** Shows the color of the gills beneath the cap.
            * **Stalk Shape:** Describes whether the stalk widens or narrows toward the base.
            * **Stalk Root:** Describes the type of root structure at the base of the stalk.
            * **Stalk Surface:** Indicates the texture on the stalk’s upper and lower surfaces.
            * **Stalk Color:** Shows the color of the stalk above and below the ring.
            * **Veil Type:** Indicates the type of protective veil covering young mushrooms.
            * **Veil Color:** Shows the color of the veil before it breaks.
            * **Ring Number:** States how many rings remain on the stalk.
            * **Ring Type:** Describes the shape or structure of the ring on the stalk.
            * **Spore Print Color:** Indicates the color of spores left when the cap is placed on a surface.
            * **Population:** Describes how many mushrooms grow together in an area.
            * **Habitat:** Indicates the type of environment where the mushroom is found.
            ")
                    )
                  )
              )
    ),
    
    # Tab: Summary Statistics
    nav_panel("Summary Statistics", icon = bs_icon("table"),
              card(
                full_screen = TRUE,
                card_header("Dataset Summary Statistics", class = "bg-primary text-white"),
                DTOutput("tbl_summary", height = "600px")
              )
    ),
    
    # Tab: Visual Charts
    nav_panel("Visual Charts", icon = bs_icon("bar-chart-fill"),
              layout_columns(
                col_widths = c(6, 6),
                card(full_screen = TRUE, card_header("Target Class Proportion", class = "bg-primary text-white"), plotlyOutput("plot_pie_class", height = "400px")),
                card(full_screen = TRUE, card_header("Stacked Bar Chart by Classes", class = "bg-primary text-white"),
                     card_body(
                       selectInput("var_security", "Variable:", choices = setNames(features_chart_opts, display_names[features_chart_opts]), selected = "odor", width = "100%"),
                       plotlyOutput("plot_security", height = "350px")
                     )
                )
              ),
              layout_columns(
                col_widths = c(12), 
                card(full_screen = TRUE, card_header("Heatmap Correlation", class = "bg-primary text-white"), plotlyOutput("plot_heatmap", height = "800px"))
              )
    ),
    
    # Tab: Prediction
    nav_panel("Prediction", icon = bs_icon("cpu-fill"),
              card(
                card_header("Predict Safety (Uses Top 10 Variables)", class = "bg-primary text-white"),
                layout_columns(
                  col_widths = c(8, 4),
                  div(
                    actionButton("btn_predict", "Run Prediction", class = "btn-primary btn-lg w-100 mb-3", icon = icon("wand-magic-sparkles")),
                    div(class="d-flex gap-2",
                        actionButton("btn_preset_edible", "Example: Edible", class = "btn-outline-success btn-sm w-50", icon = icon("leaf")),
                        actionButton("btn_preset_poison", "Example: Poisonous", class = "btn-outline-danger btn-sm w-50", icon = icon("skull"))
                    )
                  ),
                  uiOutput("prediction_result_box")
                ),
                hr(),
                div(class="alert alert-info", role="alert",
                    h5(bs_icon("info-circle-fill"), "Understanding Confidence Score"),
                    p("A high score (>90%) means the model is very certain. A lower score indicates ambiguity. Be cautious with low-confidence predictions.")
                ),
                h5("Mushroom Attributes Input", class="mb-3 text-primary"),
                uiOutput("dynamic_inputs_ui")
              )
    ),
    
    # Tab: Database
    nav_panel("Database", icon = bs_icon("table"),
              card(
                full_screen = TRUE, height = "800px", 
                card_header("Full Mushroom Database", class = "bg-primary text-white"),
                layout_columns(
                  col_widths = c(3, 9), 
                  div(style = "padding-right: 15px; border-right: 1px solid #dee2e6; display: flex; flex-direction: column; height: 650px;",
                      h5("Filters", class="text-primary fw-bold mb-3", style = "flex-shrink: 0;"),
                      div(style = "overflow-y: auto; flex-grow: 1;",
                          uiOutput("database_filters_ui")
                      )
                  ),
                  DTOutput("tbl_database")
                )
              )
    ),
    
    # Tab: Authors
    nav_panel("Authors", icon = bs_icon("people-fill"),
              div(class = "container-fluid mt-3",
                  h2("Meet the Team", class="text-center mb-5 fw-bold text-primary"),
                  layout_columns(
                    col_widths = c(4, 4, 4),
                    card(class = "text-center shadow-sm h-100 author-card", card_header("Akbar Razan", class = "bg-primary text-white"), 
                         card_body(div(class = "d-flex justify-content-center", img(src = "author1.jpg", class = "rounded-circle mb-3 border", width = "150px", height = "150px", style = "object-fit: cover;")), h5("NRP: 5003221150", class="text-muted"))),
                    card(class = "text-center shadow-sm h-100 author-card", card_header("Fadhil Rahman Saputra", class = "bg-primary text-white"), 
                         card_body(div(class = "d-flex justify-content-center", img(src = "author2.jpeg", class = "rounded-circle mb-3 border", width = "150px", height = "150px", style = "object-fit: cover;")), h5("NRP: 5003231174", class="text-muted"))),
                    card(class = "text-center shadow-sm h-100 author-card", card_header("Khairunnisa Cahya Nagari", class = "bg-primary text-white"), 
                         card_body(div(class = "d-flex justify-content-center", img(src = "author3.jpg", class = "rounded-circle mb-3 border", width = "150px", height = "150px", style = "object-fit: cover;")), h5("NRP: 5003231186", class="text-muted")))
                  )
              )
    )
  )
)

# 4. SERVER
server <- function(input, output, session) {
  get_cramer_v <- function(x, y) {
    if(nlevels(as.factor(x)) < 2 || nlevels(as.factor(y)) < 2) return(0)
    tbl <- table(x, y)
    chi2 <- tryCatch(suppressWarnings(chisq.test(tbl, correct=FALSE)$statistic), error=function(e) 0)
    denom <- sum(tbl) * (min(dim(tbl)) - 1)
    if(denom <= 0) return(0)
    as.numeric(sqrt(chi2 / denom))
  }
  
  # --- FUNGSI UNTUK TABEL SUMMARY MENGGUNAKAN DT ---
  output$tbl_summary <- renderDT({
    all_vars <- colnames(data_clean)
    all_display_names <- display_names[all_vars]
    
    summary_df <- data.frame(Variable = all_display_names, stringsAsFactors = FALSE)
    
    summary_df$Count <- sapply(all_vars, function(col) nrow(data_clean))
    summary_df$Unique <- sapply(all_vars, function(col) n_distinct(data_clean[[col]]))
    summary_df$Top <- sapply(all_vars, function(col) {
      as.character(names(sort(table(data_clean[[col]]), decreasing = TRUE))[1])
    })
    summary_df$Freq <- sapply(all_vars, function(col) {
      as.numeric(sort(table(data_clean[[col]]), decreasing = TRUE)[1])
    })
    
    datatable(summary_df,
              options = list(scrollY = "550px", scrollX = TRUE, pageLength = 25),
              class = 'cell-border stripe compact',
              rownames = FALSE,
              selection = 'none') %>%
      formatStyle('Variable', fontWeight = 'bold')
  })
  
  # Render prediction inputs
  output$dynamic_inputs_ui <- renderUI({
    inputs_list <- lapply(top_10_features, function(col_name) {
      def_val <- default_safe_values[[col_name]] %||% levels(data_clean[[col_name]])[1]
      
      selectInput(inputId = paste0("in_", col_name), label = display_names[col_name], 
                  choices = levels(data_clean[[col_name]]), selected = def_val, width = "100%")
    })
    do.call(layout_columns, c(inputs_list, list(col_widths = 3))) 
  })
  
  # Preset logic
  observeEvent(input$btn_preset_edible, {
    lapply(names(default_safe_values), function(f) updateSelectInput(session, paste0("in_", f), selected = default_safe_values[[f]]))
    delay(500, click("btn_predict"))
  })
  
  observeEvent(input$btn_preset_poison, {
    lapply(names(preset_poison_values), function(f) updateSelectInput(session, paste0("in_", f), selected = preset_poison_values[[f]]))
    delay(500, click("btn_predict"))
  })
  
  # Prediction logic
  pred_val <- reactiveVal(NULL)
  observe({ if(is.null(pred_val())) click("btn_predict") })
  
  observeEvent(input$btn_predict, {
    input_list <- lapply(top_10_features, function(col) input[[paste0("in_", col)]] %||% levels(data_clean[[col]])[1])
    names(input_list) <- top_10_features
    
    new_data <- as.data.frame(input_list, stringsAsFactors = FALSE)
    for(col in top_10_features) new_data[[col]] <- factor(new_data[[col]], levels = levels(data_clean[[col]]))
    
    prob <- predict(rf_model, new_data, type = "prob")
    pred_class <- as.character(predict(rf_model, new_data))
    max_prob <- round(max(prob) * 100, 1)
    
    confidence_threshold <- 75
    
    if (max_prob < confidence_threshold) {
      pred_val(list(class = "Cannot Predict", prob = max_prob))
    } else {
      pred_val(list(class = pred_class, prob = max_prob))
    }
  })
  
  # Render prediction result
  output$prediction_result_box <- renderUI({
    res <- pred_val()
    req(res) 
    
    ui_props <- list(
      "Edible"        = list(theme = "success", icon = "check-circle-fill", title = "SAFE TO EAT", color = "#198754"),
      "Poisonous"     = list(theme = "danger",  icon = "exclamation-triangle-fill", title = "POISONOUS", color = "#dc3545"),
      "Cannot Predict" = list(theme = "warning", icon = "x-circle-fill", title = "PREDICTION FAILED", color = "#ffc107")
    )
    props <- ui_props[[res$class]]
    
    warning_msg <- if(res$class == "Cannot Predict") {
      div(class = "alert alert-warning mt-3 d-flex align-items-center", role = "alert",
          bs_icon("exclamation-triangle-fill", class = "me-2 flex-shrink-0"),
          div(h5("Prediction Failed", class="alert-heading mb-1"),
              p("Warning: The selected feature combination is not recognized. Please check your input values.", class="mb-0"))
      )
    } else if (res$prob <= 90) {
      div(class = "alert alert-warning mt-3 d-flex align-items-center", role = "alert",
          bs_icon("exclamation-triangle-fill", class = "me-2 flex-shrink-0"),
          div(h5("Low Confidence Warning!", class="alert-heading mb-1"),
              p("The model is not highly confident. The selected features are ambiguous. Please be extremely cautious.", class="mb-0"))
      )
    } else {
      NULL
    }
    
    div(class = "p-3 border rounded-3",
        style = paste0("border-left: 5px solid ", props$color, "; background-color: #f8f9fa;"),
        h4(class = paste0("text-", props$theme), tagList(bs_icon(props$icon), " ", props$title)),
        h5(paste0("Confidence: ", res$prob, "%"), class="text-muted mb-3"),
        warning_msg
    )
  })

  color_edible <- "#198754" 
  color_poisonous <- "#dc3545" 
  class_colors <- c("Edible" = color_edible, "Poisonous" = color_poisonous)
  
  # Render charts
  output$plot_pie_class <- renderPlotly({
    df_pie <- count(data_clean, class)
    plot_ly(df_pie, labels = ~class, values = ~n, type = 'pie', textposition = 'inside', textinfo = 'label+percent',
            marker = list(colors = class_colors, line = list(color = '#FFFFFF', width = 2))) %>%
      layout(showlegend = TRUE)
  })
  
  output$plot_security <- renderPlotly({
    req(input$var_security)
    plot_data <- data_clean %>% 
      count(val = .data[[input$var_security]], class)
    
    plot_ly(
      data = plot_data,
      x = ~val, 
      y = ~n, 
      color = ~class, 
      colors = class_colors,
      type = 'bar',
      hoverinfo = 'text',
      text = ~paste(class, ":", n) # Teks yang muncul saat hover
    ) %>%
      layout(
        barmode = 'stack',
        yaxis = list(title = "Count"),
        xaxis = list(title = display_names[input$var_security])
      )
  })
  
  output$plot_heatmap <- renderPlotly({
    vars <- names(data_clean)
    cramer_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(display_names[vars], display_names[vars]))
    
    for (i in 1:length(vars)) {
      for (j in 1:length(vars)) {
        cramer_mat[i, j] <- get_cramer_v(data_clean[[vars[i]]], data_clean[[vars[j]]])
      }
    }
    
    cramer_mat[is.na(cramer_mat)] <- 0
    cramer_mat[is.infinite(cramer_mat)] <- 0
    
    plot_ly(x = display_names[vars], y = display_names[vars], z = cramer_mat, 
            type = "heatmap", 
            colorscale = list(
              c(0, "#2E8B57"),    
              c(0.5, "#FFFFFF"),       
              c(1, "#CD5C5C")         
            )) %>%
      layout(
        margin = list(b = 150, l = 150), 
        xaxis = list(tickangle = -45, title = ""), 
        yaxis = list(title = "")
      )
  })
  
  # Render database filters
  output$database_filters_ui <- renderUI({
    filter_inputs <- lapply(names(data_clean), function(var) {
      div(style = "margin-bottom: 15px;",
          selectInput(paste0("filter_", var), display_names[var], 
                      choices = c("All", levels(data_clean[[var]])), selected = "All", width = "100%")
      )
    })
    
    reset_button_div <- div(style = "margin-top: 20px;",
                            actionButton("reset_filters", "Reset All Filters", class = "btn-secondary w-100")
    )
    
    do.call(tagList, c(filter_inputs, list(reset_button_div)))
  })
  
  # Filter data logic
  filtered_data <- reactive({
    df <- data_clean
    for (var in names(data_clean)) {
      filter_value <- input[[paste0("filter_", var)]]
      if (!is.null(filter_value) && filter_value != "All") {
        df <- df %>% filter(.data[[var]] == filter_value)
      }
    }
    df
  })
  
  # Reset filter logic
  observeEvent(input$reset_filters, { 
    lapply(names(data_clean), function(var) {
      updateSelectInput(session, paste0("filter_", var), selected = "All")
    })
  })
  
  # Render database table
  output$tbl_database <- renderDT({
    df_show <- filtered_data()
    colnames(df_show) <- display_names[names(df_show)]
    
    datatable(df_show, options = list(scrollX = TRUE, scrollY = "600px", pageLength = 25), 
              style = "bootstrap4", class = "table table-striped table-hover")
  })
}

shinyApp(ui, server)

# ================================
# Interactive Sockeye Map (Shiny)
# ================================
# Features:
#  - Basemap switcher (Shiny)
#  - Points colored by field (repunit or gradient)
#  - Optional binary classification ("Pass"/"Fail") at threshold (default 80)
#  - Legend toggle (on/off)
#  - Boundaries overlay from shapefile (outline or thematic fill by CU_Name)
#  - Hover labels (points + boundaries)
#  - Robust error handling + safe column checks
#  - Shapefile SHX restore + terra fallback
#  - Download buttons: HTML map + GeoJSON points
# --------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(tmap)
  library(leaflet)
  library(leaflet.esri)
  library(readr)
  library(terra)       # fallback shapefile reading
  library(shiny)
  library(htmlwidgets) # for HTML save fallback
})

# Allow GDAL to restore/create missing .shx index files
Sys.setenv("SHAPE_RESTORE_SHX" = "YES")

# ---- Error Handling Helpers ----
validate_required_columns <- function(df, required_cols, df_name = "data frame") {
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(sprintf("Missing columns in %s: %s", df_name, paste(missing, collapse = ", ")))
  }
  invisible(df)
}

ensure_non_empty_names <- function(df, df_name = "data frame", fix = FALSE) {
  empties <- which(names(df) == "" | is.na(names(df)))
  if (length(empties) > 0) {
    msg <- sprintf("Found empty column name(s) in %s at position(s): %s",
                   df_name, paste(empties, collapse = ", "))
    if (fix) {
      new_names <- names(df)
      new_names[empties] <- paste0("X_", empties)
      names(df) <- new_names
      warning(msg, "\nRenamed to: ", paste(new_names[empties], collapse = ", "))
    } else {
      stop(msg, "\nPlease fix the file headers.")
    }
  }
  df
}

# ---- Data Loaders (tables) ----
load_stock_codes <- function(stock_codes_fn) {
  if (!file.exists(stock_codes_fn)) stop("Stock codes file not found: ", stock_codes_fn)
  df <- read.delim(stock_codes_fn, stringsAsFactors = FALSE, check.names = FALSE)
  df <- ensure_non_empty_names(df, "stock codes", fix = FALSE)
  validate_required_columns(df, c("collection", "XLONG", "YLAT"), "stock codes")
  df
}

load_repunits <- function(repunits_fn) {
  if (!file.exists(repunits_fn)) stop("Repunits file not found: ", repunits_fn)
  df <- read.delim(repunits_fn, stringsAsFactors = FALSE, check.names = FALSE)
  df <- ensure_non_empty_names(df, "repunits", fix = FALSE)
  validate_required_columns(df, c("repunit"), "repunits")
  df
}

load_baseline <- function(baseline_fn) {
  if (!file.exists(baseline_fn)) stop("Baseline file not found: ", baseline_fn)
  df <- read_tsv(baseline_fn, show_col_types = FALSE)
  df <- ensure_non_empty_names(df, "baseline", fix = FALSE)
  validate_required_columns(df, c("collection"), "baseline")
  df
}

load_gradient <- function(gradient_fn) {
  if (!file.exists(gradient_fn)) stop("Gradient file not found: ", gradient_fn)
  df <- read.delim(gradient_fn, stringsAsFactors = FALSE, check.names = FALSE)
  df <- ensure_non_empty_names(df, "gradient", fix = FALSE)
  validate_required_columns(df, c("collection"), "gradient")
  df
}

# ---- Shapefile Loader (boundaries) ----
check_shapefile_components <- function(shp_path) {
  if (!file.exists(shp_path)) stop("Shapefile (.shp) not found: ", shp_path)
  base <- sub("\\.shp$", "", shp_path, ignore.case = TRUE)
  companion <- c(".dbf", ".prj")
  advisory  <- c(".shx", ".cpg", ".sbn", ".sbx")
  
  missing_core <- companion[!file.exists(paste0(base, companion))]
  if (length(missing_core) > 0) {
    stop("Missing shapefile companion(s): ", paste(missing_core, collapse = ", "),
         "\nEnsure the .dbf and .prj are present next to the .shp.")
  }
  
  missing_advisory <- advisory[!file.exists(paste0(base, advisory))]
  if (length(missing_advisory) > 0) {
    warning("Optional shapefile companion(s) not found: ",
            paste(missing_advisory, collapse = ", "),
            "\nThe layer should still load, but indices/encoding may be suboptimal.")
  }
  invisible(TRUE)
}

load_boundaries_shp <- function(shp_path,
                                target_crs = 4326,
                                shift_longitude = TRUE) {
  Sys.setenv("SHAPE_RESTORE_SHX" = "YES")
  check_shapefile_components(shp_path)
  
  boundaries <- tryCatch(
    { sf::st_read(shp_path, quiet = TRUE) },
    error = function(e) {
      message("sf::st_read failed (", conditionMessage(e), "). Trying terra::vect fallback...")
      v <- terra::vect(shp_path)
      sf::st_as_sf(v)
    }
  )
  
  boundaries <- ensure_non_empty_names(boundaries, "boundaries shapefile", fix = TRUE)
  
  crs_obj <- sf::st_crs(boundaries)
  if (is.na(crs_obj)) stop("Boundaries shapefile has no CRS (.prj missing).")
  if (!identical(crs_obj$epsg, target_crs) && crs_obj$input != paste0("EPSG:", target_crs)) {
    boundaries <- sf::st_transform(boundaries, target_crs)
  }
  if (isTRUE(shift_longitude)) boundaries <- sf::st_shift_longitude(boundaries)
  boundaries
}

# ---- Data Wrangling ----
merge_repunits <- function(stock_codes_df, repunits_df = NULL, filter_by_repunits = FALSE) {
  if (!is.null(repunits_df)) {
    joined <- base::merge(repunits_df, stock_codes_df, by = "repunit", all.y = TRUE)
    if (filter_by_repunits) joined <- dplyr::filter(joined, repunit %in% repunits_df$repunit)
  } else {
    joined <- stock_codes_df
  }
  joined
}

filter_to_baseline <- function(joined_df, baseline_df = NULL) {
  if (!is.null(baseline_df)) joined_df <- dplyr::filter(joined_df, collection %in% baseline_df$collection)
  joined_df
}

attach_gradient <- function(joined_df, gradient_df = NULL, by = "collection") {
  if (!is.null(gradient_df)) joined_df <- base::merge(joined_df, gradient_df, by = by, all.x = TRUE)
  joined_df
}

prepare_sf_points <- function(joined_df, x_col = "XLONG", y_col = "YLAT",
                              crs = 4326, shift_longitude = TRUE) {
  req <- c(x_col, y_col, "collection")
  missing <- setdiff(req, names(joined_df))
  if (length(missing) > 0) stop("Missing required columns in joined_df: ", paste(missing, collapse = ", "))
  
  joined_df <- joined_df %>% tidyr::drop_na(dplyr::all_of(c(x_col, y_col)))
  
  points <- sf::st_as_sf(joined_df, coords = c(x_col, y_col), crs = crs) %>%
    sf::st_cast("POINT")
  if (isTRUE(shift_longitude)) points <- sf::st_shift_longitude(points)
  points
}

# ---- Helpers ----
resolve_color_by <- function(points_sf, requested, fallback = "repunit") {
  nms <- names(points_sf)
  if (!is.null(requested) && requested %in% nms && requested != "" && !is.na(requested)) return(requested)
  if (fallback %in% nms) return(fallback)
  choices <- setdiff(nms, c("geometry", "XLONG", "YLAT"))
  if (length(choices) == 0) stop("No valid columns found in points_sf to color by.")
  choices[1]
}

make_binary_factor <- function(points_sf, field, threshold = 80,
                               pass_label = "Pass", fail_label = "Fail",
                               operator = c(">=", ">")) {
  operator <- match.arg(operator)
  if (!field %in% names(points_sf)) stop("Field '", field, "' not found for binary classification.")
  vals <- points_sf[[field]]
  if (!is.numeric(vals)) stop("Binary classification requires a numeric field. '", field, "' is not numeric.")
  pass <- if (operator == ">=") vals >= threshold else vals > threshold
  factor(ifelse(pass, pass_label, fail_label), levels = c(fail_label, pass_label))
}

# ---- Mapping ----
set_tmap_mode <- function(mode = c("view", "plot")) {
  tmap_mode(match.arg(mode))
}

build_tmap <- function(points_sf,
                       color_by = "repunit",
                       group_by = "repunit",
                       palette = "viridis",
                       size = 1,
                       basemap = "Esri.WorldTopoMap",
                       hover_label_points = TRUE,
                       show_legend = TRUE,
                       # binary classification options
                       binary_on = FALSE,
                       binary_threshold = 80,
                       binary_operator = c(">=", ">"),
                       binary_pass_label = "Pass",
                       binary_fail_label = "Fail",
                       binary_colors = c(Fail = "#d73027", Pass = "#1a9850"),
                       # boundaries options (with defaults)
                       boundaries_sf = NULL,
                       boundaries_group_name = "CU Boundaries",
                       boundaries_color_by = "CU_Name",   # preconfigured field
                       boundaries_palette = "Set3",        # discrete palette
                       boundaries_alpha = 0.3,
                       boundaries_border_col = "#444",
                       boundaries_border_lwd = 1,
                       hover_label_boundaries = TRUE) {
  
  color_by <- resolve_color_by(points_sf, color_by, fallback = "repunit")
  
  points_to_plot <- points_sf
  dot_col <- color_by
  dot_style <- "cat"
  
  if (isTRUE(binary_on)) {
    points_to_plot[[".__binary__"]] <- make_binary_factor(
      points_sf = points_to_plot,
      field     = color_by,
      threshold = binary_threshold,
      pass_label = binary_pass_label,
      fail_label = binary_fail_label,
      operator  = match.arg(binary_operator)
    )
    dot_col <- ".__binary__"
    dot_style <- "cat"
  } else {
    col_vals <- points_to_plot[[color_by]]
    dot_style <- if (is.numeric(col_vals)) "cont" else "cat"
  }
  
  map <- tm_shape(points_to_plot) +
    tm_dots(group = group_by,
            col   = dot_col,
            palette = if (isTRUE(binary_on)) binary_colors else palette,
            style   = dot_style,
            popup.vars = hover_label_points,
            size  = size,
            legend.show = show_legend) +
    tm_basemap(server = basemap)
  
  if (!is.null(boundaries_sf)) {
    use_fill <- (!is.null(boundaries_color_by) &&
                   !identical(boundaries_color_by, "None") &&
                   !identical(boundaries_color_by, "") &&
                   !is.na(boundaries_color_by))
    if (use_fill) {
      if (!boundaries_color_by %in% names(boundaries_sf)) {
        warning("Field '", boundaries_color_by, "' not found in boundaries_sf; drawing outlines only.")
        use_fill <- FALSE
      }
    }
    
    if (use_fill) {
      map <- map +
        tm_shape(boundaries_sf, name = boundaries_group_name) +
        tm_polygons(group = boundaries_group_name,
                    col = boundaries_color_by,
                    palette = boundaries_palette,   # discrete palette
                    style = "cat",
                    alpha = boundaries_alpha,
                    border.col = boundaries_border_col,
                    border.lwd = boundaries_border_lwd,
                    popup.vars = hover_label_boundaries,
                    legend.show = show_legend)
    } else {
      map <- map +
        tm_shape(boundaries_sf, name = boundaries_group_name) +
        tm_polygons(group = boundaries_group_name,
                    col = NA, alpha = 0,
                    border.col = boundaries_border_col,
                    border.lwd = boundaries_border_lwd,
                    popup.vars = hover_label_boundaries,
                    legend.show = show_legend)
    }
  }
  
  if (!show_legend) {
    map <- map + tm_layout(legend.show = FALSE)
  }
  
  map
}

# ---- Non‑Shiny orchestrator (returns Leaflet map) ----
interactive_map2 <- function(stock_codes_fn,
                             repunits_fn = NULL,
                             plot_by = "repunit",
                             filter_by_baseline = TRUE,
                             baseline_fn = NULL,
                             filter_by_repunits = FALSE,
                             gradient_fn = NULL,
                             basemap = "Esri.WorldTopoMap",
                             palette = "viridis",
                             hover_label_points = TRUE,
                             show_legend = TRUE,
                             # binary options
                             binary_on = FALSE,
                             binary_threshold = 80,
                             binary_operator = c(">=", ">"),
                             binary_pass_label = "Pass",
                             binary_fail_label = "Fail",
                             binary_colors = c(Fail = "#d73027", Pass = "#1a9850"),
                             # boundaries parameters
                             boundaries_shp = NULL,
                             boundaries_group_name = "CU Boundaries",
                             boundaries_color_by = "CU_Name",
                             boundaries_palette = "Set3",
                             boundaries_alpha = 0.3,
                             boundaries_border_col = "#444",
                             boundaries_border_lwd = 1,
                             hover_label_boundaries = TRUE,
                             tmap_view = TRUE) {
  
  if (isTRUE(tmap_view)) set_tmap_mode("view") else set_tmap_mode("plot")
  
  stock_codes <- load_stock_codes(stock_codes_fn)
  repunits    <- if (!is.null(repunits_fn)) load_repunits(repunits_fn) else NULL
  joined      <- merge_repunits(stock_codes, repunits, filter_by_repunits)
  
  baseline    <- if (isTRUE(filter_by_baseline)) {
    if (is.null(baseline_fn)) stop("baseline_fn is required when filter_by_baseline = TRUE")
    load_baseline(baseline_fn)
  } else NULL
  
  joined <- filter_to_baseline(joined, baseline)
  gradient <- if (!is.null(gradient_fn)) load_gradient(gradient_fn) else NULL
  joined   <- attach_gradient(joined, gradient, by = "collection")
  
  joined <- joined %>% dplyr::select(collection, dplyr::everything())
  points <- prepare_sf_points(joined, x_col = "XLONG", y_col = "YLAT", shift_longitude = TRUE)
  
  boundaries_sf <- if (!is.null(boundaries_shp)) {
    load_boundaries_shp(boundaries_shp, target_crs = 4326, shift_longitude = TRUE)
  } else NULL
  
  map <- build_tmap(points_sf = points,
                    color_by = plot_by,
                    group_by = plot_by,
                    palette = palette,
                    basemap = basemap,
                    hover_label_points = hover_label_points,
                    show_legend = show_legend,
                    binary_on = binary_on,
                    binary_threshold = binary_threshold,
                    binary_operator = binary_operator,
                    binary_pass_label = binary_pass_label,
                    binary_fail_label = binary_fail_label,
                    binary_colors = binary_colors,
                    boundaries_sf = boundaries_sf,
                    boundaries_group_name = boundaries_group_name,
                    boundaries_color_by = boundaries_color_by,
                    boundaries_palette = boundaries_palette,
                    boundaries_alpha = boundaries_alpha,
                    boundaries_border_col = boundaries_border_col,
                    boundaries_border_lwd = boundaries_border_lwd,
                    hover_label_boundaries = hover_label_boundaries)
  
  map_leaflet <- tmap_leaflet(map)
  base_groups <- c("Esri.WorldTopoMap", "Esri.WorldImagery", "Esri.OceanBasemap", "OpenStreetMap", "CartoDB.Positron")
  overlay_groups <- c(plot_by, if (!is.null(boundaries_sf)) boundaries_group_name else NULL)
  
  map_leaflet <- map_leaflet %>%
    leaflet::addLayersControl(
      baseGroups = base_groups,
      overlayGroups = overlay_groups,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    leaflet::addControl(html = "<b>Legend:</b> use parameter show_legend = TRUE/FALSE",
                        position = "topright")
  
  return(map_leaflet)
}

# ---- Shiny basemap switcher + full app + Downloads ----
run_map_app <- function(stock_codes_fn,
                        repunits_fn = NULL,
                        baseline_fn = NULL,
                        gradient_fn = NULL,
                        boundaries_shp = NULL,
                        initial_plot_by = "repunit",
                        preset_boundary_field = "CU_Name",
                        preset_boundary_palette = "Set3") {
  
  ui <- fluidPage(
    titlePanel("Interactive Sockeye Map"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "basemap", "Basemap",
          choices = c("Esri.WorldTopoMap",
                      "Esri.WorldImagery",
                      "Esri.OceanBasemap",
                      "OpenStreetMap",
                      "CartoDB.Positron"),
          selected = "Esri.WorldTopoMap"
        ),
        selectInput(
          "palette", "Color palette",
          choices = c("viridis", "plasma", "magma", "inferno", "cividis"),
          selected = "viridis"
        ),
        checkboxInput("hover_points", "Hover labels for points", TRUE),
        checkboxInput("show_boundaries", "Show boundaries", !is.null(boundaries_shp)),
        checkboxInput("hover_boundaries", "Hover labels for boundaries", TRUE),
        checkboxInput("show_legend", "Show legends", TRUE),
        tags$hr(),
        checkboxInput("binary_on", "Binary Pass/Fail (threshold)", FALSE),
        numericInput("binary_threshold", "Threshold value", value = 80, min = 0, step = 1),
        selectInput("binary_operator", "Operator", choices = c(">=", ">"), selected = ">="),
        uiOutput("plotByUI"),
        uiOutput("boundaryColorUI"),
        tags$hr(),
        # --- Downloads ---
        downloadButton("dl_map_html",   "Download map (HTML)"),
        downloadButton("dl_points_geojson", "Download points (GeoJSON)")
      ),
      mainPanel(
        tmapOutput("map", height = "80vh")
      )
    )
  )
  
  server <- function(input, output, session) {
    tmap_mode("view")
    Sys.setenv("SHAPE_RESTORE_SHX" = "YES")
    
    stock_codes <- load_stock_codes(stock_codes_fn)
    repunits    <- if (!is.null(repunits_fn)) load_repunits(repunits_fn) else NULL
    joined      <- merge_repunits(stock_codes, repunits, filter_by_repunits = FALSE)
    
    baseline    <- if (!is.null(baseline_fn)) load_baseline(baseline_fn) else NULL
    joined      <- filter_to_baseline(joined, baseline)
    
    gradient    <- if (!is.null(gradient_fn)) load_gradient(gradient_fn) else NULL
    joined      <- attach_gradient(joined, gradient, by = "collection")
    
    joined      <- dplyr::select(joined, collection, dplyr::everything())
    points      <- prepare_sf_points(joined, x_col = "XLONG", y_col = "YLAT", shift_longitude = TRUE)
    
    boundaries_sf <- if (!is.null(boundaries_shp)) {
      load_boundaries_shp(boundaries_shp, target_crs = 4326, shift_longitude = TRUE)
    } else NULL
    
    # ---- Dynamic selectors ----
    plot_choices <- setdiff(names(points), c("geometry", "XLONG", "YLAT"))
    if (length(plot_choices) == 0) stop("No fields available to color points by. Check your input files.")
    
    initial_choice <- resolve_color_by(points, initial_plot_by, fallback = plot_choices[1])
    
    output$plotByUI <- renderUI({
      selectInput(
        "plot_by", "Color points by (numeric for binary)",
        choices  = plot_choices,
        selected = initial_choice
      )
    })
    
    boundary_color_choices <- if (!is.null(boundaries_sf)) names(boundaries_sf) else character(0)
    # BUG FIX: use correct variable name in filter
    boundary_color_choices <- boundary_color_choices[boundary_color_choices != "" & !is.na(boundary_color_choices)]
    
    output$boundaryColorUI <- renderUI({
      if (is.null(boundaries_sf)) return(NULL)
      selectInput(
        "boundaries_color_by", "Boundary fill (optional)",
        choices  = c("None", boundary_color_choices),
        selected = if (preset_boundary_field %in% boundary_color_choices) preset_boundary_field else "None"
      )
    })
    
    # ---- Map reactive ----
    map_reactive <- reactive({
      req(input$plot_by)
      b_sf <- if (isTRUE(input$show_boundaries)) boundaries_sf else NULL
      
      # Use preset palette if user picks a boundary field; otherwise outline only
      b_color_by <- if (!is.null(boundaries_sf) &&
                        !is.null(input$boundaries_color_by) &&
                        input$boundaries_color_by != "None") input$boundaries_color_by else NULL
      
      build_tmap(points_sf = points,
                 color_by = input$plot_by,
                 group_by = input$plot_by,
                 palette  = input$palette,
                 basemap  = input$basemap,
                 hover_label_points    = isTRUE(input$hover_points),
                 show_legend           = isTRUE(input$show_legend),
                 binary_on             = isTRUE(input$binary_on),
                 binary_threshold      = as.numeric(input$binary_threshold %||% 80),
                 binary_operator       = input$binary_operator,
                 binary_pass_label     = "Pass",
                 binary_fail_label     = "Fail",
                 binary_colors         = c(Fail = "#d73027", Pass = "#1a9850"),
                 boundaries_sf         = b_sf,
                 boundaries_group_name = "CU Boundaries",
                 boundaries_color_by   = b_color_by %||% preset_boundary_field,  # prefer user choice; else preset
                 boundaries_palette    = preset_boundary_palette,
                 boundaries_alpha      = 0.25,
                 boundaries_border_col = "#2b2b2b",
                 boundaries_border_lwd = 1,
                 hover_label_boundaries = isTRUE(input$hover_boundaries))
    })
    
    output$map <- renderTmap({ map_reactive() })
    
    # ---- Downloads ----
    # 1) HTML export of the current interactive map
    output$dl_map_html <- downloadHandler(
      filename = function() {
        paste0("interactive_map_", Sys.Date(), ".html")
      },
      content = function(file) {
        m <- map_reactive()
        # Try tmap_save first; fall back to saving the Leaflet widget
        ok <- try({
          tmap::tmap_save(m, filename = file)
        }, silent = TRUE)
        if (inherits(ok, "try-error")) {
          lw <- tmap::tmap_leaflet(m)
          htmlwidgets::saveWidget(lw, file = file, selfcontained = TRUE)
        }
      }
    )
    
    # 2) GeoJSON export of the points layer
    output$dl_points_geojson <- downloadHandler(
      filename = function() {
        paste0("points_", Sys.Date(), ".geojson")
      },
      content = function(file) {
        # write to GeoJSON (overwrite if exists)
        sf::st_write(points, dsn = file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
      }
    )
  }
  
  shinyApp(ui = ui, server = server)
}

# Small helper for Shiny to default missing numericInput values
`%||%` <- function(a, b) if (!is.null(a)) a else b

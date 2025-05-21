#' Launch Manual Neighbor Connector Add-in
#'
#' Starts a Shiny gadget for manually connecting polygon neighbors in the Viewer pane.
#' @export
runNeighborConnector <- function() {
  # Let user choose their sf object via a console list
  sf_choices <- ls(.GlobalEnv)[vapply(ls(.GlobalEnv), function(x) inherits(get(x, .GlobalEnv), "sf"), logical(1))]
  sf_name <- utils::select.list(
    sf_choices,
    title = "Select an sf object"
  )
  if (length(sf_name) == 0 || sf_name == "") {
    stop("No sf object chosen.")
  }

  # Retrieve and prepare data
  map_sf <- get(sf_name, envir = .GlobalEnv) |> dplyr::mutate(row_id__ = dplyr::row_number())
  centers <- suppressWarnings(sf::st_geometry(sf::st_centroid(map_sf)))

  # Build neighbor lists
  nb_original <- spdep::poly2nb(map_sf)
  nb <- nb_original
  manual_edits <- list()

  # UI
  ui <- shiny::fluidPage(
    title = "SF Neighbor Connector",
    shiny::titlePanel("SF neighbour connector"),
    shiny::fluidRow(shiny::column(12, leaflet::leafletOutput("map", height = "600px"))),
    shiny::br(),
    shiny::fluidRow(shiny::column(12, shiny::textOutput("selection", inline = TRUE))),
    shiny::br(),
    shiny::fluidRow(shiny::column(12, shiny::actionButton("reset", "Reset selection"))),
    shiny::br(),
    shiny::fluidRow(shiny::column(12, shiny::verbatimTextOutput("rcode")))
  )

  # Server
  server <- function(input, output, session) {
    clicked <- shiny::reactiveVal(character(0))

    output$map <- leaflet::renderLeaflet({
      no_nb <- which(spdep::card(nb_original) == 0)
      init_lines <- spdep::nb2lines(nb_original, coords = centers)
      leaflet::leaflet(map_sf) |>
        leaflet::addTiles() |>
        leaflet::addPolygons(
          layerId     = ~ as.character(row_id__),
          label       = ~ paste0("ID: ", row_id__),
          fillColor   = ~ ifelse(row_id__ %in% no_nb, "red", "gray"),
          fillOpacity = 0.6,
          color       = "black",
          weight      = 1
        ) |>
        leaflet::addPolylines(data = init_lines, group = "initial_links", color = "blue", weight = 2) |>
        leaflet::addPolylines(data = sf::st_sfc(), group = "manual_links", color = "darkred", weight = 3)
    })

    # add rcode
    output$rcode <- shiny::renderText(make_rcode(sf_name, manual_edits))

    # Add connections incrementally and update manual_edits
    shiny::observeEvent(input$map_shape_click, {
      id <- input$map_shape_click$id
      if (is.null(id)) {
        return()
      }
      sel <- clicked()
      if (!(id %in% sel)) clicked(c(sel, id))
      if (length(clicked()) == 2) {
        ids <- as.integer(clicked())
        i <- ids[1]
        j <- ids[2]

        if (!(j %in% nb[[i]])) {
          nb[[i]] <<- sort(c(nb[[i]], j))
          manual_edits[[length(manual_edits) + 1]] <<- sprintf("nb[[%d]] <- c(nb[[%d]], %d)", i, i, j)
        }
        if (!(i %in% nb[[j]])) {
          nb[[j]] <<- sort(c(nb[[j]], i))
          manual_edits[[length(manual_edits) + 1]] <<- sprintf("nb[[%d]] <- c(nb[[%d]], %d)", j, j, i)
        }

        clicked(character(0))

        # draw just this new connection
        new_line <- sf::st_sfc(
          sf::st_cast(
            sf::st_combine(
              sf::st_sfc(centers[[i]], centers[[j]], crs = sf::st_crs(map_sf))
            ),
            "LINESTRING"
          ),
          crs = sf::st_crs(map_sf)
        )
        leaflet::leafletProxy("map") |>
          leaflet::addPolylines(data = new_line, group = "manual_links", color = "darkred", weight = 3)
      }
      # Reactive R code output
      output$rcode <- shiny::renderText(make_rcode(sf_name, manual_edits))
    })

    shiny::observeEvent(input$reset, {
      clicked(character(0))
    })

    output$selection <- shiny::renderText({
      sel <- clicked()
      if (length(sel) == 0) {
        "Select a region by clicking on it."
      } else {
        paste("Selected:", paste(sel, collapse = " → "))
      }
    })
  }

  # Run as an RStudio add-in in the Viewer
  shiny::runGadget(shiny::shinyApp(ui, server), viewer = shiny::paneViewer(minHeight = 830))
}

# Build the reactive R code text
make_rcode <- function(sf_name, manual_edits) {
  if (length(manual_edits) == 0) {
    "# No manual edits made"
  } else {
    paste0(
      c(
        sprintf("nb <- spdep::poly2nb(%s)", sf_name),
        manual_edits,
        "nb <- spdep::make.sym.nb(nb)"
      ),
      collapse = "\n"
    )
  }
}

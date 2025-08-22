library(shiny)
library(bslib)
js_dir <- file.path("www", "js")
if (!dir.exists(js_dir)) dir.create(js_dir, recursive = TRUE)
library(shinyjs)
library(leaflet)
library(DT)
library(bslib)
library(shinyWidgets)


download.file(
  url = "https://unpkg.com/@panzoom/panzoom/dist/panzoom.min.js",
  destfile = file.path(js_dir, "panzoom.min.js"),
  mode = "wb"
)


myspecies_list <- c(
  "Dissostichus mawsoni", 
  "Dissostichus eleginoides", 
  "Champsocephalus gunnari"
)

# Clean names for folder usage
clean_names <- gsub("[ ()]", "", myspecies_list)

# Define app directory (where ui.R/server.R or app.R lives)
app_dir <- normalizePath("App")  # works anywhere

# Define paths for www, data, and Outputs for each species
www_paths <- file.path(app_dir, "www", clean_names)
data_paths <- file.path(app_dir, "data", clean_names)
output_paths <- file.path(app_dir, "Outputs", clean_names)

# Function to get the folder for a species and folder type
get_species_folder <- function(species_index, folder_type = c("www", "data", "Outputs")) {
  folder_type <- match.arg(folder_type)
  switch(folder_type,
         www = www_paths[species_index],
         data = data_paths[species_index],
         Outputs = output_paths[species_index])
}


ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bootswatch = "journal",            # base theme
    primary = "#1f77b4",              # main highlight color
    secondary = "#ff7f0e",
  ),
  tags$head(
    tags$style(HTML("
/* Top banner */
      #top-banner { 
      background-color: #155a8a ; 
      box-shadow: 0 2px 5px rgba(0,0,0,0.1); 
      color: white; 
      font-weight: bold; 
      font-size: 2.0em; text-align: center; 
  }
/* Sidebar styling */
#sidebar {
  width: 280px;
  background-color: #e0e0e0;
  padding: 20px;
  border-right: 1px solid #ccc;
  height: 100vh;
  position: fixed;
  overflow-y: auto;
  box-shadow: 2px 0 5px rgba(0,0,0,0.1);
  border-radius: 0 10px 10px 0;
  font-size: 16px; /* slightly larger for readability */
  line-height: 1.5;
}

#sidebar h2, #sidebar h4 {
  color: var(--bs-primary);
  font-weight: bold;
  margin-top: 20px;
  margin-bottom: 10px;
}

#sidebar p {
  margin-bottom: 15px; /* spacing between text elements */
}

#sidebar .fun-fact {
  font-style: italic; /* visually distinct */
  color: #555;
  margin-bottom: 15px;
}

#sidebar img {
  max-width: 100%;
  height: auto;
  max-height: 200px;
  display: block;
  margin: 15px 0;
  object-fit: contain;
  border-radius: 8px;
  box-shadow: 0 2px 5px rgba(0,0,0,0.15);
  transition: transform 0.2s ease;
}

#sidebar img:hover {
  transform: scale(1.03);
}

/* Main content */
#main {
  margin-left: 300px;
  padding: 20px;
}

/* Map container */
#map-container {
  flex: 1 1 auto;
  margin-bottom: 30px;
  margin-right: 20px;
  max-height: 80vh;
  overflow: auto;
  border: 1px solid #ddd;
  border-radius: 10px;
  box-shadow: 0 2px 5px rgba(0,0,0,0.1);
}

#map-container h3 {
  color: var(--bs-primary);
  font-weight: bold;
}

#map-container img {
  width: 100%;
  height: auto;
  object-fit: contain;
  border-radius: 8px;
  border: 1px solid #ddd;
}

.small-btn {
  background-color: #1f77b4; /* your blue */
  color: white;
  font-size: 0.9em;
  padding: 5px 10px;
  border-radius: 4px;
  border: none;
}
.small-btn:hover {
  background-color: #155a8a; /* slightly darker on hover */
}

/* Tables */
#tables-row {
  display: flex;
  justify-content: space-between;
  gap: 15px;
  margin-bottom: 20px;
}

#tables-row > div {
  border: 1px solid #ddd;
  border-radius: 10px;
  padding: 10px;
  background: white;
  box-shadow: 0 2px 5px rgba(0,0,0,0.05);
  overflow-y: auto;
  max-height: 400px;
}

#tables-row > div:first-child {
  flex: 0 0 70%;
}

#tables-row > div:last-child {
  flex: 0 0 30%;
}

#tables-row h3 {
  color: var(--bs-primary);
  font-weight: bold;
}

/* Table headers */
table thead {
  background-color: #e6f2ff;
  font-weight: bold;
}

/* Scrollbar for sidebar */
#sidebar::-webkit-scrollbar {
  width: 8px;
}
#sidebar::-webkit-scrollbar-thumb {
  background-color: #ccc;
  border-radius: 4px;
}

/* Leaflet map */
.leaflet-container {
  background: white !important;
  border-radius: 10px;
}

/* Titles */
.primary-title {
  color: var(--bs-primary);
  font-weight: bold;
}

/* Tabs */
.nav-tabs .nav-link.active {
  background-color: #cce5ff;
  color: var(--bs-primary);
  font-weight: bold;
  border-radius: 5px 5px 0 0;
}

.nav-tabs .nav-link {
  padding: 8px 15px;
  border-radius: 5px 5px 0 0;
  margin-right: 2px;
}

.nav-tabs .nav-link:hover {
  background-color: #ddeeff;
}

/* Response plots styling */
  .plot-container {
    border-radius: 10px;
    border: 1px solid #ddd;
    box-shadow: 0 2px 5px rgba(0,0,0,0.1);
    padding: 5px;
    background-color: white;
    margin-bottom: 20px;
  }
  
  #footer { 
  background-color: #f0f8ff; 
  padding: 8px 20px; 
  text-align: center; 
  font-size: 0.9em; color: #555; 
  margin-top: 30px; border-top: 1px solid #ddd; 
  }
  
  img { border-radius: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }
"))
  ),
  div(id = "top-banner", "Habitat Suitability Viewer"),
  
  tabsetPanel(
    id = "tabs",
    selected = "Model Output",
    
    tabPanel(
      "Model Output",
      
      div(id = "sidebar",
          selectInput("species", "Select Species:", choices = names(get_species_folder),
          hr(),
          h4("Species Snapshot"),
          uiOutput("species_snapshot"),
          imageOutput("species_image", width = "100%", height = "auto", inline = TRUE),
          p(HTML("<small>Image source: <a href='https://www.ccamlr.org/' target='_blank'>CCAMLR</a></small>"))
      ),
      
      div(id = "main",
          uiOutput("map_title_ui"), 
          
          # Map container
          div(id = "map-container",
              leafletOutput("mymap", height = "600px")
          ),
          
          # Download button outside map
          downloadButton(
            outputId = "download_raster",
            label = "Download Raster",
            class = "small-btn"
          ),
          
          # Layer selector
          selectInput("selected_layer", "Choose Raster Layer:",
                      choices = c("Favourability Mean", "Favourability Uncertainty")
          ),
          
          div(id = "tables-row",
              div(
                h3("Variable Importance"),
                tableOutput("var_imp_table")
              ),
              
              div(
                h3("Model Metrics"),
                tableOutput("metrics_table")
              )
          ),
          
          h3("Model Overview", class = "primary-title"),
          textOutput("species_overview"),
          
          h3("Response Curves", class = "primary-title"),
          uiOutput("response_curve_selector"),
          
          # Response curve image
          div(class = "plot-container",
              imageOutput("response_curve_image", width = "100%", height = "auto")
          ),
          
          # Footer
          div(id = "footer")
      )
    ), 
    
    tabPanel(
      "Species Information",
      h3("Species Information", class = "primary-title"),
      textOutput("species_info"),  
      h3("Species Overview", class = "primary-title"),
      textOutput("species_overview")
    ),
    
    tabPanel(
      "About",
      h3("About This App", class = "primary-title"),
      HTML("<p>This app visualizes habitat suitability models for species in the Southern Ocean...</p>")
    )
  )
  )
)

server <- function(input, output, session) {
  # ---- Reactive species folder paths ----
  species_clean <- reactive({
    req(input$species)
    gsub("[ ()]", "", input$species)
  })
  
  species_www <- reactive({ get_species_folder(input$species, "www") })
  species_data <- reactive({ get_species_folder(input$species, "data") })
  species_outputs <- reactive({ get_species_folder(input$species, "Outputs") })
  
  # ---- SNAPSHOT TEXT ----
  output$species_snapshot <- renderUI({
    snapshot_file <- file.path(species_www(), "overview", "snapshot.txt")
    if (!file.exists(snapshot_file)) return("Snapshot not available.")
    
    txt <- readLines(snapshot_file)
    parts <- strsplit(txt, ",")[[1]]
    
    formatted_lines <- lapply(parts, function(line) {
      kv <- strsplit(line, ":")[[1]]
      if (length(kv) == 2) {
        key <- trimws(kv[1])
        val <- trimws(kv[2])
        HTML(paste0("<b>", key, ":</b> ", val, "<br>"))
      } else {
        HTML(paste0(line, "<br>"))
      }
    })
    do.call(tagList, formatted_lines)
  })
  
  # ---- SNAPSHOT IMAGE ----
  output$species_image <- renderImage({
    img_path <- file.path(species_www(), "overview", "picture.jpg")
    if (!file.exists(img_path)) {
      return(list(src = "", contentType = "image/jpeg", alt = "No image available"))
    }
    list(src = img_path, contentType = "image/jpeg", alt = input$species)
  }, deleteFile = FALSE)
  
  # ---- HABITAT SUITABILITY MAP ----
  output$mymap <- renderLeaflet({
    fav_url <- paste0(species_clean(), "/maps/SO_fav_best_model.png")
    unc_url <- paste0(species_clean(), "/maps/SO_fav_best_modeluncert.png")
    
    leaflet(options = leafletOptions(zoomSnap = 0.4, zoomDelta = 0.4)) %>%
      htmlwidgets::onRender("
      function(el, x) { el.style.background = 'white'; }
    ") %>%
      htmlwidgets::onRender(sprintf("
      function(el, x) {
        var map = this;
        var imageBounds = [[-90, -180], [90, 180]];
        
        var favLayer = L.imageOverlay('%s', imageBounds);
        var uncLayer = L.imageOverlay('%s', imageBounds);
        
        favLayer.addTo(map);
        
        var baseMaps = {
          'Favourability': favLayer,
          'Uncertainty': uncLayer
        };
        L.control.layers(baseMaps, null, {collapsed: false}).addTo(map);
        map.fitBounds(imageBounds);
      }
    ", fav_url, unc_url))
  })
  
  # reactive returning currently selected raster
  current_raster <- reactive({
    switch(input$selected_layer,
           "Favourability Mean" = favour_mean,
           "Favourability Uncertainty" = favour_uncertainty)
  })
  
  # download handler
  output$download_raster <- downloadHandler(
    filename = function() {
      paste0(species_clean(), "_Predictions.tif")
    },
    content = function(file) {
      writeRaster(current_raster(), file, format = "GTiff")
    }
  )
  
  output$map_title_ui <- renderUI({
    h3(
      HTML(paste("Suitability Map: <em>", input$species, "</em>")),
      style = "color: var(--bs-primary); font-weight: bold;"
    )
  })
  
  # ---- VARIABLE IMPORTANCE SCORES ----
  output$var_imp_table <- renderTable({
    var_file <- file.path(species_data(), "bestvars_scores.csv")
    if (file.exists(var_file)) {
      read.csv(var_file)
    } else {
      data.frame(Message = "Variable importance data not available.")
    }
  })
  
  # ---- MODEL METRICS ----
  output$metrics_table <- renderTable({
    metric_file <- file.path(species_data(), "best_modeldiagnostics.csv")
    if (file.exists(metric_file)) {
      read.csv(metric_file)
    } else {
      data.frame(Message = "Model metrics data not available.")
    }
  })
  
  # ---- OVERVIEW TEXT ----
  output$species_overview <- renderText({
    overview_file <- file.path(species_www(), "overview", "overview.txt")
    if (file.exists(overview_file)) {
      paste(readLines(overview_file), collapse = "\n")
    } else {
      "Overview not available."
    }
  })
  
  # ---- RESPONSE CURVES ----
  response_curves <- reactive({
    req(input$species)
    if (!dir.exists(species_outputs())) return(NULL)
    files <- list.files(species_outputs(), pattern = "response\\.png$", full.names = FALSE)
    names(files) <- gsub("_", " ", tools::file_path_sans_ext(files))
    files
  })
  
  # Dropdown menu
  output$response_curve_selector <- renderUI({
    req(response_curves())
    selectInput(
      "selected_curve",
      "Select a response curve:",
      choices = response_curves()
    )
  })
  
  # Show selected response curve
  output$response_curve_image <- renderImage({
    req(input$selected_curve)
    img_path <- file.path(species_outputs(), input$selected_curve)
    list(
      src = img_path,
      contentType = "image/png",
      width = "100%",
      alt = "Response curve"
    )
  }, deleteFile = FALSE)
  
}

shinyApp(ui, server)

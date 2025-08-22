library(shiny)
library(bslib)
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
js_dir <- file.path("www", "js")
if (!dir.exists(js_dir)) dir.create(js_dir, recursive = TRUE)
library(shinyjs)
library(leaflet)
library(DT)
library(bslib)


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

clean_names <- gsub("[ ()]", "", myspecies_list)

species_paths <- file.path(getwd(), clean_names)
names(species_paths) <- myspecies_list

ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bootswatch = "journal",            # base theme
    primary = "#1f77b4",              # main highlight color
    secondary = "#ff7f0e",
  ),
  tags$head(
    tags$style(HTML("
      /* Sidebar styling */
      #sidebar {
        width: 280px;
        background-color: #e0e0e0;
        padding: 15px;
        border-right: 1px solid #ddd;
        height: 100vh;
        position: fixed;
        overflow-y: auto;
      }
      
      #sidebar h2 {
      color: var(--bs-primary); /* use your theme's primary color */
      font-weight: bold;
      }
      
      #sidebar h4 {
      color: var(--bs-primary); /* use your theme's primary color */
      font-weight: bold;
      }
      
       #sidebar img {
       max-width: 100%;      
       height: auto;         
       max-height: 200px;    
       display: block;       
       margin: 10px 0;       
       object-fit: contain;  
    }
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
    }
  
  #map-container h3 {
  color: var(--bs-primary);
  font-weight: bold;
}

  /* images inside map container */
    #map-container img {
    width: 100%;
    height: auto;
    object-fit: contain;
    border: 1px solid #ddd;
  }
      #tables-row {
  display: flex;
  justify-content: space-between;
  gap: 15px;
  margin-bottom: 20px;
}

#tables-row > div:first-child {
  flex: 0 0 70%;
  overflow-y: auto;
  max-height: 400px;
  border: 1px solid #ddd;
  padding: 10px;
  background: white;
}

#tables-row > div:last-child {
  flex: 0 0 30%;
  overflow-y: auto;
  max-height: 400px;
  border: 1px solid #ddd;
  padding: 10px;
  background: white;
}

/*Colour only the headings */
#tables-row h3 {
  color: var(--bs-primary);
  font-weight: bold;
}

      /* Keep sidebar scrollable */
      #sidebar::-webkit-scrollbar {
        width: 8px;
      }
      #sidebar::-webkit-scrollbar-thumb {
        background-color: #ccc;
        border-radius: 4px;
      }
    .leaflet-container {
    background: white !important;
    }
  
  /* Colour all other main titles with primary color */
.primary-title {
  color: var(--bs-primary);
  font-weight: bold;
}

.nav-tabs .nav-link.active {
  background-color: #cce5ff; /* light blue */
  color: var(--bs-primary);   /* text color */
  font-weight: bold;
}
    "))
  ),
  tabsetPanel(
    id = "tabs",
    selected = "Model Output",
    
    tabPanel(
      "Model Output",
      div(id = "sidebar",
          h2("Habitat Suitability Viewer"),
          selectInput("species", "Select Species:", choices = names(species_paths)),
          hr(),
          h4("Species Snapshot"),
          uiOutput("species_snapshot"),
          imageOutput("species_image", width = "100%", height = "auto", inline = TRUE),
          p(HTML("<small>Image source: <a href='https://www.ccamlr.org/' target='_blank'>CCAMLR</a></small>"))
      ),
      
      div(id = "main",
          uiOutput("map_title_ui"), 
          div(id = "map-container",
          leafletOutput("mymap", height = "600px")
          ),
          
          div(id = "tables-row",
              div(
                h3("Variable Importance"),
                tableOutput("var_imp_table"),
                DT::dataTableOutput("var_imp_table")
              ),
          div(
          h3("Model Metrics"),
          tableOutput("metrics_table"))
          ),
           
          h3("Model Overview", class = "primary-title"),
          textOutput("species_overview"),

    h3("Response Curves", class = "primary-title"),
    uiOutput("response_curve_selector"),   
    imageOutput("response_curve_image") 
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

server <- function(input, output, session) {
  species_path <- reactive({
    req(input$species)
    species_paths[input$species]
  })
  
  # ---- SNAPSHOT TEXT ----
  output$species_snapshot <- renderUI({
    req(input$species)
    snapshot_file <- file.path(species_path(), "overview", "snapshot.txt")
    
    if (!file.exists(snapshot_file)) {
      return("Snapshot not available.")
    }
    
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
    req(input$species)
    img_path <- file.path(species_path(), "overview", "picture.jpg") # now jpg
    
    if (!file.exists(img_path)) {
      return(list(src = "", contentType = "image/jpeg", alt = "No image available"))
    }
    
    list(
      src = img_path,
      contentType = "image/jpeg",
      alt = input$species
    )
  }, deleteFile = FALSE)
  
  # ---- MAP ----
  output$mymap <- renderLeaflet({
    req(input$species)
    
    species_clean <- gsub("[ ()]", "", input$species)
    
    fav_url <- paste0(species_clean, "/maps/SO_fav_best_model.png")
    unc_url <- paste0(species_clean, "/maps/SO_fav_best_modeluncert.png")
    
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
        
        favLayer.addTo(map); // default visible
        
        var baseMaps = {
          'Favourability': favLayer,
          'Uncertainty': uncLayer
        };
        
        L.control.layers(baseMaps, null, {collapsed: false}).addTo(map);
        map.fitBounds(imageBounds);
      }
    ", fav_url, unc_url))
  })
  
  output$map_title_ui <- renderUI({
    req(input$species)
    h3(
      HTML(paste("Suitability Map: <em>", input$species, "</em>")),
      style = "color: var(--bs-primary); font-weight: bold;"
    )
  })
  
  output$var_imp_table <- renderTable({
    var_file <- file.path(species_path(), "varselection", "bestvars_scores.csv")
    if (file.exists(var_file)) {
      read.csv(var_file)
    } else {
      data.frame(Message = "Variable importance data not available.")
    }
  })
  
  output$metrics_table <- renderTable({
    metric_file <- file.path(species_path(), "evaluation", "best_modeldiagnostics.csv")
    if (file.exists(metric_file)) {
      read.csv(metric_file)
    } else {
      data.frame(Message = "Model metrics data not available.")
    }
  })
  
  output$species_overview <- renderText({
    overview_file <- file.path(species_path(), "overview", "overview.txt")
    if (file.exists(overview_file)) {
      paste(readLines(overview_file), collapse = "\n")
    } else {
      "Overview not available."
    }
  })
  
  # Reactive: list available response curves
  response_curves <- reactive({
    req(input$species)
    species_folder <- species_paths[input$species]
    
    curves_dir <- file.path(species_folder, "varselection")
    if (!dir.exists(curves_dir)) return(NULL)
    
    # Find only files ending in partial.png
    files <- list.files(curves_dir, pattern = "response\\.png$", full.names = FALSE)
    
    # Make prettier names for dropdown
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
    species_folder <- species_paths[input$species]
    
    curves_dir <- file.path(species_folder, "varselection")
    img_path <- file.path(curves_dir, input$selected_curve)
    
    list(
      src = img_path,
      contentType = "image/png",
      width = "100%",
      alt = "Response curve"
    )
  }, deleteFile = FALSE)


}
shinyApp(ui, server)

library(shiny)
library(bslib)
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
js_dir <- file.path("www", "js")
if (!dir.exists(js_dir)) dir.create(js_dir, recursive = TRUE)
library(shinyjs)


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
  theme = bs_theme(version = 5),
  tags$head(
    tags$style(HTML("
      /* Sidebar styling */
      #sidebar {
        width: 280px;
        background-color: #f8f9fa;
        padding: 15px;
        border-right: 1px solid #ddd;
        height: 100vh;
        position: fixed;
        overflow-y: auto;
      }
      /* Main content fills the rest of the space */
      #main {
        margin-left: 300px;
        padding: 10px 20px;
        display: flex;
        flex-direction: column;
        height: 100vh;
        overflow-y: auto;
      }
      #map-container {
        flex: 1 1 auto;
        margin-bottom: 20px;
        max-height: 80vh;
      }
      #map-container img {
        width: 100%;
        height: auto;
        max-height: 80vh;
        object-fit: contain;
        border: 1px solid #ddd;
      }
      #tables-row {
        display: flex;
        justify-content: space-between;
        gap: 15px;
        margin-bottom: 20px;
      }
      #tables-row > div {
        flex: 1 1 45%;
        overflow-y: auto;
        max-height: 250px;
        border: 1px solid #ddd;
        padding: 10px;
        background: white;
      }
      /* Keep sidebar scrollable */
      #sidebar::-webkit-scrollbar {
        width: 8px;
      }
      #sidebar::-webkit-scrollbar-thumb {
        background-color: #ccc;
        border-radius: 4px;
      }
    ")),
    tags$script(src = "js/panzoom.min.js"),
    tags$script(HTML("
  document.addEventListener('DOMContentLoaded', function() {
    var image = document.getElementById('map-image');
    if(image) {
      console.log('Found the map-image element!');
      panzoom(image, {
        maxZoom: 5,
        minZoom: 1,
        contain: 'outside'
      });
    } else {
      console.log('Did NOT find the map-image element.');
    }
  });
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
          imageOutput("species_image")    
      ),
      
      div(id = "main",
          h3("Suitability Map"),
          div(id = "map-container",
              uiOutput("map")
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
          
          h3("Model Overview"),
          textOutput("species_overview")
      )
    ),
    
    tabPanel(
      "Species Information",
      h3("Species Information"),
      textOutput("species_info"),  
      h3("Species Overview"),
      textOutput("species_overview")
    ),
    
    tabPanel(
      "About",
      h3("About This App"),
      HTML("<p>This app visualizes habitat suitability models for species in the Southern Ocean...</p>")
    )
  )
)


server <- function(input, output, session) {
  
  species_path <- reactive({
    req(input$species)
    species_paths[input$species]
  })
  
  output$species_snapshot <- renderUI({
    req(input$species)
    snapshot_file <- file.path(species_path(), "overview", "snapshot.txt")
    if (file.exists(snapshot_file)) {
      # Read the text file (assuming it's one line like your example)
      txt <- readLines(snapshot_file)
      
      # Split by comma to separate each category
      parts <- strsplit(txt, ",")[[1]]
      
      # Clean up whitespace and format each part with bold key and value
      formatted_lines <- lapply(parts, function(line) {
        # Split key and value by colon
        kv <- strsplit(line, ":")[[1]]
        if(length(kv) == 2) {
          key <- trimws(kv[1])
          val <- trimws(kv[2])
          HTML(paste0("<b>", key, ":</b> ", val, "<br>"))
        } else {
          # If format is unexpected, just output the line as is
          HTML(paste0(line, "<br>"))
        }
      })
      
      # Return all lines as tagList
      do.call(tagList, formatted_lines)
      
    } else {
      "Snapshot not available."
    }
  })
  
  output$species_image <- renderImage({
    req(input$species)
    img_path <- file.path(species_path(), "overview", "picture.jpg")
    if (file.exists(img_path)) {
      list(src = normalizePath(img_path),
           contentType = "image/jpeg",
           width = 250,
           height = 150,
           alt = paste("Image of", input$species))
    } else {
      NULL
    }
  }, deleteFile = FALSE)
  
  output$species_overview <- renderText({
    overview_file <- file.path(species_path(), "overview", "overview.txt")
    if (file.exists(overview_file)) {
      paste(readLines(overview_file), collapse = "\n")
    } else {
      "Overview not available."
    }
  })
  
  output$map <- renderUI({
    req(input$species)
    clean_species <- gsub("[ ()]", "", input$species)
    img_src <- paste0("/", clean_species, "/maps/SO_fav_best_model.png")
    tags$img(src = img_src,
             id = "map-image",  
             style = "width: 100%; height: auto; border: 1px solid #ddd;",
             alt = paste("Map for", input$species))
  })
  
  output$var_imp_table <- renderTable({
    var_file <- file.path(species_path(), "varselection", "varsel_embScoresVIF.csv")
    if (file.exists(var_file)) {
      read.csv(var_file)
    } else {
      data.frame(Message = "Variable importance data not available.")
    }
  })
  
  output$metrics_table <- renderTable({
    metric_file <- file.path(species_path(), "evaluation", "modeldiagnosticsVIF.csv")
    if (file.exists(metric_file)) {
      read.csv(metric_file)
    } else {
      data.frame(Message = "Model metrics data not available.")
    }
  })
  
}

shinyApp(ui, server)

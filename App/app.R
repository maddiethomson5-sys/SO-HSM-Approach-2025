#Ensure to set working directory is defined wherever the app is saved. 

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
app_dir <- normalizePath(".", mustWork = TRUE)

# Define paths for www, data, and Outputs for each species
www_paths <- file.path(app_dir, "www", clean_names)
data_paths <- file.path(app_dir, "Data", clean_names)
output_paths <- file.path(app_dir, "Outputs", clean_names)

# Function to get the folder for a species and folder type
get_species_folder <- function(species_name, folder_type = c("www", "Data", "Outputs")) {
  folder_type <- match.arg(folder_type)
  file.path(folder_type, gsub("[ ()]", "", species_name))
}


ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bootswatch = "journal",            # base theme
    primary = "#1f77b4",              # main highlight color
    secondary = "#ff7f0e"
  ),
  tags$head(
    tags$style(HTML("

#page-container {
  display: flex;
  align-items: flex-start;   /* align top */
  width: 100%;               /* full width */
  margin: 0;                 /* reset bootstrap margins */
  padding: 0;                /* reset bootstrap padding */
}

/* Sidebar styling */
#sidebar-container {
  width: 280px;
  flex-shrink: 0;     /* don't let it shrink */
  height: 100vh;      /* full viewport height */
  overflow-y: auto;   /* independent scroll if needed */
}

#sidebar {
  background-color: #e0e0e0;
  padding: 20px;
  border-right: 1px solid #ccc;
  position: sticky;
  top: 0;
  box-shadow: 2px 0 5px rgba(0,0,0,0.1);
  border-radius: 0 10px 10px 0;
  font-size: 16px;
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
  flex: 1;            /* take up remaining horizontal space */
  padding: 20px;
  margin: 0;          /* no margin-left anymore */
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

.about-title {
    padding-top: 20px;  /* increase top spacing */
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
  padding-top: 20px;  
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

.nav-tabs {
  margin-top: -15px;   /* adjust this value until spacing looks right */
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
 
  tags$style(HTML("
.banner {
    background-color: #1f77b4;
    background-image: url('AntarcticaCoastline3.png');
    background-repeat: no-repeat;
    background-position: bottom center;
    background-size: 100% 80%;  /* stretch horizontally to fill width, 80% of banner height */
    height: 150px;
    color: white;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 32px;
    font-weight: bold;
    text-shadow: 
        2px 2px 4px rgba(0,0,0,0.7), 
       -2px -2px 4px rgba(0,0,0,0.7);  /* stronger shadow */
}
")),
  
  div(class = "banner",
      "Species Distribution Modeling of Southern Ocean Fishes"  # Your header text
  ),
  
  tabsetPanel(
    id = "tabs",
    selected = "Model Output",
    
    # -------------------- Model Output Tab --------------------
    tabPanel(
      "Model Output",
      
 div(id = "page-container",
    div(id = "sidebar-container",
      div(id = "sidebar",
          selectInput("species", "Select Species:", choices = myspecies_list, selected = myspecies_list[1]),
          hr(),
          h4("Species Snapshot"),
          uiOutput("species_snapshot"),
          imageOutput("species_image", width = "100%", height = "auto"),
          p(HTML("<small>Image source: <a href='https://www.ccamlr.org/' target='_blank'>CCAMLR</a></small>"))
      )
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
          
          tags$hr(style = "border-top: 2px solid #cccccc; margin-top: 20px; margin-bottom: 20px;"),

          
          div(id = "tables-row",
              div(
                h3("Variable Importance"),
               DT::DTOutput("var_imp_table")
              ),
              
              div(
                h3("Model Metrics"),
                tableOutput("metrics_table")
              )
          ),
          
          # About the model 
          h3("Modelling Approach", class = "section-title"),
          uiOutput("model_approach"),
          
          tags$div(style = "margin-top: 15px;"),  # <-- this creates a visible buffer
          
          # Response curve selector
          h3("Response Curves", class = "section-title"),
          uiOutput("response_curve_selector"),
          
          # Response curve image
          div(class = "plot-container",
              imageOutput("response_curve_image", width = "100%", height = "auto")
          ),
          
          # Footer
          tags$footer(
            "Developed by Maddie Thomson", 
            style = "text-align:center; padding:10px; color: #555; font-size:12px;"
          )
      ))
    ), 
    
    # -------------------- Species Information Tab --------------------
    tabPanel(
      "Species Information",
      
      fluidRow(
        # Main text column
        column(
          width = 8,
          h2("Species Information", class = "primary-title"),
          
          h3("Overview", class = "section-title"),
          uiOutput("species_overview"),
          
          div(style = "height:20px;"),
          
          h3("Distribution / Habitat", class = "section-title"),
          uiOutput("species_distribution"),
          
          div(style = "height:20px;"),
          
          # References inside this tab
          div(
            style = "background-color:#f5f5f5; font-size:0.85em; padding:10px; margin-top:20px; width:100%;",
            h4("References"),
            uiOutput("species_references")
          )
        ),
        
        # Life History sidebar
        column(
          width = 4,
          div(
            style = "
      background-color:#e0f7fa; 
      padding:20px; 
      border-radius:12px; 
      position: sticky;
      top: 20px;
      max-height: 80vh;
      overflow: auto;       /* allow scrolling if content is tall */
      box-shadow: 0 4px 8px rgba(0,0,0,0.2);
    ",
            h3("Biology", class = "section-title"),
            uiOutput("life_history_ui")
          ),
        )
      )
    ),
    
    # -------------------- About Tab --------------------
    tabPanel("About",
             fluidRow(
               column(12,
                      h2("About This App", class = "about-title", style = "color: var(--bs-primary);"),
                      div(htmlOutput("about_app")),
                      
                      # Access to Code section
                      div(
                        style = "background-color:#f0f8ff; padding:10px; border-radius:8px; border:2px solid #1f77b4; margin-top:10px; font-size:14px;",
                        h4("Access to Code", style="color:#1f77b4;"),
                        p(
                          "All the code behind this app is available on GitHub: ",
                          tags$a(
                            href="https://github.com/maddiethomson5-sys/SO-HSM-Approach-2025.git", 
                            HTML("<b style='color:#1f77b4;'>SO-HSM-Approach-2025</b> &#x1F517;"), 
                            target="_blank"
                          ),
                          style="margin:0;"
                        ),
                        p(
                          "For any questions, you can contact me, Maddie Thomson, at ",
                          tags$a(href="mailto:maddie.thomson5@gmail.com", "maddie.thomson5@gmail.com"),
                          style="margin:0;"
                        )
                      ),
                      
                      # Source Code section
                      div(
                        style = "padding:10px; margin-top:10px; background-color:#fafafa; border-radius:8px;",
                        h3("Source Code"),
                        uiOutput("species_sources")
                      ),
                      
                      # References section
                      div(
                        style = "padding:10px; margin-top:10px; background-color:#fafafa; border-radius:8px;",
                        h3("References"),
                        uiOutput("species_references2")
                      )
               )
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
  species_data <- reactive({ get_species_folder(input$species, "Data") })
  species_outputs <- reactive({ get_species_folder(input$species, "Outputs") })
  
  # ---- SNAPSHOT TEXT ----
  output$species_snapshot <- renderUI({
    snapshot_file <- file.path(species_www(), "snapshot.txt")
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
    img_path <- file.path(species_www(), "picture.jpg")
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
      layer_short <- ifelse(input$selected_layer == "Favourability Mean", "Mean", "Uncertainty")
      paste0(species_clean(), "_", layer_short, ".tif")
    },
    content = function(file) {
      # Determine the correct file path dynamically
      raster_file <- file.path(species_data(),
                               ifelse(input$selected_layer == "Favourability Mean",
                                      "Best_ModelFavourabilityMean.tif",
                                      "Best_ModelFavourabilityUncertainty.tif"))
      # Load the raster only when the button is pressed
      r <- raster::raster(raster_file)
      raster::writeRaster(r, file, format = "GTiff", overwrite = TRUE)
    }
  )
  
  
  # ---- VARIABLE IMPORTANCE SCORES ----
  output$var_imp_table <- DT::renderDataTable({
    var_file <- file.path(species_data(), "bestvars_scores.csv")
    
    if (file.exists(var_file)) {
      dat <- read.csv(var_file)
      
      DT::datatable(
        dat,
        options = list(
          pageLength = nrow(dat),
          dom = 't',
          ordering = TRUE
        ),
        rownames = FALSE,
        class = "row-border"  # horizontal borders only
      ) %>%
        DT::formatRound(columns = "Value", digits = 3)
      
    } else {
      DT::datatable(
        data.frame(Message = "Variable importance data not available."),
        options = list(dom = 't'),
        rownames = FALSE,
        class = "row-border"
      )
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
  
  # ---- MODELLING APPROACH ----
  output$model_approach <- renderUI({
    overview_file <- file.path(species_www(), "m_approach.txt")
    if (file.exists(overview_file)) {
      text_lines <- readLines(overview_file)
      # Replace the plain names with links
      text_lines <- gsub(
        "OBIS",
        '<a href="https://www.obis.org" target="_blank">OBIS</a>',
        text_lines
      )
      text_lines <- gsub(
        "GBIF",
        '<a href="https://www.gbif.org" target="_blank">GBIF</a>',
        text_lines
      )
      text_lines <- gsub(
        "Bio-ORACLE",
        '<a href="https://www.bio-oracle.org" target="_blank">Bio-ORACLE</a>',
        text_lines
      )
      
      HTML(paste(text_lines, collapse = "<br>"))
    } else {
      HTML("Model approach info not available.")
    }
  })

  
  # ---- OVERVIEW / DISTRIBUTION TEXT ----
  output$species_overview <- renderUI({
    overview_file <- file.path(species_www(), "overview.txt")
    if (file.exists(overview_file)) {
      overview_text <- paste(readLines(overview_file, warn = FALSE), collapse = "\n")
      parts <- strsplit(overview_text, "<p>")[[1]]
      text <- parts[1]           # <- MUST assign!
      text <- gsub("\\*(.*?)\\*", "<i>\\1</i>", text)  # convert *text* to italics
      text <- gsub("\n", "<br>", text)                # preserve line breaks
      HTML(text)                                      # render HTML
    } else {
      HTML("Overview not available.")
    }
  })
  
  # ---- DISTRIBUTION ----
  output$species_distribution <- renderUI({
    overview_file <- file.path(species_www(), "overview.txt")
    if (file.exists(overview_file)) {
      overview_text <- paste(readLines(overview_file, warn = FALSE), collapse = "\n")
      parts <- strsplit(overview_text, "<p>")[[1]]
      text <- if(length(parts) > 1) parts[2] else "Distribution/Habitat not available."
      
      # convert *text* to <i>text</i>
      text <- gsub("\\*(.*?)\\*", "<i>\\1</i>", text)
      text <- gsub("\n", "<br>", text)
      
      HTML(text)
    } else {
      HTML("Distribution/Habitat not available.")
    }
  })
  
  
  # ---- LIFE HISTORY ----
  life_history <- reactive({
    lifehistory_file <- file.path(species_www(), "lifehistory.txt")
    if (file.exists(lifehistory_file)) {
      strsplit(paste(readLines(lifehistory_file, warn = FALSE), collapse = "\n"), ";")[[1]]
    } else {
      c("Life history info not available.")
    }
  })
  
  output$life_history_ui <- renderUI({
    lh <- life_history()
  
    tags$ul(
      style = "list-style-type: none; padding-left: 0;",
      lapply(lh, function(b) {
        kv <- strsplit(b, ":")[[1]]
        if(length(kv) == 2){
          tags$li(
            HTML(paste0("<b>", kv[1], ":</b> ", kv[2])),
            style = "margin-bottom: 10px;"  # <-- adds spacing between points
          )
        } else {
          tags$li(HTML(b), style = "margin-bottom: 10px;")
        }
      })
    )
  })
  
  # ---- REFERENCES ----
  # SERVER
  output$species_references <- renderUI({
    references_file <- file.path(species_www(), "references.txt")
    if (file.exists(references_file)) {
      refs_raw <- readLines(references_file, warn = FALSE)
      
      # Split the single long line into references by semicolon
      refs <- unlist(strsplit(refs_raw, ";"))
      refs <- trimws(refs)  # remove leading/trailing spaces
      
      tags$div(
        style = "background-color:#f5f5f5; font-size:0.90em; padding:10px; margin-top:10px; border-radius:6px;",
        lapply(refs, function(r) tags$p(style = "margin:0 0 6px 0;", HTML(r)))
      )
    } else {
      tags$p("References not available.")
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
  
  # ---- ABOUT TAB ----
  about_text <- reactive({
    about_file <- file.path(species_www(), "about_app.txt")
    if (file.exists(about_file)) {
      lines <- readLines(about_file, warn = FALSE)
      
      # Replace plain names with links
      lines <- gsub("OBIS", '<a href="https://www.obis.org" target="_blank">OBIS</a>', lines)
      lines <- gsub("GBIF", '<a href="https://www.gbif.org" target="_blank">GBIF</a>', lines)
      lines <- gsub("Bio-ORACLE", '<a href="https://www.bio-oracle.org" target="_blank">Bio-ORACLE</a>', lines)
      
      # Convert *text* to italics
      lines <- gsub("\\*(.*?)\\*", "<i>\\1</i>", lines)
      
      # Wrap each non-empty line in a paragraph
      html_lines <- paste0("<p>", trimws(lines[lines != ""]), "</p>")
      
      paste(html_lines, collapse = "\n")
    } else {
      "About info not available."
    }
  })
  
  output$about_app <- renderUI({
    HTML(about_text())
  })

  # Code Sources 
  output$species_sources <- renderUI({
    sources_file <- file.path(species_www(), "sources.txt")
    if (file.exists(sources_file)) {
      sources <- strsplit(
        paste(readLines(sources_file, warn = FALSE), collapse = "\n"),
        ";"
      )[[1]]
      
      tags$div(
        style = "background-color:#f5f5f5; font-size:0.70em; padding:10px; margin-top:10px; border-radius:6px;",
        tags$ul(
          style = "list-style-type:none; padding-left:0; margin:0;",
          lapply(sources, function(s) tags$li(style = "margin-bottom:6px;", HTML(s)))
        )
      )
    } else {
      tags$p("Sources not available.")
    }
  })
  
  # References
  output$species_references2 <- renderUI({
    references2_file <- file.path(species_www(), "references2.txt")
    if (file.exists(references2_file)) {
      refs <- strsplit(
        paste(readLines(references2_file, warn = FALSE), collapse = "\n"),
        ";"
      )[[1]]
      
      tags$div(
        style = "background-color:#f5f5f5; font-size:0.70em; padding:10px; margin-top:10px; border-radius:6px;",
        tags$ul(
          style = "list-style-type:none; padding-left:0; margin:0;",
          lapply(refs, function(r) tags$li(style = "margin-bottom:6px;", HTML(r)))
        )
      )
    } else {
      tags$p("References not available.")
    }
  })
}

shinyApp(ui, server)

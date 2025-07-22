library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(shinyWidgets)

# Chargement du contour du Québec avec découpe du Saguenay
qc_contour <- readRDS("data/prov_sf.rds")

# Chargement des fichiers TBE annuels en une seule liste nommée
tbe_list <- lapply(2007:2024, function(yr) {
  df <- readRDS(file.path("data/periodes", paste0("TBE_", yr, ".rds")))
  df$AN_TBE <- yr
  df
})
names(tbe_list) <- as.character(2007:2024)

# UI
ui <- fluidPage(
  titlePanel(NULL),
  tags$style(HTML("
    .header-title {
      background-color: #2C3E50;
      color: white;
      padding: 20px;
      font-size: 22px;
      font-weight: bold;
      text-align: left;
      text-transform: uppercase;
      border-radius: 0px;
      margin-bottom: 20px;
      box-shadow: 2px 2px 8px rgba(0,0,0,0.2);
    }
    .box-style {
      background-color: #f9f9f9;
      border: 1px solid #ccc;
      border-radius: 8px;
      padding: 20px;
      box-shadow: 2px 2px 8px rgba(0,0,0,0.1);
      height: 700px;
      overflow-y: auto;
    }
    .irs--shiny .irs-bar,
    .irs--shiny .irs-bar-edge {
      background: #ccc !important;
      border-color: #ccc !important;
    }
    .irs--shiny .irs-single {
      background-color: #2C3E50 !important;
      color: white;
      border: none;
    }
    .irs--shiny .irs-line {
      background: #ccc !important;
      border-color: #ccc !important;
    }
  ")),
  div("Superficie annuelle affectée par la TBE de 2007 à 2024", class = "header-title"),
  fluidRow(
    column(6,
           div(class = "box-style",
               sliderTextInput(
                 inputId = "annee",
                 label = "Choisissez une année :",
                 choices = as.character(2007:2024),
                 selected = "2007",
                 grid = TRUE,
                 width = "100%"
               ),
               plotOutput("plot_saguenay", height = "250px"),
               plotOutput("plot_quebec", height = "250px")
           )
    ),
    column(6,
           div(class = "box-style",
               leafletOutput("map", height = "640px")
           )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  data_filtered <- reactive({
    req(input$annee)
    tbe_list[[as.character(input$annee)]]
  })
  
  output$map <- renderLeaflet({
    bbox <- st_bbox(qc_contour)
    
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = qc_contour,
        color = NA,
        fillColor = "gray20",
        fillOpacity = 0.4,
        weight = 0,
        group = "Province"
      ) %>%
      fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
                lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
  })
  
  observe({
    tbe_sf <- data_filtered()
    
    leafletProxy("map") %>%
      clearGroup("TBE") %>%
      addPolygons(
        data = tbe_sf,
        group = "TBE",
        fillColor = "#085016",
        fillOpacity = 0.7,
        color = NA,
        weight = 0#,
        #label = ~paste("Superficie:", round(SUP_HA), "ha"),
        #labelOptions = labelOptions(style = list("font-weight" = "bold"), textOnly = TRUE)
      )
  })
  
  output$plot_saguenay <- renderPlot({
    df_all <- do.call(rbind, tbe_list) %>%
      st_drop_geometry() %>%
      mutate(RES_NM_REG = trimws(RES_NM_REG)) %>%
      filter(RES_NM_REG == "Saguenay–Lac-Saint-Jean") %>%
      group_by(AN_TBE) %>%
      summarise(SUP_HA = sum(SUP_HA, na.rm = TRUE)) %>%
      mutate(SUP_HA = SUP_HA / 1e6,
             couleur = ifelse(AN_TBE == as.numeric(input$annee), "Sélectionnée", "Autre"))
    
    ggplot(df_all, aes(x = factor(AN_TBE), y = SUP_HA, fill = couleur)) +
      geom_bar(stat = "identity") +
      geom_text(
        data = subset(df_all, couleur == "Sélectionnée"),
        aes(label = sprintf("%.1f", SUP_HA)),
        vjust = -0.5,
        color = "black",
        size = 4
      ) +
      ylim(0, 15) +
      scale_fill_manual(values = c("Sélectionnée" = "gray50", "Autre" = "#4a5a76"), guide = "none") +
      labs(title = "Saguenay–Lac-Saint-Jean", x = NULL, y = "Superficie (millions ha)") +
      theme_minimal()
  })
  
  
  output$plot_quebec <- renderPlot({
    df_all <- do.call(rbind, tbe_list) %>%
      st_drop_geometry()
    
    df_quebec <- df_all %>%
      group_by(AN_TBE) %>%
      summarise(SUP_HA = sum(SUP_HA, na.rm = TRUE)) %>%
      mutate(SUP_HA = SUP_HA / 1e6,
             couleur = ifelse(AN_TBE == as.numeric(input$annee), "Sélectionnée", "Autre"))
    
    ggplot(df_quebec, aes(x = factor(AN_TBE), y = SUP_HA, fill = couleur)) +
      geom_bar(stat = "identity") +
      geom_text(
        data = subset(df_quebec, couleur == "Sélectionnée"),
        aes(label = sprintf("%.1f", SUP_HA)),
        vjust = -0.5,
        color = "black",
        size = 4
      ) +
      ylim(0, 15) +
      scale_fill_manual(values = c("Sélectionnée" = "gray50", "Autre" = "#085016"), guide = "none") +
      labs(title = "Province du Québec", x = NULL, y = "Superficie (millions ha)") +
      theme_minimal()
  })
  
  
}

shinyApp(ui, server)

# Kenya Gut Virome Dashboard
# Author: James Mordecai
# Study: Building a Catalogue of the Gut Virome in Kenya
# Data: KenyaViroCat_Dashboard_Dataset.csv

# Load Required Libraries
library(shiny)
library(shinydashboard)
library(bslib)
library(tidyverse)
library(plotly)
library(leaflet)
library(DT)
library(shinycssloaders)
library(shinyWidgets) 
library(thematic)

# Load Dataset
metadata <- read.csv("KenyaViroCat_Dashboard_Dataset.csv")

# Clean Up Unclassified Families
metadata$Family <- ifelse(tolower(metadata$Family) == "unclassified", NA, metadata$Family)

# Create Classified and Unclassified Subsets
classified_data <- metadata %>% filter(!is.na(Family))
unclassified_data <- metadata %>% filter(is.na(Family))


# Data Summaries 
contig_counts <- classified_data %>%
  group_by(Family, Region, Age_Group) %>%
  summarise(Contigs = n_distinct(Contig), .groups = "drop")

host_data <- classified_data %>%
  group_by(Host_Phylum, Region) %>%
  summarise(HostAbundance = n_distinct(Contig), .groups = "drop")

diversity_data <- classified_data %>%
  group_by(SampleID, Region) %>%
  summarise(ObservedContigs = n_distinct(Contig), .groups = "drop")

region_coords <- data.frame(
  Region = c("Nairobi", "Kwale", "Kilifi"),
  lat = c(-1.2921, -4.1816, -3.6224),
  lon = c(36.8219, 39.4606, 39.8484)
)

# Themes
light_theme <- bs_theme(bootswatch = "flatly", primary = "#2E7D32", base_font = font_google("Roboto"))
dark_theme  <- bs_theme(bootswatch = "darkly", primary = "#64B5F6", base_font = font_google("Roboto"))

# UI 
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Kenya Gut Virome Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Taxonomy", tabName = "taxonomy", icon = icon("dna")),
      menuItem("Functional", tabName = "functional", icon = icon("project-diagram")),
      menuItem("Diversity", tabName = "diversity", icon = icon("chart-bar")),
      menuItem("Geography", tabName = "geo", icon = icon("map")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    ),
    hr(),
    awesomeRadio(
      inputId = "mode",
      label = "Theme Mode:",
      choices = c("Light", "Dark"),
      selected = "Light",
      inline = TRUE
    )
  ),
  
  dashboardBody(
    thematic::thematic_shiny(),
    uiOutput("themeUI"),
    tabItems(
      
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_contigs", width = 3),
                valueBoxOutput("n_samples", width = 3),
                valueBoxOutput("n_families", width = 3),
                valueBoxOutput("n_regions", width = 3)
              ),
              br(),
              fluidRow(
                box(width = 4, title = "Classified vs Unclassified Viral Clusters",
                    withSpinner(plotlyOutput("classified_pie", height = 400))),
                box(width = 6, title = "Number of Viral Clusters per Viral Family", solidHeader = TRUE,
                    withSpinner(plotlyOutput("family_abundance_plot", height = 400))),
                box(width = 6, title = "Overall Bacterial Host Abundance", solidHeader = TRUE,
                    withSpinner(plotlyOutput("bacterial_host_plot", height = 400)))
              )
      ),
      
      # Taxonomy Tab
      tabItem(tabName = "taxonomy",
              fluidRow(
                box(width = 3, title = "Filters", solidHeader = TRUE,
                    selectInput("age_group", "Select Age Group:",
                                choices = c("All", sort(unique(metadata$Age_Group))),
                                selected = "All")),
                box(width = 9, title = "Viral Family Composition by Age Group", solidHeader = TRUE,
                    withSpinner(plotlyOutput("taxonomy_plot", height = 500)))
              )
      ),
      
      # Functional Tab
      tabItem(tabName = "functional",
              fluidRow(
                box(width = 3, title = "Filters", solidHeader = TRUE,
                    selectInput("region_filter", "Select Region:",
                                choices = c("All", sort(unique(metadata$Region))),
                                selected = "All")),
                box(width = 9, title = "Viral Family Distribution by Region", solidHeader = TRUE,
                    withSpinner(plotlyOutput("functional_plot", height = 500)))
              )
      ),
      
      # Diversity Tab
      tabItem(tabName = "diversity",
              fluidRow(
                box(width = 12, title = "Observed Viral Clusters per Sample", solidHeader = TRUE,
                    withSpinner(plotlyOutput("diversity_plot", height = 500)))
              )
      ),
      
      # Geography Tab
      tabItem(tabName = "geo",
              fluidRow(
                box(width = 12, title = "Sample Distribution Across Kenya", solidHeader = TRUE,
                    withSpinner(leafletOutput("kenya_map", height = 550)))
              )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
              fluidRow(
                box(width = 12, title = "Explore and Download Metadata", solidHeader = TRUE,
                    downloadButton("download_data", "Download CSV", icon = icon("download"), class = "btn-success"),
                    br(), br(),
                    withSpinner(DTOutput("data_table")))
              )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Theme switcher
  output$themeUI <- renderUI({
    if (input$mode == "Dark") bs_theme_dependencies(dark_theme)
    else bs_theme_dependencies(light_theme)
  })
  
  # Overview Value Boxes
  output$total_contigs <- renderValueBox({
    valueBox(format(n_distinct(metadata$Contig), big.mark = ","),
             "Total Viral Clusters (Including Unclassified)", icon = icon("virus"), color = "green")
  })
  output$n_samples <- renderValueBox({
    valueBox(nrow(classified_data), "Classified Viral Clusters", icon = icon("vial"), color = "green")
  })
  output$n_families <- renderValueBox({
    valueBox(length(unique(contig_counts$Family)), "Viral Families", icon = icon("virus"), color = "aqua")
  })
  output$n_regions <- renderValueBox({
    valueBox(length(unique(classified_data$Region)), "Regions", icon = icon("map"), color = "yellow")
  })
  
  # Pie Chart: Classified vs Unclassified
  output$classified_pie <- renderPlotly({
    counts <- data.frame(
      Category = c("Classified", "Unclassified"),
      Count = c(nrow(classified_data), nrow(unclassified_data))
    )
    plot_ly(counts, labels = ~Category, values = ~Count, type = "pie",
            textinfo = "label+percent", insidetextorientation = "radial",
            marker = list(colors = c("#2E7D32", "#B71C1C"))) %>%
      layout(title = "Classified vs Unclassified Viral Clusters")
  })
  
  # Overview: Contigs per Family
  output$family_abundance_plot <- renderPlotly({
    p <- ggplot(contig_counts, aes(x = reorder(Family, -Contigs), y = Contigs, fill = Region)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Number of Viral Clusters per Viral Family", x = NULL, y = "Number of Viral Clusters")
    ggplotly(p)
  })
  
  # Overview: Host Abundance
  output$bacterial_host_plot <- renderPlotly({
    p <- ggplot(host_data, aes(x = reorder(Host_Phylum, -HostAbundance), y = HostAbundance, fill = Region)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Overall Bacterial Host Abundance", x = NULL, y = "Relative Abundance")
    ggplotly(p)
  })
  
  # Taxonomy by Age
  output$taxonomy_plot <- renderPlotly({
    data <- contig_counts
    if (input$age_group != "All") data <- data %>% filter(Age_Group == input$age_group)
    
    p <- ggplot(data, aes(x = reorder(Family, -Contigs), y = Contigs, fill = Region)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Viral Family Composition -", input$age_group), x = NULL, y = "Number of Viral Clusters")
    ggplotly(p)
  })
  
  # Functional by Region
  output$functional_plot <- renderPlotly({
    data <- contig_counts
    if (input$region_filter != "All") data <- data %>% filter(Region == input$region_filter)
    
    p <- ggplot(data, aes(x = reorder(Family, -Contigs), y = Contigs, fill = Age_Group)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Viral Family Distribution -", input$region_filter), x = NULL, y = "Number of Viral Clusters")
    ggplotly(p)
  })
  
  # Diversity
  output$diversity_plot <- renderPlotly({
    p <- ggplot(diversity_data, aes(x = Region, y = ObservedContigs, fill = Region)) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      labs(title = "Observed Viral Clusters per Sample by Region", y = "Observed Viral Clusters", x = NULL)
    ggplotly(p)
  })
  
  # Geography
  output$kenya_map <- renderLeaflet({
    merged_data <- classified_data %>%
      group_by(Region) %>%
      summarise(SampleCount = n_distinct(SampleID), .groups = "drop") %>%
      left_join(region_coords, by = "Region")
    
    leaflet(merged_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(~lon, ~lat,
                       label = ~paste0(Region, ": ", SampleCount, " samples"),
                       color = "#2E7D32", fillOpacity = 0.9, radius = 10) %>%
      addLegend("bottomright", title = "Sample Distribution",
                colors = "#2E7D32", labels = "Regions with Samples", opacity = 1)
  })
  
  # Data Explorer + Download
  output$data_table <- renderDT({
    datatable(classified_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("KenyaGutVirome_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(classified_data, file, row.names = FALSE)
    }
  )
}

# Run App 
shinyApp(ui, server)

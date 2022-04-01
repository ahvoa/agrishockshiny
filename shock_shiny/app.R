#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(viridis)
library(plotly)
library(shinyWidgets)
library(tmap)
library(tmaptools)
library(terra)
library(sf)


path_to_data <- "C:/Users/ahvoa1/data/"
crop_list <- c("barley", "cassava", "groundnut", "maize", "millet", "potato", "rice", "sorghum", "soybean", "sugarbeet", "sugarcane", "wheat")
scenario_labs <- c("25 %", "50 %", "75 %")
data("World")
rob_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
#change projection on raster and World vector
data("World")
World_rob <- st_transform(World, rob_crs)
tmap_options(max.raster = c(plot = 1e9, view = 1e9))
names(scenario_labs) <- c(25, 50, 75)
pal_bivariate <- c("#d3d3d3", "#b6cdcd", "#97c5c5", "#75bebe", "#52b6b6",
                   "#d6c1a6", "#b8bba1", "#99b49c", "#77ae96", "#53a690",
                   "#d9ad77", "#bba874", "#9ca26f", "#799c6b", "#549567",
                   "#dd9843", "#bf9341", "#9e8e3f", "#7b893d", "#56833a",
                   "#e17e06", "#c27b06", "#a17606", "#7d7206", "#576d05")

##### UI #####
# Define UI for application that draws 
ui <- navbarPage("Agricultural input shocks",
        ###### Start tab ######         
        tabPanel("Start",
                 
          titlePanel("Background"),
          
          sidebarLayout(
            
            sidebarPanel("Lorem ipsum about the premise of the study ",
                         plotOutput("binlegend")),
            
            mainPanel(
              tabsetPanel(
                tabPanel("Agricultural inputs",
                         "more text lorem ipsum"),
                tabPanel("Climate bins",
                         
                         tmapOutput("climatebinmap"),
                         
                         selectInput("bincrop", label = "Select crop",
                                     choices = crop_list,
                                     selected = "barley")
                         
                         ) #tabpanel ends
                
              ) #tabset ends
              
            ) #mainpanel ends
            
          ) #sidebarlayout ends
                 
                 ), #panel ends
        
        ###### Scenarios tab ######
        tabPanel("Scenarios",
                 
          titlePanel("Scatterplots"),
          fluidRow(
            
            column(2,
              helpText("Examine shock scatterplots with different agricultural inputs."),
              
              
              selectInput("crop", label = "Choose crop:",
                          choices = c("barley", "cassava", "groundnut", "maize",
                                      "millet", "potato", "rice", "sorghum",
                                      "soybean", "sugarbeet", "sugarcane", "wheat"),
                          selected = "barley"),
              
              sliderInput("bin", 
                          label = "Choose climate bin:",
                          min = 1, max = 25, value = 10),
              
              selectInput("shock", label = "Choose shock type:",
                           choices = c("nitrogen shock", "phosphorus shock",
                                          "potassium shock",
                                           "machinery shock",
                                          "pesticide shock",
                                          "fertilizer shock", "shock in all inputs"), 
                           selected = "nitrogen shock"),
              
              selectInput("agri", 
                          label = "Choose agricultural input rate (colour):",
                          choices = c("nitrogen fertilizer", 
                                      "phosphorus fertilizer",
                                      "potassium fertilizer", 
                                      "machinery",
                                      "pesticides"),
                          selected = "nitrogen fertilizer"),
              
              checkboxInput("abline",
                            label = "Show 1:1 line",
                            value = FALSE)
              
              ), #column ends
              
            
            column(10, plotOutput("agriplot"))
            ) #column ends
        ), #panel ends
        
        tabPanel("next")
)

##### SERVER #####
# Define server logic required to draw 
server <- function(input, output) {
  
  ###### Climate bin plots ######
  output$binlegend <- renderPlot({
    bins <- as.data.frame(as.factor(seq(1, 25)))
    
    names(bins) <- "bin"
    
    bins$ones <- rep((rep(seq(1, 5, by= 1), 5)), 1)
    bins$tens <- rep((rep(seq(00, 20, by = 5), each= 5)), 1)
    
    bin_legend <- ggplot(data = bins, aes(ones, tens)) +
      geom_tile(aes(fill = bin)) + 
      geom_text(aes(label = bin, size = 2))+
      xlab(paste("precipitation \u2192"))+
      ylab(paste("temperature \u2192")) +
      scale_fill_manual(values = pal_bivariate) +
      theme_classic()+
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.title.y = element_text(margin = ggplot2::margin(0, -20, 0,0))) +
      ggtitle(paste0("Climate bins")) +
      scale_y_discrete(breaks=NULL)+
      scale_x_discrete(breaks = NULL)
    
    plot(bin_legend)
    
    
  })
  
  bin_data <- reactive({
    raster_name <- paste0(path_to_data, 'fertandbins/new_bins/5x5/', input$bincrop, "_binmatrix.tif")
    climate_bins <- rast(raster_name)
  })
  
  output$climatebinmap <- renderTmap({
    
    tm_shape(bin_data()) +
      tm_raster(style = "cont" , # draw gradient instead of classified
                palette = pal_bivariate,
                colorNA = "white") +
      tm_shape(World) +
      tm_view(alpha = 1, set.view = 1.5,
              leaflet.options = list(attributionControl = FALSE,
                                     maxBounds = c(-180, 180)))+
      tm_borders(col = "grey") +
      tm_layout(main.title = input$bincrop,
                main.title.size = 1,
                legend.show = FALSE
                #earth.boundary = c(-180, 180, -90, 90)
                )
    
  })
  
  
  
  ###### load crop scenarios for scatterplot ######
  plot_data <- reactive({
    file_name <- paste0(path_to_data, "results_final/scenarios/", input$crop, "/", input$crop, "_scenario_summary.RData")
    get(load(file_name))
    colnames(crop_scenarios) <- c("cell", "scenario_percent", "x", "y", "observed_normal_yield",
                               "nitrogen shock", "phosphorus shock",
                               "potassium shock", "machinery shock",
                               "pesticide shock", "fertilizer shock", 
                               "shock in all inputs", "bin", "nitrogen fertilizer", 
                               "phosphorus fertilizer",
                               "potassium fertilizer", 
                               "machinery", "pesticides", "irrigation")
    as.data.frame(crop_scenarios)
  })
  
  ###### scatterplot ######
  output$agriplot <- renderPlot({
    
    scatter_plot <- plot_data() %>%
      dplyr::filter(bin == input$bin) %>%
      dplyr::select(shock = input$shock, scenario_percent, input_rate = input$agri, observed_normal_yield, bin) %>%
      ggplot(aes(x = observed_normal_yield, y = shock))+
      geom_jitter(aes(color = input_rate), alpha = 0.5)+
      scale_color_viridis(name = paste0(input$agri, " kg/ha"), option = "magma", direction = -1)+
      theme_classic()+
      labs(x = "original yield t/ha", y = "yield after shock t/ha", title = input$shock)+
      theme(strip.background = element_blank())+
      facet_wrap(~scenario_percent, labeller = labeller(scenario_percent = scenario_labs))
    
    if (input$abline) {
      scatter_plot <- scatter_plot + 
        geom_abline(slope = 1, intercept = 0, color = "red")

    }
    
    plot(scatter_plot)
    
  })

}

##### Run the application #####
shinyApp(ui = ui, server = server)

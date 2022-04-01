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
library(leaflet)
library(raster)


path_to_data <- "C:/Users/ahvoa1/data/"
crop_list <- c("barley", "cassava", "groundnut", "maize", "millet", "potato", "rice", "sorghum", "soybean", "sugarbeet", "sugarcane", "wheat")
scenario_labs <- c("25 %", "50 %", "75 %")
data("World")
rob_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
#change projection on raster and World vector
data("World")
World_rob <- st_transform(World, rob_crs)
tmap_options(max.raster = c(plot = 1e9, view = 1e9))
ctrl_list <- list(attributionControl = FALSE,
                  zoomDelta = 0.5)
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
          
            tabsetPanel(
              tabPanel("Agricultural inputs",
                       "more text lorem ipsum"),
              tabPanel("Climate bins",
                 sidebarLayout(
                   
                   sidebarPanel("Lorem ipsum about the premise of the study ",
                                selectInput("bincrop", label = "Select crop",
                                            choices = crop_list,
                                            selected = "barley"),
                                
                                plotOutput("binlegend")),
                   
                    mainPanel(
                       tmapOutput("climatebinmap"),
                       
                       ) #tabpanel ends
                
              ) #tabset ends
              
            ) #mainpanel ends
            
          ) #tabset ends
                 
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
        
        tabPanel("Tiles",
          titlePanel("Tile plots"),
          fluidRow(
            
            column(2,
                   helpText("Examine scenario shocks from tileplots"),
                   
                   selectInput("tilecrop", "Select crop",
                               choices = crop_list,
                               selected = "barley")
                   ),
            
            column(10, plotlyOutput("tileplot"))
                  )

                 )
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
                  colorNA = "white",
                  title = input$bincrop) +
        tm_shape(World) +
        tm_borders(col = "grey") +
        tm_view(alpha = 1, set.view = c(0,0,1.25),
                leaflet.options = ctrl_list)
    
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
  
  ###### Tile plots ######
  
  tileplot_data <- reactive({
    file_name <- paste0(path_to_data, "results_final/scenarios/", input$tilecrop, "/", input$tilecrop, "_scenario_summary.RData")
    get(load(file_name))
    
  })
  
  output$tileplot <- renderPlotly({
    
    count_test <- tileplot_data() %>%
      dplyr::group_by(scenario_percent, bin) %>%
      dplyr::summarise(across(contains("shock"), ~ sum( .x < 0.9 * observed_normal_yield)))
    
    count_test <- tileplot_data() %>%
      dplyr::filter(scenario_percent == 25) %>%
      dplyr::count(bin) %>%
      right_join(count_test, by = "bin")
    
    count_test <- count_test %>%
      dplyr::mutate(across(contains("shock"), ~ (.x/n)*100))
    
    ### make climate tile plots for count
    
    climate_count <- count_test %>%
      dplyr::filter(scenario_percent == 75)
    
    climate_count$ones <- rep((rep(seq(1, 5, by= 1), 5)), 1)
    climate_count$tens <- rep((rep(seq(00, 20, by = 5), each= 5)), 1)
    
    climate_pivot_count <- climate_count %>%
      dplyr::select(-scenario_percent, -n)%>%
      pivot_longer(c(2:8), names_to = "shock", values_to = "count")
    
    climate_pivot_count%>%
      mutate(across(shock, factor, levels=c("N_rate_shock","P_rate_shock","K_rate_shock",
                                            "machinery_shock", "rescaled_pesticide_sum_shock",
                                            "shock_fert_only", "shock_all"))) %>%
      mutate(across(shock, plyr::mapvalues, from = c("N_rate_shock","P_rate_shock","K_rate_shock",
                                                     "machinery_shock", "rescaled_pesticide_sum_shock",
                                                     "shock_fert_only", "shock_all"),
                    to = c("N-rate shock", "P-rate shock", "K-rate shock", "machinery shock",
                           "pesticide shock", "fertilizer shock", "all inputs shock")))%>%
      ggplot(aes(ones, tens)) +
      geom_tile(aes(fill = count)) + 
      geom_text(aes(label = bin), size = 2)+
      labs(x = "precipitation", y = "temperature") +
      #scale_fill_viridis(direction = 1, name = "% area of bin")+
      scale_fill_scico(name = "% area of bin", palette = "batlow", direction = 1, limits = c(0, 100))+
      ggtitle(paste0(crop_list[i], ", cells where yield decline was more than 10%")) +
      theme(axis.text.x = element_text(angle=90))+
      theme_classic()+
      theme(axis.ticks = element_line(),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            strip.background = element_blank()) +
      scale_y_discrete(breaks=NULL)+
      scale_x_discrete(breaks = NULL)+
      facet_wrap(~shock)
    

    ### make normal tile plot count
    
    count_pivot <- count_test %>%
      pivot_longer(cols = c(4:10), names_to = "shock" , values_to = "percent")
    
    
    count_pivot %>%
      #dplyr::filter(scenario_percent == 75)%>%
      ggplot(aes(as.numeric(bin), shock))+
      geom_tile(aes(fill = percent))+
      scale_fill_scico(name = "% of bin area", palette = "batlow", direction = 1, limits = c(0, 100))+
      scale_y_discrete("",limits = unique(count_pivot$shock), 
                       labels = c("N shock", "P shock", "K shock", "machinery shock",
                                  "pesticide shock", "fertilizer shock", "shock all"))+
      scale_x_continuous("bin", breaks = seq(5,25,5))+
      ggtitle(paste0(crop_list[i], ", cells where yield decline was more than 10%"))+
      theme(#axis.text.x = element_text(size = 7, angle = 0),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank())+
      facet_wrap(~scenario_percent, nrow = 1, labeller = labeller(scenario_percent = scenario_labs))
    
    
    scenario_labs <- c("25 %", "50 %", "75 %")
    names(scenario_labs) <- c(25, 50, 75)
    
    ### make plot from yield decrease depth
    
    depth_test <- crop_scenarios %>%
      dplyr::group_by(scenario_percent, bin) %>%
      dplyr::mutate(across(contains("shock"), ~ 100*( .x - observed_normal_yield)/observed_normal_yield)) %>%
      dplyr::select(-x, -y, -observed_normal_yield, -N_rate, -P_rate, -K_rate, -machinery, -rescaled_pesticide_sum, -irrigation_share)
    
    depth_test_neg <- depth_test %>%
      dplyr::mutate(across(contains("shock"), ~ replace(.x, .x > -10, NA)))
    
    next_test <- depth_test_neg %>%
      dplyr::select(-cell)%>%
      dplyr::group_by(bin, scenario_percent) %>%
      dplyr::summarise(across(contains("shock"), ~mean(.x, na.rm = TRUE)))
    
    ### make graph with climate tiles ###
    
    climate_depth <- next_test %>%
      dplyr::filter(scenario_percent == 75)
    
    climate_depth$ones <- rep((rep(seq(1, 5, by= 1), 5)), 1)
    climate_depth$tens <- rep((rep(seq(00, 20, by = 5), each= 5)), 1)
    
    climate_pivot <- climate_depth %>%
      dplyr::select(-scenario_percent)%>%
      pivot_longer(c(2:8), names_to = "shock", values_to = "decline")
    
    
    climate_pivot %>%
      mutate(across(shock, factor, levels=c("N_rate_shock","P_rate_shock","K_rate_shock",
                                            "machinery_shock", "rescaled_pesticide_sum_shock",
                                            "shock_fert_only", "shock_all"))) %>%
      mutate(across(shock, plyr::mapvalues, from = c("N_rate_shock","P_rate_shock","K_rate_shock",
                                                     "machinery_shock", "rescaled_pesticide_sum_shock",
                                                     "shock_fert_only", "shock_all"),
                    to = c("N-rate shock", "P-rate shock", "K-rate shock", "machinery shock",
                           "pesticide shock", "fertilizer shock", "all inputs shock")))%>%
      ggplot(aes(ones, tens)) +
      geom_tile(aes(fill = decline)) + 
      geom_text(aes(label = bin), size = 2)+
      labs(x = "precipitation", y = "temperature") +
      #scale_fill_viridis(direction = -1, name = "average yield decline")+
      scale_fill_scico(name = "average yield decline < -10%", palette = "batlow", direction = -1, limits = c(-50, -10), oob = squish)+
      ggtitle(paste0(crop_list[i], ", mean yield decline of bin cells where yield decline was more than 10%")) +
      theme(axis.text.x = element_text(angle=90))+
      theme_classic()+
      theme(axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            strip.background = element_blank()) +
      scale_y_discrete(breaks=NULL)+
      scale_x_discrete(breaks = NULL)+
      facet_wrap(~shock)
    
    climate_pivot <- climate_depth %>%
      dplyr::select(-scenario_percent)%>%
      pivot_longer(c(2:8), names_to = "shock", values_to = "decline")
    
    ### make normal depth tile plot
    
    depth_pivot <- next_test %>%
      pivot_longer(c(3:9), names_to = "shock", values_to = "decline")
    
    depth_pivot %>%
      ggplot(aes(as.numeric(bin), shock)) +
      geom_tile(aes(fill = decline))+
      scale_fill_scico(name = "average yield decline < -10%", palette = "batlow", direction = -1, limits = c(-50, -10), oob = squish)+
      #scale_fill_viridis(name = "% area of bin", option = "plasma", direction = -1, limits = c(0, 100))+
      scale_y_discrete("",limits = unique(depth_pivot$shock), 
                       labels = c("N shock", "P shock", "K shock", "machinery shock",
                                  "pesticide shock", "fertilizer shock", "shock all"))+
      scale_x_continuous("bin", breaks = seq(5,25,5))+
      ggtitle(paste0(crop_list[i], ", Mean yield decline of bin cells where yield decline was more than 10%"))+
      theme(#axis.text.x = element_text(size = 7, angle = 0),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank())+
      facet_wrap(~scenario_percent, nrow = 1, labeller = labeller(scenario_percent = scenario_labs))
    
    
    
  })
  

}

##### Run the application #####
shinyApp(ui = ui, server = server)

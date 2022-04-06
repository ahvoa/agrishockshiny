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
library(scico)
library(dplyr)
library(tidyr)
library(scales)
library(wesanderson)


##### Helper variables and functions #####

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

scenario_pal <- viridis(n = 5, option = "magma", direction = -1)

get(load(paste0(path_to_data, "results_final/prod_change_bardata.RData")))
get(load(paste0(path_to_data, "results_final/prod_change_countries.RData")))

prod_change_raster <- raster(paste0(path_to_data, "results_final/prod_change_raster_norm.tif"))

make_shock_raster_shiny <- function(crop_scenarios, shock_column, scenario_number) {
  
  #select data for raster, make columns of scenario percents
  raster_data <- crop_scenarios %>%
    dplyr::select(cell, shock = shock_column, observed_normal_yield, scenario_percent) %>%
    dplyr::filter(scenario_percent == scenario_number)%>%
    mutate(shock_value = 100 * ((shock - observed_normal_yield)/ observed_normal_yield))%>%
    dplyr::select(cell, shock_value)
  
  #turn all positive values to zero
  raster_data$shock_value[raster_data$shock_value > 0] <- 0
  raster_data$shock_value <- round(raster_data$shock_value)
  
  #get a raster file to input shock values to
  earthstat_file <- list.files(path = "C:/Users/ahvoa1/data/EarthStat/",
                               recursive = TRUE, pattern = paste0(crop_list[1], "_YieldPerHectare.tif"), full.names = TRUE)
  yield_raster <- rast(earthstat_file)
  
  #substitute all values of the raster
  yield_raster[] <- 9999
  
  #create a dataframe of the raster with cell numbers and coordinates
  yield_df <- terra::as.data.frame(yield_raster, xy = TRUE, cells = TRUE)
  
  #join shock data to empty raster by cell number
  raster_data_with_all_cells <- left_join(yield_df, raster_data, by = "cell")
  #delete cell number column and yield column
  raster_data_with_all_cells <- raster_data_with_all_cells %>%
    dplyr::select(-cell, -barley_YieldPerHectare)
  
  #create raster
  shock_raster <- rasterFromXYZ(raster_data_with_all_cells, res = c(0.08333333, 0.08333333), crs = "+proj=longlat +datum=WGS84", digits = 6)
  shock_raster <- rast(shock_raster)
  
  #create a vector of shock values
  shock_vec <- na.omit(c(as.matrix(shock_raster)))
  
  #substitute NA-values of shock raster
  shock_raster[is.na(shock_raster)] <- 9999
  
  #select all result values for mask
  shock_mask <- shock_raster <= 0
  
  #create a raster base for the final
  final_raster <- rast(shock_mask)
  
  #substitute base raster shock mask area with result values
  final_raster[shock_mask] <- shock_vec
  
  return(final_raster)
  
}



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
                       tmapOutput("climatebinmap")
                       
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
        
        
        
        ###### Tiles ###### 
        tabPanel("Tiles",
          titlePanel("Tile plots"),
          fluidRow(
            
            column(3,
                   helpText("Examine scenario shocks from tileplots"),
                   
                   selectInput("tilecrop", "Select crop",
                               choices = crop_list,
                               selected = "barley"),
                   
                   radioButtons("tilescenario", "Select shock severity",
                                choices = c(25, 50, 75),
                                selected = 50),
                   
                   radioButtons("tilecount", "Select to see either share of cells where yield decline was > 10% (share),
                            or the mean decline in the cells where yield decline was > 10% (mean)",
                                choices = c("share", "mean"),
                                selected = "share"),
                   
                   checkboxInput("tileclimate", "Select if you would like to see
                                 tileplots grouped in climate bins",
                                 value = FALSE)
                   
                   ),
            
            column(9, plotlyOutput("tileplot"))
                  )

                 ),
        
        
        
        ###### Maps ######
        
        tabPanel("Scenario maps",
                 fluidRow(
                   
                   column(3,
                          helpText("Draw maps from different angricultural input shock scenarios"),
                          
                          selectInput("mapcrop", "Select crop:",
                                      choices = crop_list,
                                      selected = "barley"),
                          
                          selectInput("mapshock", "Select shock:",
                                      choices = c("nitrogen shock", "phosphorus shock",
                                                  "potassium shock",
                                                  "machinery shock",
                                                  "pesticide shock",
                                                  "fertilizer shock", "shock in all inputs"), 
                                      selected = "nitrogen shock"),
                          
                          radioButtons("mapscenario", "Select shock severity:",
                                       choiceNames = c("25%", "50%", "75%"),
                                       choiceValues = c(25, 50, 75),
                                       selected = 50)
                          ),
                   
                   column(9,
                          
                          tmapOutput("scenariomap"))
                 )),
        
        
        
        
        ###### Production ######
        tabPanel("Production",
                 titlePanel("Production plots"),
                 fluidRow(
                   
                   column(3,
                          helpText("Examine scenario shock effects on total production")
                   ),
                   
                   column(9,
                          
                      tabsetPanel(
                        
                        tabPanel("Different crops",
                                  plotlyOutput("productionbars")),    
                      
                        tabPanel("All crops",
                                  tmapOutput("productionraster")),
                      
                        tabPanel("All countries",
                                  tmapOutput("productioncountries"))
                      
                   )
                 
                 )))
        
        
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
                  title = input$bincrop,
                  legend.show = FALSE) +
        tm_shape(World) +
        tm_borders(col = "grey") +
        tm_view(alpha = 1, set.view = c(30,50,2),
                leaflet.options = ctrl_list,
                view.legend.position = NA)
    
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
    
    if (input$tilecount == "share") {
    
      count_test <- tileplot_data() %>%
        dplyr::group_by(scenario_percent, bin) %>%
        dplyr::summarise(across(contains("shock"), ~ sum( .x < 0.9 * observed_normal_yield)))
      
      count_test <- tileplot_data() %>%
        dplyr::filter(scenario_percent == 25) %>%
        dplyr::count(bin) %>%
        right_join(count_test, by = "bin")
      
      count_test <- count_test %>%
        dplyr::mutate(across(contains("shock"), ~ (.x/n)*100))
      
      if (input$tileclimate) {
      
        ### make climate tile plots for count
        
        climate_count <- count_test %>%
          dplyr::filter(scenario_percent == input$tilescenario)
        
        climate_count$ones <- rep((rep(seq(1, 5, by= 1), 5)), 1)
        climate_count$tens <- rep((rep(seq(00, 20, by = 5), each= 5)), 1)
        
        climate_pivot_count <- climate_count %>%
          dplyr::select(-scenario_percent, -n)%>%
          pivot_longer(c(2:8), names_to = "shock", values_to = "count")
        
        tile_plot <- climate_pivot_count%>%
          mutate(across(shock, factor, levels=c("N_rate_shock","P_rate_shock","K_rate_shock",
                                                "machinery_shock", "rescaled_pesticide_sum_shock",
                                                "shock_fert_only", "shock_all"))) %>%
          mutate(across(shock, plyr::mapvalues, from = c("N_rate_shock","P_rate_shock","K_rate_shock",
                                                         "machinery_shock", "rescaled_pesticide_sum_shock",
                                                         "shock_fert_only", "shock_all"),
                        to = c("N-rate shock", "P-rate shock", "K-rate shock", "machinery shock",
                               "pesticide shock", "fertilizer shock", "all inputs shock")))%>%
          ggplot(aes(ones, tens)) +
          geom_text(aes(label = bin), size = 2)+
          geom_tile(aes(fill = count)) + 
          xlab(paste("precipitation \u2192"))+
          ylab(paste("temperature \u2192")) +
          #scale_fill_viridis(direction = 1, name = "% area of bin")+
          scale_fill_scico(name = "% area of bin", palette = "batlow", direction = 1, limits = c(0, 100))+
          ggtitle(paste0(input$tilecrop, ", cells where yield decline was more than 10%")) +
          theme(axis.text.x = element_text(angle=90))+
          theme_classic()+
          theme(axis.ticks = element_blank(),
                axis.line.x = element_blank(),
                axis.line.y = element_blank(),
                axis.text = element_blank(),
                strip.background = element_blank()) +
          facet_wrap(~shock, nrow = 2)
        
        ggplotly(tile_plot)
      
      } else {
  
      ### make normal tile plot count
      
        count_pivot <- count_test %>%
          pivot_longer(cols = c(4:10), names_to = "shock" , values_to = "percent")
        
        tile_plot <- count_pivot %>%
          dplyr::filter(scenario_percent == input$tilescenario)%>%
          ggplot(aes(as.numeric(bin), shock))+
          geom_tile(aes(fill = percent))+
          scale_fill_scico(name = "% of bin area", palette = "batlow", direction = 1, limits = c(0, 100))+
          scale_y_discrete("",limits = unique(count_pivot$shock), 
                           labels = c("N shock", "P shock", "K shock", "machinery shock",
                                      "pesticide shock", "fertilizer shock", "shock all"))+
          scale_x_continuous("bin", breaks = seq(5,25,5))+
          ggtitle(paste0(input$tilecrop, ", cells where yield decline was more than 10%"))+
          theme(#axis.text.x = element_text(size = 7, angle = 0),
            axis.ticks.y = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            strip.background = element_blank())#+
          #facet_wrap(~scenario_percent, nrow = 1, labeller = labeller(scenario_percent = scenario_labs))
      
          ggplotly(tile_plot)
        }  
    } else {
    
    ### make plot from yield decrease depth
    
      depth_test <- tileplot_data() %>%
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
      
      if (input$tileclimate) {
      
        climate_depth <- next_test %>%
          dplyr::filter(scenario_percent == input$tilescenario)
        
        climate_depth$ones <- rep((rep(seq(1, 5, by= 1), 5)), 1)
        climate_depth$tens <- rep((rep(seq(00, 20, by = 5), each= 5)), 1)
        
        climate_pivot <- climate_depth %>%
          dplyr::select(-scenario_percent)%>%
          pivot_longer(c(2:8), names_to = "shock", values_to = "decline")
        
        tile_plot <- climate_pivot %>%
          mutate(across(shock, factor, levels=c("N_rate_shock","P_rate_shock","K_rate_shock",
                                                "machinery_shock", "rescaled_pesticide_sum_shock",
                                                "shock_fert_only", "shock_all"))) %>%
          mutate(across(shock, plyr::mapvalues, from = c("N_rate_shock","P_rate_shock","K_rate_shock",
                                                         "machinery_shock", "rescaled_pesticide_sum_shock",
                                                         "shock_fert_only", "shock_all"),
                        to = c("N-rate shock", "P-rate shock", "K-rate shock", "machinery shock",
                               "pesticide shock", "fertilizer shock", "all inputs shock")))%>%
          ggplot(aes(ones, tens)) +
          geom_text(aes(label = bin), size = 2)+
          geom_tile(aes(fill = decline)) + 
          xlab(paste("precipitation \u2192"))+
          ylab(paste("temperature \u2192")) +
          scale_fill_scico(name = "average yield decline < -10%", palette = "batlow", direction = -1, limits = c(-50, -10), oob = squish)+
          ggtitle(paste0(input$tilecrop, ", mean yield decline of bin cells where yield decline was more than 10%")) +
          theme(axis.text.x = element_text(angle=90))+
          theme_classic()+
          theme(axis.line.x = element_blank(),
                axis.line.y = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                strip.background = element_blank()) +
          facet_wrap(~shock, nrow = 2)
        
        ggplotly(tile_plot)
        
        
      } else {
      ### make normal depth tile plot
      
        depth_pivot <- next_test %>%
          dplyr::filter(scenario_percent == input$tilescenario) %>%
          pivot_longer(c(3:9), names_to = "shock", values_to = "decline")
        
        tile_plot <- depth_pivot %>%
          ggplot(aes(as.numeric(bin), shock)) +
          geom_tile(aes(fill = decline))+
          scale_fill_scico(name = "average yield decline < -10%", palette = "batlow", direction = -1, limits = c(-50, -10), oob = squish)+
          #scale_fill_viridis(name = "% area of bin", option = "plasma", direction = -1, limits = c(0, 100))+
          scale_y_discrete("",limits = unique(depth_pivot$shock), 
                           labels = c("N shock", "P shock", "K shock", "machinery shock",
                                      "pesticide shock", "fertilizer shock", "shock all"))+
          scale_x_continuous("bin", breaks = seq(5,25,5))+
          ggtitle(paste0(input$tilecrop, ", Mean yield decline of bin cells where yield decline was more than 10%"))+
          theme(#axis.text.x = element_text(size = 7, angle = 0),
            axis.ticks.y = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            strip.background = element_blank())#+
          #facet_wrap(~scenario_percent, nrow = 1, labeller = labeller(scenario_percent = scenario_labs))
      
        ggplotly(tile_plot)
        
        }
    }
    
  })
  
  ###### Maps ######
  
  #load data
  scenario_map_data <- reactive({
    file_name <- paste0(path_to_data, "results_final/scenarios/", input$mapcrop, "/", input$mapcrop, "_scenario_summary.RData")
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

  #make raster
  map_raster <- reactive({
    
    map_raster <- make_shock_raster_shiny(scenario_map_data(), input$mapshock, input$mapscenario)
    raster(map_raster)

  })
  
  #render output
  output$scenariomap <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(map_raster())
    
    # tm_shape(map_raster()) +
    #     tm_raster(style = "cont" , # draw gradient instead of classified
    #               palette = scenario_pal,
    #               colorNA = "white",
    #               breaks = seq(-100, 0, 25),
    #               legend.reverse = FALSE
    #               ) +
    #     tm_shape(World) +
    #     tm_borders(col = "grey") +
    #     tm_view(alpha = 1, set.view = c(30,50,2))
    
  })
  
  
  ###### Production plots ######
  
  output$productionbars <- renderPlotly({
    
    production_plot <- prod_df %>%
      mutate(across(shock, factor, levels=c("N_rate_shock","P_rate_shock","K_rate_shock",
                                            "machinery_shock", "rescaled_pesticide_sum_shock",
                                            "shock_fert_only", "shock_all"))) %>%
      mutate(across(shock, plyr::mapvalues, from = c("N_rate_shock","P_rate_shock","K_rate_shock",
                                                     "machinery_shock", "rescaled_pesticide_sum_shock",
                                                     "shock_fert_only", "shock_all"),
                    to = c("N-rate shock", "P-rate shock", "K-rate shock", "machinery shock",
                           "pesticide shock", "fertilizer shock", "all inputs shock")))%>%
      dplyr::mutate(production_decrease = ifelse(production_decrease > 0, NA, production_decrease))%>%
      ggplot(aes(x = shock, y = production_decrease, alpha = scenario_percent, fill = shock))+ #
      geom_bar(position= position_stack(reverse = T), stat = "identity")+
      #coord_cartesian(ylim= c(-0.5, 35))+
      scale_y_continuous(name = "production decrease after shock %")+
      ggtitle("Scenario shock effects on global production")+
      scale_alpha_discrete("scenario percent", range = c(1.0, 0.4))+
      scale_fill_manual(values = wes_palette("GrandBudapest1", n = 7, type = "continuous"))+
      theme_bw()+
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        #axis.text.x = element_text(angle = 90),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none"
      )+
      facet_wrap(~crop, nrow = 3)+
      guides(alpha = guide_legend(order = 2), fill = guide_legend(order = 1))
    
    ggplotly(production_plot)
    
  })
  
  
  output$productionraster <- renderLeaflet({

    prod_change_raster <- raster(paste0(path_to_data, "results_final/prod_change_raster_norm.tif"))
    #prod_change_raster <- prod_change_raster * (-1)
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(prod_change_raster)

    # tm_shape(prod_change_raster) +
    #   tm_raster(style = "cont" , # draw gradient instead of classified
    #             palette = scenario_pal,
    #             colorNA = "white",
    #             breaks = seq(0, 100, 25),
    #             legend.reverse = FALSE,
    #             ) +
    #   tm_shape(World) +
    #   tm_borders(col = "grey") +
    #   tm_view(alpha = 1, set.view = c(30,50,2),
    #           leaflet.options = ctrl_list,
    #           view.legend.position = NA)

  })
  
  output$productioncountries <- renderTmap({
    
    tm_shape(World_sp)+
      tm_polygons("prod_change",
                  style="cont",
                  palette = viridis(n = 5, option = "magma", direction = -1),
                  breaks = seq(-75, 0, 25),
                  legend.reverse = FALSE,
                  title = "decrease %")+
      tm_view(set.view = c(30,50,2))
      # tm_layout(main.title = "Change in production in all 12 crops after 50% shock in all inputs",
      #           main.title.size = 1,
      #           bg.color = "white",
      #           legend.show = TRUE,
      #           legend.outside = TRUE,
      #           #legend.outside.position = c("left", "bottom"),
      #           earth.boundary = c(-180, 180, -90, 90),
      #           earth.boundary.color = "gray",
      #           frame = FALSE)
  })
  
}

##### Run the application #####
shinyApp(ui = ui, server = server)

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

path_to_data <- "C:/Users/ahvoa1/data/results_final/"
crop <- "wheat"
load(paste0(path_to_data, "scenarios/", crop, "/", crop, "_scenario_summary.RData"))
scenario_labs <- c("25 %", "50 %", "75 %")
names(scenario_labs) <- c(25, 50, 75)

# Define UI for application that draws 
ui <- fluidPage(
  
  titlePanel("Agricultural input shocks"),
  
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
      
      ),
      
    
    column(10, plotOutput("agriplot"))
    )  

)

# Define server logic required to draw 
server <- function(input, output) {
  
  plot_data <- reactive({
    file_name <- paste0(path_to_data, "scenarios/", input$crop, "/", input$crop, "_scenario_summary.RData")
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

# Run the application 
shinyApp(ui = ui, server = server)

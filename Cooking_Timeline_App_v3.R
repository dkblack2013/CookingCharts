library(shiny)
library(lubridate)
library(DiagrammeR)
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)


# --- Input datafile

AllData <- data.frame(Project = c("Mac&Cheese", "Mac&Cheese", "Mac&Cheese", "Mac&Cheese", "Mac&Cheese", "Mac&Cheese",
                                  "Green Beans","Green Beans","Green Beans","Green Beans"),  
                      task = c("Boil 6 cups water - 5 min", "Stir in pasta - 1 min",
                               "Cook, stirring occationally - 8 min",
                               "Drain pasta, return to pan - 1 min",
                               "Squeeze cheese sauce over hot pasta, stir until blended - 5 min",
                               "Refrigerate leftovers - 1 min",
                               "Remove frozen green beans from freezer - 4 min", "Microwave for 7 minutes - 7 min",
                               "Let sit in Microwave for 2 minutes - 2 min",
                               "Remove from Microwave and enjoy - 1 min"),
                      status = c("active", "active", "active", "active", "active", "active",
                                 "active", "active", "active", "active"),
                      pos = c("first_1", "first_2", "first_3", "first_4", "first_5", "first_6",
                              "first_1_side", "first_2_side", "first_3_side", "first_4_side"),
                      start = c("tm1","after first_1", "after first_2", "after first_3", "after first_4", "after first_5",
                                "tm1","after first_1_side", "after first_2_side", "after first_3_side"),
                      end = c("5m", "1m", "8m", "1m", "5m", "1m",
                              "4m", "7m", "2m", "1m"),
                      stringsAsFactors = FALSE)


# Define UI for application

ui <- fluidPage(
  
  titlePanel("Cooking Timeline Application"),
  
  sidebarLayout(
    sidebarPanel(                       # --- setup LHS data input frames ---
      
      
      selectInput("Proj", "Project",
                  c(unique(as.character(AllData$Project)),
                    unique(as.character(df$Project)),
                    unique(as.character(df2$Project)))),
      
      
      selectInput("Stg", "Stage",
                  c("All", unique(as.character(AllData$task)))),
      
      width = 3),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Gantt Chart", DiagrammeROutput("plot")),
                  tabPanel("Data Table", tableOutput("table"))))
    
  )
)

server <- function(input, output) {
  
  # --- filter the selected project into a reactive function (access later using () suffix) ---
  SelectedProject <- reactive({dplyr::filter(AllData, Project == input$Proj)})
  
  output$plot <- renderDiagrammeR({
    mermaid(
      paste0(
        "gantt", "\n", 
        "dateFormat  YYYY-MM-DD", "\n", 
        "title Gantt Chart - Project ", input$Proj, "\n",
        
        # --- unite the first two columns (task & status) and separate them with ":" ---
        # --- then, unite the other columns and separate them with "," ---
        paste(SelectedProject() %>%
                unite(i, task, status, sep = ":") %>%
                unite(j, i, pos, start, end, sep = ",") %>%
                .$j, 
              collapse = "\n"
        ), "\n"
      )
    )
  })
  
  output$table <- renderTable({SelectedProject()})   
  
}       


# --- run application ---
shinyApp(ui = ui, server = server)
library(shiny)
library(ggplot2)
library(plyr)
library(tidyr)
library(dplyr)
library(DiagrammeR)

#App Data
df <- data.frame(task = c("Boil 6 cups water - 5 min", "Stir in pasta - 1 min", 
                          "Cook, stirring occationally - 8 min", 
                          "Drain pasta, return to pan - 1 min",
                          "Squeeze cheese sauce over hot pasta, stir until blended - 5 min", 
                          "Refrigerate leftovers - 1 min"),
                 status = c("active", "active", "active", "active", "active", "active"),
                 pos = c("first_1", "first_2", "first_3", "first_4", "first_5", "first_6"),
                 start = c("tm1","after first_1", "after first_2", "after first_3", "after first_4", "after first_5"),
                 end = c("5m", "1m", "8m", "1m", "5m", "1m"))
df2 <- data.frame(task = c("Remove frozen green beans from freezer - 4 min", "Microwave for 7 minutes - 7 min", 
                           "Let sit in Microwave for 2 minutes - 2 min", 
                           "Remove from Microwave and enjoy - 1 min"),
                  status = c("active", "active", "active", "active"),
                  pos = c("first_1_side", "first_2_side", "first_3_side", "first_4_side"),
                  start = c("after first_1","after first_1_side", "after first_2_side", "after first_3_side"),
                  end = c("4m", "7m", "2m", "1m"))

MacwSide <- mermaid(
  paste0(
    # mermaid "header", each component separated with "\n" (line break)
    "gantt", "\n", 
    "dateFormat  HH:MM:SS", "\n", 
    "title Making Dinner", "\n\n",
    "section Mac & Cheese", "\n",
    # unite the first two columns (task & status) and separate them with ":"
    # then, unite the other columns and separate them with ","
    # this will create the required mermaid "body"
    paste(df %>%
            unite(i, task, status, sep = ":") %>%
            unite(j, i, pos, start, end, sep = ",") %>%
            .$j, 
          collapse = "\n"
    ), "\n\n",
    "section Side", "\n",
    paste(df2 %>%
            unite(i, task, status, sep = ":") %>%
            unite(j, i, pos, start, end, sep = ",") %>%
            .$j, 
          collapse = "\n"
    ), "\n"
  )
)

MacnCheese <- mermaid(
  paste0(
    # mermaid "header", each component separated with "\n" (line break)
    "gantt", "\n", 
    "dateFormat  HH:MM:SS", "\n", 
    "title Making Dinner", "\n\n",
    "section Mac & Cheese", "\n",
    # unite the first two columns (task & status) and separate them with ":"
    # then, unite the other columns and separate them with ","
    # this will create the required mermaid "body"
    paste(df %>%
            unite(i, task, status, sep = ":") %>%
            unite(j, i, pos, start, end, sep = ",") %>%
            .$j, 
          collapse = "\n"
    ), "\n\n"
  )
)

GreenBeans <- mermaid(
  paste0(
    # mermaid "header", each component separated with "\n" (line break)
    "gantt", "\n", 
    "dateFormat  HH:MM:SS", "\n", 
    "title Making Dinner", "\n\n",
    "section Green Beans", "\n",
    paste(df2 %>%
            unite(i, task, status, sep = ":") %>%
            unite(j, i, pos, start, end, sep = ",") %>%
            .$j, 
          collapse = "\n"
    ), "\n"
  )
)

recipes <- list(MacnCheese, MacwSide, GreenBeans)

# Define UI for application that presents a recipe timeline
ui <- fluidPage(
   
   # Application title
   titlePanel("Cooking Timelines"),
   
   # Sidebar with a selection input for the desired recipe timeline 
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId="Recipe", label="Select a Recipe",
                    choices=c("Mac&Cheese"="1",
                              "MacwSide"="2",
                              "GreenBeans"="3"),
                    selected="2")
      ),
      
      # Show the selected timeline
      mainPanel(
         plotOutput("timeline",
                    width = "100%",
                    height = "400px")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$timeline <- renderPlot(hist(runif(500, 1, 99)))  
    # output$timeline <- renderDiagrammeR({
    # recipeValue <- input$Recipe
    # #print the selected graphic
    # recipes[recipeValue]
    #})
    #renderDiagrammeR({
    #  mermaid(
    #    paste0(input$Recipe)
    #  )  
    #})
}

# Run the application 
shinyApp(ui = ui, server = server)


## -----------------------------------------------------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)


## -----------------------------------------------------------------------------------------------
url <- "https://raw.githubusercontent.com/ChienMax/video-game-sales/refs/heads/main/vgsales.csv"
video_game_sales <- read_csv(url)

## -----------------------------------------------------------------------------------------------
video_game_sales <- video_game_sales %>%
    mutate(Year = as.numeric(Year)) %>%
    filter(!is.na(Year))


## -----------------------------------------------------------------------------------------------
ui <- fluidPage(
    titlePanel("Video Games Sales Analysis"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("region", "Choose Region:", 
                        choices = c("NA_Sales" = "NA_Sales", 
                                    "EU_Sales" = "EU_Sales", 
                                    "JP_Sales" = "JP_Sales", 
                                    "Other_Sales" = "Other_Sales", 
                                    "Global_Sales" = "Global_Sales"),
                        selected = "Global_Sales"),
            
            selectInput("genre", "Game Type:", 
                        choices = unique(video_game_sales$Genre),
                        selected = "Action"),
            
            sliderInput("yearRange", "Year interval:",
                        min = min(video_game_sales$Year, na.rm = TRUE),
                        max = min(max(video_game_sales$Year, na.rm = TRUE), 2016),
            value = c(min(video_game_sales$Year, na.rm = TRUE), 2016),
                        step = 1,
                        sep = "")
        ),
        
        mainPanel(
            plotOutput("salesPlot"),
            tableOutput("filteredData")
        )
    )
)


## -----------------------------------------------------------------------------------------------
server <- function(input, output) {
    filtered_data <- reactive({
        data <- video_game_sales %>%
            filter(Genre == input$genre) %>%
            filter(Year >= input$yearRange[1] & Year <= input$yearRange[2]) 
        
        data %>% 
            arrange(desc(!!sym(input$region))) %>%
            head(10)
    })
    
    # 绘制销量柱状图
    output$salesPlot <- renderPlot({
        ggplot(filtered_data(), aes(x = reorder(Name, !!sym(input$region)), 
                                    y = !!sym(input$region), 
                                    fill = Publisher)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(title = paste("Top 10 Sales -", input$genre, "Game (", input$yearRange[1], "-", input$yearRange[2], ")"),
                 x = "Game Name",
                 y = "Sales (Million)",
                 fill = "Publisher") +
            theme_minimal()
    })
    output$filteredData <- renderTable({
        filtered_data()
    })
}


## -----------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)


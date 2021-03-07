library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(shiny)

covid <- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"))
covid <- covid[!(covid$location=="Europe" | covid$location=="European Union" | covid$location=="Africa" | covid$location=="International" | covid$location=="Micronesia (country)" | covid$location=="North America" | covid$location=="Oceania" | covid$location=="South America"),]
covid$date <- ymd(covid$date)
covid <- subset(covid, select = -c(1:2, 5, 7:59))
covid[is.na(x = covid)] <- 0
covid <- covid %>%
    filter(new_cases > "0")
names(covid) <- c("Name", "Date", "New.cases")

shinyApp(
    
    ui <- bootstrapPage(
        
        setBackgroundColor("black"),
        
        tags$style("html, body {width:100%;height:100%}", 
                   tags$head(includeCSS("style.css"),
                             tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                                         type = "text/javascript"),
                             
                             plotlyOutput("plot", width = "100%", height = "100%"),
                             
                             absolutePanel(id = "controls", top = 10, left = "auto", right = 20, width = 200, height = "100", fixed = F, draggable = T,
                                           style = "border-radius: 8px;",
                                           
                                           uiOutput("countryOutput"))))
        
    ),
    
    server <- function(input, output) {
        output$countryOutput <- renderUI({
            selectInput("countryInput", "Choose a country:",
                        sort(unique(covid$Name)),
                        selected = "World")
        })  
        
        filtered <- reactive({
            if (is.null(input$countryInput)) {
                return(NULL)
            }    
            
            covid %>%
                filter(Name == input$countryInput
                )
        })
        
        output$plot <- renderPlotly({
            if (is.null(filtered())) {
                return()
            }
            p <- ggplot(filtered(), aes(Date, New.cases)) +
                geom_line(colour = "#00C5FF", size = 0.5) +  
                labs(x = NULL, y = "New cases") +
                ggtitle("Spread of SARS-CoV-2 in the World") +
                theme(plot.background = element_rect(fill = "black", color = NA), 
                      panel.background = element_rect(fill = "black", color = NA),
                      title = element_text(color = "white"),
                      axis.title = element_text(color = "white"),
                      axis.text = element_text(color = "white"))
            
            ggplotly(p, tooltip = c("Date", "New.cases"))
        })
        
        output$results <- renderTable({
            filtered()
        })
    },
    
    options = list(height = "500")
    
)

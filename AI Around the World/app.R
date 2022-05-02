## load packages ---------------------------------------------------------------
library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)
library(tidyverse)
library(scales)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda)
library(stopwords)
library(readr)
library(tidyverse)
library(scales)
library(ggplot2)
library(xts)
library(dygraphs)
library(DT)
library(stringr)
library(lubridate)
library(RColorBrewer)

## read data -------------------------------------------------------------------
deals_data <- read_csv("ai_data/deals.csv")
ai_country_data <- read.csv("ai_data/WorldAICompaniesTimeSeriesData.csv")
ai_news <- read_csv("ai_data/ai_news_stories.csv")
world <- map_data("world")
time_series <- read_csv("ai_data/World AI and tech companies time series .csv")
time_series <- time_series %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))
time_series$Year <- format(as.Date(time_series$Date, format ="%m/%d/%Y"), "%Y")
time_series_df <- time_series %>%
  group_by(Year)
ais = c("AI Companies" = "CompanyCount_AI", "AI Deals" = "DealCount_AI", 
        "All Tech Companies" = "Company_Count_All_Tech", "All Tech Deals" = "Deal_Count_All_Tech")




## Clean up deals_data to be in the correct format and name --------------------
deals_data$Company_count <- as.numeric(gsub(",","",deals_data$Company_count))
deals_data$Deal_count <- as.numeric(gsub(",","",deals_data$Deal_count))
deals_data$Capital_invested <- as.numeric(gsub(",","",deals_data$Capital_invested))
deals_data$Capital_invested_median <- as.numeric(gsub(",","",deals_data$Capital_invested_median)) 
deals_data$Post_valuation_median <- as.numeric(gsub(",","",deals_data$Post_valuation_median)) 
deals_data$Pre_valuation_median <- as.numeric(gsub(",","",deals_data$Pre_valuation_median)) 
deals_data$Revenue_median <- as.numeric(gsub(",","",deals_data$Revenue_median)) 
deals_data$Revenue_max <- as.numeric(gsub(",","",deals_data$Revenue_max)) 
deals_data$Percent_acquired_median <- as.numeric(gsub(",","",deals_data$Percent_acquired_median))

names(deals_data)[2] <- 'Investment_count'

## Text Mining data prep -------------------------------------------------------
#remove common characters and punctuation from  text in the title column
ai_news$title <- gsub("'","",ai_news$title)
ai_news$title <- gsub("[()]", "", ai_news$title)
ai_news$title <- gsub("&amp", "", ai_news$title)
ai_news$title <- gsub("@\\w+", "", ai_news$title)
ai_news$title <- gsub("[[:punct:]]", "", ai_news$title)
ai_news$title <- gsub("[[:digit:]]", "", ai_news$title)
ai_news$title <- gsub("http\\w+", "", ai_news$title)
ai_news$title <- gsub("[ \t]{2,}", "", ai_news$title)
ai_news$title <- gsub("^\\s+|\\s+$", "", ai_news$title) 
ai_news$title <- gsub("â","",ai_news$title)
ai_news$title <- gsub("???","",ai_news$title)
ai_news$title <- gsub("T","",ai_news$title)

#create corpus with comment text
myCorpusAI <- corpus(ai_news$title)

# Create tokens and dfm with text from title column 
myTokensAI <- tokens(myCorpusAI)
myDfmAI <- dfm(myTokensAI)

# Remove prepackaged stop words and addition special characters 
myDfmAI <- dfm_remove(myDfmAI,stopwords("english"))
myDfmAI <- dfm_remove(myDfmAI,c('.',',','t','�', '-', '?', '/', 'can', 
                                'get', 'use', ':', '5', '2018', '$'))

## World Map data prep ---------------------------------------------------------
#transform deals_data to be able to graph it on map
deals.map <- deals_data %>%
  select(Country, Investment_count, Capital_invested, Post_valuation_median) %>%
  rename(region = Country, 
         investment_count = Investment_count, 
         investment_amount = Capital_invested,
         post_valuation_median = Post_valuation_median) %>%   # Rename columns
  # Replace "United States of America" by USA in the region column
  mutate(
    region = ifelse(region == "United States", "USA", region)
  )                     

#join AI data set with world coordinate data set
deals.map <- right_join(deals.map, world, by = "region")

# create choice lists to be used in drop down menus ----------------------------
country_choices = c("Australia", "Brazil", "Canada", "China", "France", 
                    "Germany", "India", "Japan", "Kenya", 
                    "South Africa", "South Korea", 
                    "Spain", "United Kingdom", "United States")
color_scales = c("magma", "inferno", "plasma", "viridis", "cividis")
country_fill = c("investment_count", "investment_amount", "post_valuation_median")
bar_options = c("Investment_count", "Capital_invested")

# Start of shiny application ---------------------------------------------------
# Define UI for application that draws a histogram
ui <- navbarPage("The Rise of Artificial Intelligence (AI) around the World",
                 # About Tab ---------------------------------------------------
                 tabPanel("About", icon = icon("bars"),
                          fluidRow(
                            column(12,
                                   wellPanel(style = "background-color: #fff; border-color: #2c3e50;",
                                             fluidRow(
                                               column(2, tags$div(tags$img(src='AI.png', width = 190, height = 180), width = 2)),
                                               fluidRow(
                                                 column(h2("ARTIFICIAL INTELLIGENCE (AI) AROUND THE WORLD", align = "center"),
                                                        br(),
                                                        h4("The purpose of this app is to visualize the rising prevalence of Artificial Intelligence (AI) around the world. In this app you will find a multitude of visualizations that provide different perspectives of how AI investments have grown over time, and how AI investments differ by country.",
                                                           align = "center"), 
                                                        br(),
                                                        width = 9),
                                                 column(
                                                   h4("AI Companies Over Time"),
                                                   h5("On this tab you will find a line graph that visualizes the number of AI companies per country from 2000 to 2017. There is a drop-down menu that allows you to select the country you would like to view data for."),
                                                   h4("AI Headlines in 2017"),
                                                   h5("On this tab you will find a word cloud that displays the most frequently used terms when discussing artificial intelligence. There are an array of filters allowing you to customize the appearance of the word cloud."),
                                                   h4("AI Around the World in 2017"),
                                                   h5("On this tab you will find a world map that visualizes the different amounts of investment in artificial intelligence by each country in the year 2017."),
                                                   h4("Historical Data"),
                                                   h5("On this tab you will find a table of raw data allowing you to compare attributes of AI companies vs Tech companies. A drop-down menu allows you to select which year you would like to view data for."),
                                                   h4("AI Companies vs Tech Companies Over Time"),
                                                   h5("On this tab you will find a visual representation of the table from the Historical Data tab over time. A drop-down menu allows you to select which feature you would like to view."),

                                                   
                                                   width = 12)
                                               )
                                               
                                             ))))),
              
                 
                 # number of ai companies per country overtime -----------------
                 tabPanel("AI Companies Over Time",
                          fluidPage(theme = shinytheme("flatly")),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(
                            headerPanel('Apply filters'),
                            sidebarPanel(width = 3,
                                         selectInput(inputId = "ai_country", label = "Country",
                                                     choices = country_choices, selected = "United States"),
                                         submitButton("Update filters")
                            ),
                            mainPanel(
                              column(8,plotlyOutput("AICountryCount",  width = 600, height=500)))
                          )),
                 
                 # Word Cloud with news headlines for AI in 2017 ---------------
                 tabPanel("AI Headlines in 2017",
                          fluidPage(theme = shinytheme("flatly")),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(
                            headerPanel('Apply filters'),
                            sidebarPanel(width = 3,
                                         sliderInput(inputId = "maxwords", label = "Max # of words",
                                                     min = 10, max = 30, value = 15, step = 10),
                                         
                                         sliderInput(inputId = "size_largest_words", label = "Size of  words",
                                                     min = 1, max = 8, value = 4),
                                         
                                         sliderInput(inputId = "size_smallest_words", label = "Size of smallest words",
                                                     min = 0.1, max = 4, value = 0.5),
                                         submitButton("Update filters")
                            ),
                            mainPanel(
                              column(8,plotOutput("AI_cloud",  width = 600, height=500)))
                          )),
                 
                 # world map showing number of investments
                 # and amount invested in 2017 ---------------------------------
                 tabPanel("AI Around the World in 2017",
                          fluidPage(theme = shinytheme("flatly")),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(
                            headerPanel('Apply filters'),
                            sidebarPanel(width = 3,
                                         selectInput(inputId = "country_boundaries", label = "Change Country Boundary Color",
                                                     choices = c("white", "black", "grey"), 
                                                     selected = "white"),
                                         
                                         selectInput(inputId = "color_scale", label = "Change Color Scale",
                                                     choices = color_scales, 
                                                     selected = "viridis"),
                                         
                                         selectInput(inputId = "map_fill", label = "Change Map Info",
                                                     choices = country_fill, 
                                                     selected = "investment_amount"),
                                         submitButton("Update filters")
                            ),
                            mainPanel(
                              column(8,plotlyOutput("AI_map", width = 700, height=600)))
                          )),
                 # Historical Data tab input options ---------------------------
                 tabPanel("Historical Data",
                          fluidRow(
                            column(12,
                                   wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 775px;",
                                             fluidRow(style = "margin-top: 25px; margin-bottom: -10px;",
                                                      column(5, 
                                                             p(tags$b('AI Companies vs Tech Companies', style = "font-size: 150%; font-family:Helvetica; color:#4c4c4c; text-align:left;")))),
                                             hr(),
                                             fluidRow(column(5,
                                                             selectInput(inputId = 'year',
                                                                         label = NULL,
                                                                         choices = sort(unique(time_series_df$Year), decreasing = T)))),
                                             submitButton("Update filters"),
                                             DTOutput("yearly_table"))))),
                 #AI Companies vs Tech Companies Over Time ---------------------
                 tabPanel("AI  vs Tech",
                          fluidPage(theme = shinytheme("flatly")),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(
                            headerPanel('Apply filters'),
                            sidebarPanel(width = 3,
                                         selectInput(inputId = "ai", label = "Choose a filter",
                                                     choices = ais, 
                                                     selected = "CompanyCount_AI"),
                                         submitButton("Update filters")
                            ),
                            mainPanel(
                              column(8,plotlyOutput("geom", width = 600, height=500)))
                          ))
                 
)

# Define server logic required to output plots --- -----------------------------
server <- function(input, output) {
  
  # create chart to show number of AI companies over time by country -----------
  output$AICountryCount <- renderPlotly(
    {
      companies_count <- ggplot(data = ai_country_data %>% 
                                  filter(Countries == input$ai_country),
                                aes(x = Year,
                                    y = Number_of_companies)) +
        geom_line(color="#2c3e50") + 
        geom_area(fill="#2c3e50", alpha=.5) +
        theme_bw() +
        xlab("Year") + ylab("Number of AI Companies") +
        ggtitle(paste("Count of AI Companies in", input$ai_country, "Over Time")) +
        theme(plot.title = element_text(hjust = 0.5))
      ggplotly(companies_count)
    })
  
  
  # create word cloud with AI news titles from 2017 ----------------------------
  output$AI_cloud <- renderPlot(
    {
      textplot_wordcloud(
        myDfmAI,
        scale = c(input$size_smallest_words, input$size_largest_words),
        random.order = FALSE,
        max_words = input$maxwords,
        color = "#2c3e50")
    })
  
  
  # create map view of AI data for 2017 ----------------------------------------
  output$AI_map <- renderPlotly(
    {
      #plot deal count as the fill for the map
      map <- ggplot(deals.map, 
                    aes(long, lat, group = group,
                        text = paste("country:",region))) +
        geom_polygon(
          aes_string(fill = input$map_fill), 
          color = input$country_boundaries) +
        scale_fill_viridis_c(option = input$color_scale, 
                             direction = -1)  + 
        theme_bw() +
        xlab("Latitude") + ylab("Longitude") +
        ggtitle(paste(input$map_fill, "by Country in 2017 (millions of dollars)"))
      
      ggplotly(map)
    })
  
  # create table to see yearly metrics for AI and Tech companies ---------------
  y_table <- function(score_data, year) {
    
    header <- c("Date" = "Date", "AI Companies" = "CompanyCount_AI", "AI Deals" = "DealCount_AI", 
                "All Tech Companies" = "Company_Count_All_Tech", "All Tech Deals" = "Deal_Count_All_Tech")
    tbl_oi <- score_data %>%
      filter(Year == year) %>%
      select(Date, CompanyCount_AI, DealCount_AI, Company_Count_All_Tech, Deal_Count_All_Tech) %>%
      rename(!!header)
    
    return(tbl_oi)
  }
  
  
  yearly <- reactive({y_table(time_series_df,
                              year = as.numeric(input$year)) })
  
  output$yearly_table <- renderDT({ datatable(yearly(), 
                                              options = list(info = F,
                                                             paging = F,
                                                             searching = T,
                                                             stripeClasses = F, 
                                                             lengthChange = F,
                                                             scrollY = '505px',
                                                             scrollCollapse = T),
                                              rownames = F)  
  })
  
  
  # create time series lot to see yearly metrics for AI and Tech companies -----
  output$geom <- renderPlotly({ 
    p <- ggplot(data = time_series_df, aes_string(x='Date', y = input$ai))+
      geom_area(fill="#2c3e50", alpha=0.5)+
      geom_line(color="#2c3e50")+
      ylab(input$ai) +
      ggtitle("AI and Technology Trends Over Time") +
      theme()
    ggplotly(p)
  })
  
}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)
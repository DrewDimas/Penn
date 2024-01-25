library(ggplotgui)
library(rsconnect)
library(ggplot2)
library(plotly)
library(survey)
library(rgdal)
library(foreign)
library(leaflet)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(sp)
library(gridExtra)
library(DT)
library(shinythemes)
library(shiny)
library(shinydashboard)

#-------------------
# Load Data
#-------------------

philly_schools <- read.csv("Philly_schools.csv")  
philly_codes <- read_excel('philly_schools_codebook.xlsx')


#-------------------
# Data Cleanup + Data Compiling 
#-------------------
#Adding Pacific Islander field to Other due to insignificant data
philly_schools$Other <- philly_schools$Other + philly_schools$Pacific_Islander

philly_zip <- philly_schools %>%
  filter(grepl("(high|middle)", SCHOOL_LEVEL_NAME, ignore.case = TRUE)) %>%
  group_by(SCHOOL_ZIP) %>% 
  select_if(~!is.character(.) & !is.factor(.)) %>%
  summarise_all(~ round(mean(.), 2))

philly_zip <- as.data.frame(philly_zip)

philly_df <- philly_zip %>% select(., c(SCHOOL_ZIP, 
                                        Low_income_family, 
                                        Attendance, 
                                        Enrollment, 
                                        Average_salary, 
                                        African_American,
                                        White,
                                        Asian,
                                        Latino,
                                        Other,
                                        English_second_language,
                                        Drugs,
                                        Morals,
                                        Assaults,
                                        Weapons,
                                        Thefts,
                                        Total_suspensions,
                                        Special_education,
                                        Gifted_education))

philly_df_clean <- philly_df %>% rename("School Zip Code" = SCHOOL_ZIP,
                                        "Low-Income Family" = Low_income_family,
                                        "Average Teacher Salary" = Average_salary,
                                        "African American" = African_American,
                                        "Other Race" = Other,
                                        "English Second Language" = English_second_language,
                                        "Total Suspensions" = Total_suspensions,
                                        "Special Education" = Special_education,
                                        "Gifted Education" = Gifted_education)

#-------------------
# UI
#-------------------

ui <- dashboardPage(
  dashboardHeader(title = "Drew Dimas App",
                  tags$li(
                    class = "dropdown",
                    style = "padding-top: 8px; text-align: center; padding-right: 20px;",
                    actionButton("show", "Data Guide")
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", badgeLabel = icon("home"), badgeColor = "light-blue"),
      menuItem("Mapping Philly Schools", tabName = "Map", badgeLabel = icon("map"), badgeColor = "light-blue"),
      menuItem("Charts", tabName = "charts", badgeLabel = icon("line-chart"), badgeColor = "light-blue"),
      menuItem("Raw Data", tabName = "data", badgeLabel = icon("database"), badgeColor = "light-blue")
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              div(class = "text-center",
              h1("Assessing Low-Income Families in Philadelphia Middle & High Schools"),
              img(
                src = "philadelphia.jpg",
                alt = "Philly pic",
                style = "max-width: 80%;"
                ),
              h2("Created by Drew Dimas")
                )
             
              ),
      tabItem(tabName = "Map",
              h2("Mapping Low-Income Families in Philadephia Schools"),
              div(leafletOutput("map", width = "75%", height = "75vh")),
              h4("Click on regions for additional information")
               ),
      tabItem(tabName = "charts",
              h2("Interactive Charts"),
              selectInput("column_choice",
                          "Area of Interest:",
                          choices = setdiff(names(philly_df_clean), c("Low-Income Family", "School Zip Code")),
                          selected = "Enrollment"),
              plotlyOutput("comparePlot")
              ),
      tabItem(tabName = "data",
              h2("Data Table - Philadelphia Schools by Zip Code"),
              DT::dataTableOutput("table2"))
              
) #tab items
), #Dash bar
tags$head(
  tags$style(
    HTML("
        .sidebar-menu .menu-icon {
          margin-left: 20px;
        }
      ")
  )
)
) #Dashboard

#-------------------
# Server
#-------------------

server <- function(input, output) {
  
  #-------------------
  # Map 
  #-------------------
  
  zip <- readOGR(dsn="Zipcodes_Poly")
  
  zip@data <- data.frame(zip@data, philly_zip[match(zip@data$CODE, philly_zip$SCHOOL_ZIP),])
 
  zip@data$rank[is.na(zip@data$Low_income_family)] <- 0
  zip@data$rank[zip@data$Low_income_family <= 40] <- 1
  zip@data$rank[40 < zip@data$Low_income_family & zip@data$Low_income_family <= 60] <- 2
  zip@data$rank[60 < zip@data$Low_income_family & zip@data$Low_income_family <= 80] <- 3
  zip@data$rank[80 < zip@data$Low_income_family] <- 4
  
  zip@data$rank <- as.numeric(zip@data$rank)
  
  risk <- colorNumeric(c("#D6D2CF", 
                        "green",
                        "#FFEE67",
                        "orange",
                        "red"),
                      zip@data$rank)
  
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(1 * x, format = format, digits = digits, ...), "%")
  }
  
  map <- leaflet(zip) %>% 
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = TRUE, 
                color="black",
                smoothFactor = 0.2, 
                fillOpacity = 1,
                fillColor = ~risk(rank),
                weight = .6, 
                popup = paste0("<B>Zip Code: </B>", zip@data$CODE, "<br>",
                              "<br>",
                              "<B>Low Income: </B>", zip@data$Low_income_family,'%', "<br>",
                              "<B>Suspensions: </B>", zip@data$Total_suspensions, "<br>",
                              "<B>Teacher Salary: </B>", '$',zip@data$Average_salary, "<br>",
                              "<B>African American: </B>", zip@data$African_American,'%', "<br>",
                              "<B>White: </B>", zip@data$White,'%', "<br>",
                              "<B>Asian: </B>", zip@data$Asian,'%', "<br>",
                              "<B>Latino: </B>", zip@data$Latino,'%', "<br>",
                              "<B>Other Races: </B>", zip@data$Other,'%', "<br>"
                                )
                              ) %>%
    addLegend("bottomright", 
              colors = c("#D6D2CF","green", "#FFEE67", "orange", "red"),
              labels = c('No Data', "0 - 40", "41 - 60", "61 - 80", "81 - 100"),  
              title = "Low-Income: Lowest to Highest",
              opacity = 1) 
  
  # output the map
  output$map <- renderLeaflet(map)
  
  #-------------------
  # Data Table
  #-------------------
  output$table2 <- DT::renderDataTable(DT::datatable(philly_df_clean,
                                                     options = list(scrollX = TRUE)
                                                     ),
                                       rownames = FALSE)
    

  #-------------------
  # Graphs
  #-------------------
  
    output$comparePlot <-  renderPlotly({
     plot1 <- ggplotly(ggplot(data = philly_df_clean, aes(x = `Low-Income Family`, y = .data[[input$column_choice]], 
                                            text = paste("<br>Zip code: ", `School Zip Code`))) +
                        theme_linedraw() +
                        scale_y_continuous(limits = c(0, max(philly_df_clean[[input$column_choice]]))) +
                        scale_x_continuous(labels = scales::percent_format(scale = 1)) +
                        geom_point() +
                        geom_smooth()) %>%
                        layout(title = paste("Percent of Students from Low-Income Family vs", input$column_choice),
                          paper_bgcolor = "rgba(0,0,0,0)",
                          plot_bgcolor = "rgba(0,0,0,0)",
                          yaxis = list(side = "left", title = input$column_choice),
                          xaxis = list(side = "bottom", title = "Low-Income Family"),
                          margin = list(l = 100, r = 100, t = 60, b = 60)
     )
      
      plot2 <- ggplotly(
                        ggplot(data = philly_df_clean, aes(x = `Low-Income Family`, y = .data[[input$column_choice]])) +
                        theme_linedraw() +
                        scale_y_continuous(limits = c(0, max(philly_df_clean[[input$column_choice]])), position = "right") +
                        scale_x_continuous(labels = scales::percent_format(scale = 1)) +
                        geom_smooth()) %>%
                        layout(
                          paper_bgcolor = "rgba(0,0,0,0)",
                          plot_bgcolor = "rgba(0,0,0,0)",
                          yaxis = list(side = "right"),
                          font = list(size = 12),
                          showlegend = FALSE,
                          xaxis = list(side = "bottom", title = "Low-Income Family"),
                          margin = list(l = 60, r = 60, t = 60, b = 60)
                       )

      subplot(plot1, plot2, nrows = 1,  shareX = TRUE, titleY = TRUE)
  })
  
  #-------------------
  # Data Guide Button
  #-------------------
  
  observeEvent(input$show, {
    showModal(modalDialog(
      title = h2(HTML("Philadelphia Schools Data Guide")),
      h4(HTML("<B>Low-Income family:</B>"), "Percentage of student body that is from a low income family"),
      h4(HTML("<B>Attendance:</B>"), "Average percentage of student attendance"),
      h4(HTML("<B>Enrollment:</B>"), "Average enrollment"),
      h4(HTML("<B>Teacher Salary:</B>"), "Average teacher salary"),
      h4(HTML("<B>African American:</B>"), "Percentage of student body that is African American"),
      h4(HTML("<B>White:</B>"), "Percentage of student body that is White"),
      h4(HTML("<B>Asian:</B>"), "Percentage of student body that is Asian"),
      h4(HTML("<B>Latino:</B>"), "Percentage of student body that is Latino"),
      h4(HTML("<B>Other Race:</B>"), "Percentage of student body that is another race"),
      h4(HTML("<B>English Second Language:</B>"), "Percentage of student body receiving ESL services"),
      h4(HTML("<B>Drugs:</B>"), "Drug infractions per 100 students"),
      h4(HTML("<B>Morals:</B>"), "Morals infractions per 100 students"),
      h4(HTML("<B>Assaults:</B>"), "Assaults per 100 students"),
      h4(HTML("<B>Weapons:</B>"), "Weapons infractions per 100 students"),
      h4(HTML("<B>Thefts:</B>"), "Thefts per 100 students"),
      h4(HTML("<B>Total Suspensions:</B>"), "Average total suspensions at the schools per year"),
      h4(HTML("<B>Special Education:</B>"), "Percentage of student body receiving special education"),
      h4(HTML("<B>Gifted Education:</B>"), "Percentage of student body receiving gifted education"),
      easyClose = TRUE,
      footer = "Click outside of box to close"
    ))
  })
  
  

  
}  


shinyApp(ui, server)

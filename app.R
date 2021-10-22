library(shiny)

library(shinydashboard)

library(ggplot2)

library(data.table)

library(curl)

file_name <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumVaccinationCompleteCoverageByVaccinationDatePercentage&metric=newCasesBySpecimenDateRollingRate&metric=newDeaths28DaysByDeathDateRollingRate&format=csv"


df <- fread(file_name)

#read.csv(file_name,
#               header = TRUE,
#               fileEncoding = "UTF-16LE")

df$date <- as.Date(as.character(df$date))

df$areaName <- as.character(df$areaName)

df$areaCode <- as.character(df$areaCode)

df <- subset(df, date >= "2020-10-20")

#Only consider Local Authorities in England
df <- subset(df, substr(areaCode, 1, 1) == "E")

df$cumVaccinationCompleteCoverageByVaccinationDatePercentage <- as.numeric(df$cumVaccinationCompleteCoverageByVaccinationDatePercentage)
df$newCasesBySpecimenDateRollingRate <- as.numeric(df$newCasesBySpecimenDateRollingRate)
df$newDeaths28DaysByDeathDateRollingRate <- as.numeric(df$newDeaths28DaysByDeathDateRollingRate)

locations <- sort(unique(df$areaName))

maxdate <- max(df$date)




maxvac_locations <- subset(df, date == maxdate)

maxvac_value <- max(maxvac_locations$cumVaccinationCompleteCoverageByVaccinationDatePercentage)

maxvac_locations <- subset(maxvac_locations, cumVaccinationCompleteCoverageByVaccinationDatePercentage == maxvac_value)

maxvac_locations <- sort(maxvac_locations$areaName)


minvac_locations <- subset(df, date == maxdate)

minvac_value <- min(minvac_locations$cumVaccinationCompleteCoverageByVaccinationDatePercentage)

minvac_locations <- subset(minvac_locations, cumVaccinationCompleteCoverageByVaccinationDatePercentage == minvac_value)

minvac_locations <- sort(minvac_locations$areaName)






ui <- dashboardPage(
  dashboardHeader(title = "Covid-19 Dashboard"),
  skin="purple",
  
  dashboardSidebar(
    sidebarMenu(
      #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      
      selectInput(
        "location1", label = "Local Authority 1:",
        choices = c(locations), selected = minvac_locations[1]
      ),
      
      selectInput(
        "location2", label = "Local Authority 2:",
        choices = c("---", locations), selected = maxvac_locations[1]
      ),
      
      selectInput(
        "location3", label = "Local Authority 3:",
        choices = c("---", locations), selected = "---"
      ),
      
      selectInput(
        "location4", label = "Local Authority 4:",
        choices = c("---", locations), selected = "---"
      ),
      
      dateRangeInput("dates", label = "Select Date Range:",
                     start = "2020-11-01", end=maxdate,
                     min="2020-11-01", max=maxdate)
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard"#,
              #h2("Lower Tier Local Authorities"),
              #h3("(England)"),
              #h6(br())
      )
    ),
    # Boxes need to be put in a row (or column)
    
    h2("Lower Tier Local Authorities (England)"),
    h6(br()),

    
    fluidRow(
      shinydashboard::box(title =paste0("Daily Cases \n(Averaged over seven days)"),
                          plotOutput("caseplot", height = 275),
                          status = "danger"),
      shinydashboard::box(title =paste0("Daily Deaths \n(Averaged over seven days)"),
                          plotOutput("deathplot", height = 275),
                          status = "danger")
    ),
    # Boxes need to be put in a row (or column)
    fluidRow(
      shinydashboard::box(title =paste0("Percentage Fully Vaccinated"),
                          plotOutput("vaccplot", height = 275),
                          status = "danger"),
      
      shinydashboard::box(
        title = "Notes",
        paste0("The most recent date for which data is available is ", maxdate),
        br(),
        br(),
        paste0("As of ", maxdate, ":"),
        br(),
        paste0("Vaccine uptake is greatest in: ", paste(as.character(maxvac_locations), collapse="; "), "  (", as.numeric(maxvac_value), "%)"),
        br(),
        paste0("Vaccine uptake is lowest in: ", paste(as.character(minvac_locations), collapse="; "), "  (", as.numeric(minvac_value), "%)"),
        br(),
        br(),
        br(),
        br(),
        "Dashboard created by ",
        tags$a(href="https://benjburgess.github.io/", 
               "Ben Burgess",
               target="_blank"),
        br(),
        "Code to reproduce this dashboard is openly available via ",
        tags$a(href="https://github.com/benjburgess/CovidDashboard", 
               "Github"),
        br(),
        br(),
        "Data obtained from the ",
        tags$a(href="https://coronavirus.data.gov.uk/", 
               "UK Government COVID-19 Portal"),
        br(),
        "Data available under an ",
        tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", 
               "Open Government Licence v3.0"),
        status = "primary"
      )
    )
  )
)


server <- function(input, output) {
  
  output$vaccplot <- renderPlot({
    
    location_1 <- reactive(input$location1)
    
    location_2 <- reactive(input$location2)
    
    location_3 <- reactive(input$location3)
    
    location_4 <- reactive(input$location4)
    
    start_date <- reactive(input$dates[1])
    end_date <- reactive(input$dates[2])
    
    #print(location_1())
    
    location_list <- c(location_1(), 
                       location_2(), 
                       location_3(),
                       location_4())
    
    location_list <- location_list[location_list != "---"]
    
    dfplot <- subset(df, areaName %in% location_list)
    
    
    dfplot <- subset(dfplot, date >= start_date() & date <= end_date())
    
    pal_col <- c("#d60a0a", "#3457ed", "#ff9333", "#000000")
    
    line_types <- c("solid", "solid", "solid", "solid")
    
    dfplot$areaName <- factor(dfplot$areaName, levels = unique(location_list))
    
    ggplot(dfplot, aes(x=date, y=cumVaccinationCompleteCoverageByVaccinationDatePercentage,
                       color=areaName, linetype=areaName)) +
      geom_line(na.rm=TRUE) +
      ylim(c(0,100)) +
      ylab("%") +
      xlab("Date") +
      labs(subtitle = paste0(min(dfplot$date), "  to  ", max(dfplot$date))) +
      scale_color_manual(values=c(pal_col[1:length(unique(dfplot$areaName))]),
                         name="Local Authority") +
      scale_linetype_manual(values=c(line_types[1:length(unique(dfplot$areaName))]),
                         name="Local Authority") +
      theme_classic() +
      theme(legend.position = "bottom")
    
    
  })
  
  output$deathplot <- renderPlot({
    
    location_1 <- reactive(input$location1)
    
    location_2 <- reactive(input$location2)
    
    location_3 <- reactive(input$location3)
    
    location_4 <- reactive(input$location4)
    
    start_date <- reactive(input$dates[1])
    end_date <- reactive(input$dates[2])
    
    #print(location_1())
    
    location_list <- c(location_1(), 
                       location_2(), 
                       location_3(),
                       location_4())
    
    location_list <- location_list[location_list != "---"]
    
    dfplot <- subset(df, areaName %in% location_list)
    
    
    dfplot <- subset(dfplot, date >= start_date() & date <= end_date())
    
    pal_col <- c("#d60a0a", "#3457ed", "#ff9333", "#000000")
    
    line_types <- c("solid", "solid", "solid", "solid")
    
    dfplot$areaName <- factor(dfplot$areaName, levels = unique(location_list))
    
    ggplot(dfplot, aes(x=date, y=newDeaths28DaysByDeathDateRollingRate,
                       color=areaName, linetype=areaName)) +
      geom_line(na.rm=TRUE) +
      #ylim(c(0,100)) +
      ylab("Number of Deaths") +
      xlab("Date") +
      labs(subtitle = paste0(min(dfplot$date), "  to  ", max(dfplot$date))) +
      scale_color_manual(values=c(pal_col[1:length(unique(dfplot$areaName))]),
                         name="Local Authority")  +
      scale_linetype_manual(values=c(line_types[1:length(unique(dfplot$areaName))]),
                            name="Local Authority") +
      theme_classic() +
      theme(legend.position = "bottom")
    
  })
  
  output$caseplot <- renderPlot({
    
    location_1 <- reactive(input$location1)
    
    location_2 <- reactive(input$location2)
    
    location_3 <- reactive(input$location3)
    
    location_4 <- reactive(input$location4)
    
    start_date <- reactive(input$dates[1])
    end_date <- reactive(input$dates[2])
    
    #print(location_1())
    
    location_list <- c(location_1(), 
                       location_2(), 
                       location_3(),
                       location_4())
    
    location_list <- location_list[location_list != "---"]
    
    dfplot <- subset(df, areaName %in% location_list)
    
    
    dfplot <- subset(dfplot, date >= start_date() & date <= end_date())
    
    pal_col <- c("#d60a0a", "#3457ed", "#ff9333", "#000000")
    
    line_types <- c("solid", "solid", "solid", "solid")
    
    dfplot$areaName <- factor(dfplot$areaName, levels = unique(location_list))
    
    dfplot$newCasesBySpecimenDateRollingRate <- as.numeric(dfplot$newCasesBySpecimenDateRollingRate)
    
    ggplot(dfplot, aes(x=date, y=newCasesBySpecimenDateRollingRate,
                       color=areaName, linetype=areaName)) +
      geom_line(na.rm=TRUE) +
      #ylim(c(0,100)) +
      ylab("Number of Cases") +
      xlab("Date") +
      labs(subtitle = paste0(min(dfplot$date), "  to  ", max(dfplot$date))) +
      scale_color_manual(values=c(pal_col[1:length(unique(dfplot$areaName))]),
                         name="Local Authority")  +
      scale_linetype_manual(values=c(line_types[1:length(unique(dfplot$areaName))]),
                            name="Local Authority") +
      theme_classic() +
      theme(legend.position = "bottom")
    
    
  })
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

library(shiny)

library(shinydashboard)

library(ggplot2)

library(data.table)

library(curl)

file_name <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumVaccinationCompleteCoverageByVaccinationDatePercentage&metric=newCasesBySpecimenDateRollingRate&metric=newDeaths28DaysByDeathDateRollingRate&format=csv"


df <- fread(file_name)


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

maxdate_death <- max(subset(df, is.na(newDeaths28DaysByDeathDateRollingRate) == F)$date)
maxdate_cases <- max(subset(df, is.na(newCasesBySpecimenDateRollingRate) == F)$date)

date_vax_df <- subset(df, date == maxdate)

max_vax_loc <- date_vax_df[with(date_vax_df, 
                                order(-cumVaccinationCompleteCoverageByVaccinationDatePercentage,
                                      areaName))]$areaName[1:5]

min_vax_loc <- date_vax_df[with(date_vax_df, 
                                order(cumVaccinationCompleteCoverageByVaccinationDatePercentage,
                                      areaName))]$areaName[1:5]

date_death_df <- subset(df, date == maxdate_death)

max_death_loc <- date_death_df[with(date_death_df, 
                                    order(-newDeaths28DaysByDeathDateRollingRate,
                                          areaName))]$areaName[1:5]

min_death_loc <- date_death_df[with(date_death_df, 
                                    order(newDeaths28DaysByDeathDateRollingRate,
                                          areaName))]$areaName[1:5]

date_cases_df <- subset(df, date == maxdate_cases)

max_cases_loc <- date_cases_df[with(date_cases_df, 
                                    order(-newCasesBySpecimenDateRollingRate,
                                          areaName))]$areaName[1:5]

min_cases_loc <- date_cases_df[with(date_cases_df, 
                                    order(newCasesBySpecimenDateRollingRate,
                                          areaName))]$areaName[1:5]

rm(date_vax_df)
rm(date_death_df)
rm(date_cases_df)



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
        choices = c(locations), selected = "Westminster"
      ),
      
      selectInput(
        "location2", label = "Local Authority 2:",
        choices = c("---", locations), selected = "---"
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
      tabItem(tabName = "dashboard"
      )
    ),
    # Boxes need to be put in a row (or column)
    
    h2("Lower Tier Local Authorities (England)"),
    h6(br()),
    
    fluidRow(
      tabBox(
        width=12,
        tabPanel(
          tags$p("Daily Cases \n(Averaged over seven days)", style = "font-size: 200%"),
          title="Cases",
          plotOutput("caseplot"),
          fluidRow(),
          fluidRow(
            valueBoxOutput("casebox_bad", width = 6),
            valueBoxOutput("casebox_good", width = 6)
          )
        ),
        tabPanel(
          tags$p("Daily Deaths \n(Averaged over seven days)", style = "font-size: 200%"),
          title="Deaths",
          plotOutput("deathplot"),
          fluidRow(),
          fluidRow(
            valueBoxOutput("deathbox_bad", width = 6),
            valueBoxOutput("deathbox_good", width = 6)
          )
        ),
        tabPanel(
          tags$p("Percentage Fully Vaccinated", style = "font-size: 200%"),
          title="Vaccinations",
          plotOutput("vaccplot"),
          fluidRow(),
          fluidRow(
            valueBoxOutput("vaccinationbox_bad", width = 6),
            valueBoxOutput("vaccinationbox_good", width = 6)
          )
        )
      )
    ),
    
    
    
    # Boxes need to be put in a row (or column)
    fluidRow(
      shinydashboard::box(
        title = "Notes",
        width=12,
        "This dashboard illustrates how daily case, death, and vaccination rates vary amongst different",
        "Lower Tier Local Authorities. Using the selection window on the lefthand side, up to four different",
        "local authorities can be selected and the range of dates altered.",
        br(),
        br(),
        "This dashboard automatically downloads the most recently available Covid-19 data each time",
        "it is launched.",
        br(),
        br(),
        "The most recent date for which data is available is as follows:",
        br(),
        paste0("Cases (", maxdate_cases, ");"),
        br(),
        paste0("Deaths (", maxdate_death, ");"),
        br(),
        paste0("Vaccinations (", maxdate, ")"),
        br(),
        br(),
        "Dashboard created by ",
        tags$a(href="https://benjburgess.github.io/", 
               "Ben Burgess",
               target="_blank"),
        "with code to reproduce this dashboard available via ",
        tags$a(href="https://github.com/benjburgess/CovidDashboard", 
               "Github"),
        br(),
        br(),
        "Data obtained from the ",
        tags$a(href="https://coronavirus.data.gov.uk/", 
               "UK Government COVID-19 Portal"),
        "and is available under an",
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
      theme(legend.position = "bottom",
            legend.key.width= unit(1, 'cm'),
            legend.text = element_text(size=12),
            legend.title = element_text(size=14))
    
    
  })
  
  output$deathplot <- renderPlot({
    
    location_1 <- reactive(input$location1)
    
    location_2 <- reactive(input$location2)
    
    location_3 <- reactive(input$location3)
    
    location_4 <- reactive(input$location4)
    
    start_date <- reactive(input$dates[1])
    end_date <- reactive(input$dates[2])
    

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
      theme(legend.position = "bottom",
            legend.key.width= unit(1, 'cm'),
            legend.text = element_text(size=12),
            legend.title = element_text(size=14))
    
  })
  
  output$caseplot <- renderPlot({
    
    location_1 <- reactive(input$location1)
    
    location_2 <- reactive(input$location2)
    
    location_3 <- reactive(input$location3)
    
    location_4 <- reactive(input$location4)
    
    start_date <- reactive(input$dates[1])
    end_date <- reactive(input$dates[2])
    

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
      theme(legend.position = "bottom",
            legend.key.width= unit(1, 'cm'),
            legend.text = element_text(size=12),
            legend.title = element_text(size=14))
    
    
  })
  
  output$vaccinationbox_bad <- renderValueBox({
    valueBox(value = tags$p(paste0("Lowest Vaccination Rates (", maxdate, ")"), style = "font-size: 50%"), 
             paste0(min_vax_loc[1], "; ",  
                    min_vax_loc[2], "; ", 
                    min_vax_loc[3], "; ",
                    min_vax_loc[4], "; ",
                    min_vax_loc[5] 
             ), 
             icon = tags$i(class = " fas fa-exclamation-triangle", style="font-size: 50%"), color = "red")
  })
  output$vaccinationbox_good <- renderValueBox({
    valueBox(value = tags$p(paste0("Highest Vaccination Rates (", maxdate, ")"), style = "font-size: 50%"), 
             paste0(max_vax_loc[1], "; ",  
                    max_vax_loc[2], "; ", 
                    max_vax_loc[3], "; ",
                    max_vax_loc[4], "; ",
                    max_vax_loc[5] 
             ),
             color = "green")
  })
  
  output$deathbox_bad <- renderValueBox({
    valueBox(value = tags$p(paste0("Highest Death Rates (", maxdate_death, ")"), style = "font-size: 50%"),
             paste0(max_death_loc[1], "; ",  
                    max_death_loc[2], "; ", 
                    max_death_loc[3], "; ",
                    max_death_loc[4], "; ",
                    max_death_loc[5] 
             ),
             icon = tags$i(class = " fas fa-exclamation-triangle", style="font-size: 50%"), color = "red")
  })
  output$deathbox_good <- renderValueBox({
    valueBox(value = tags$p(paste0("Lowest Death Rates (", maxdate_death, ")"), style = "font-size: 50%"),
             paste0(min_death_loc[1], "; ",  
                    min_death_loc[2], "; ", 
                    min_death_loc[3], "; ",
                    min_death_loc[4], "; ",
                    min_death_loc[5] 
             ),
             color = "green")
  })
  
  output$casebox_bad <- renderValueBox({
    valueBox(value = tags$p(paste0("Highest Case Rates (", maxdate_cases, ")"), style = "font-size: 50%"),
             paste0(max_cases_loc[1], "; ",  
                    max_cases_loc[2], "; ", 
                    max_cases_loc[3], "; ",
                    max_cases_loc[4], "; ",
                    max_cases_loc[5] 
             ),
             icon = tags$i(class = " fas fa-exclamation-triangle", style="font-size: 50%"),
             color = "red")
  })
  output$casebox_good <- renderValueBox({
    valueBox(value = tags$p(paste0("Lowest Case Rates (", maxdate_cases, ")"), style = "font-size: 50%"),
             paste0(min_cases_loc[1], "; ",  
                    min_cases_loc[2], "; ", 
                    min_cases_loc[3], "; ",
                    min_cases_loc[4], "; ",
                    min_cases_loc[5] 
             ),
             color = "green")
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

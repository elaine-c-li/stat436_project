library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(usmap)
library(readr)
library(ggalt)
library(shinyWidgets)

data_applicants <- read_csv("https://uwmadison.box.com/shared/static/uo7fntpeuk22bx337cqhvn59dvv7jj6z.csv") %>%
  drop_na(major_discipline, education_level, gender, company_type, company_size) %>%
  filter(!(company_size %in% c("Oct-49", "10000+"))) %>% 
  mutate(company_size = ifelse(company_size %in% c("10/49"), "10-49", company_size)) 
  
datajob_pie <- read_csv("https://uwmadison.box.com/shared/static/50z80zegvymqwmjqu8jd7h87pd9tiak0.csv")

datajob_pie <- datajob_pie %>%
  filter(company_location == "US")

datajob_pie$experience_level <- factor(datajob_pie$experience_level, levels = c("MI", "SE", "EX", "EN"))

experience_percentages <- prop.table(table(datajob_pie$experience_level)) * 100

pie_chart_experience <- ggplot(data = NULL, aes(x = "", y = experience_percentages, fill = names(experience_percentages))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Percentage Distribution by Experience Level", fill = "Experience Level") +
  geom_text(aes(label = paste0(round(experience_percentages, 1), "%")), position = position_stack(vjust = 0.5)) 

year_percentages <- prop.table(table(datajob_pie$work_year)) * 100

pie_chart_year <- ggplot(data = NULL, aes(x = "", y = year_percentages, fill = names(year_percentages))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Percentage Distribution by Year", fill = "Year") +
  geom_text(aes(label = paste0(round(year_percentages, 1), "%")), position = position_stack(vjust = 0.5))

datajob <- read_csv("https://uwmadison.box.com/shared/static/50z80zegvymqwmjqu8jd7h87pd9tiak0.csv")

datajob = datajob %>% 
  filter(company_location == "US") %>% 
  filter(work_year == 2023)

datajob <- datajob %>%
  mutate(category = case_when(
    job_title %in% c("AI Scientist", "Applied Data Scientist", "Applied Scientist", "Data Science Consultant", 
                     "Data Science Engineer", "Data Science Lead", "Data Science Manager", "Data Science Tech Lead", 
                     "Data Scientist", "Data Scientist Lead", "Director of Data Science", "Head of Data Science", 
                     "Lead Data Scientist", "Principal Data Scientist", "Staff Data Scientist", "Head of Data") ~ "Data Science",
    job_title %in% c("AI Developer", "Applied Machine Learning Engineer", "Applied Machine Learning Scientist", 
                     "Computer Vision Engineer", "Computer Vision Software Engineer", "Deep Learning Engineer", 
                     "ML Engineer", "MLOps Engineer", "Machine Learning Developer", "Machine Learning Engineer", 
                     "Machine Learning Infrastructure Engineer", "Machine Learning Manager", "Machine Learning Researcher", 
                     "Machine Learning Scientist", "Machine Learning Software Engineer", "NLP Engineer", 
                     "Principal Machine Learning Engineer") ~ "Machine Learning",
    job_title %in% c("Analytics Engineer", "Big Data Engineer", "Cloud Data Architect", "Cloud Data Engineer", 
                     "Cloud Database Engineer", "Data Architect", "Data Engineer", "Data Infrastructure Engineer", 
                     "Data Modeler", "Data Operations Engineer", "ETL Developer", "Lead Data Engineer", 
                     "Manager Data Management", "Principal Data Engineer") ~ "Data Engineering",
    job_title %in% c("BI Analyst", "BI Data Analyst", "BI Data Engineer", "BI Developer", 
                     "Business Data Analyst", "Business Intelligence Engineer") ~ "Business Intelligence",
    job_title %in% c("Data Analyst", "Data Analytics Consultant", "Data Analytics Engineer", "Data Analytics Lead", 
                     "Data Analytics Manager", "Data Analytics Specialist", "Data Lead", "Data Manager", 
                     "Data Operations Analyst", "Data Quality Analyst", "Financial Data Analyst", 
                     "Lead Data Analyst", "Principal Data Analyst", "Product Data Analyst", "Data Specialist") ~ "Data Analytics",
    job_title %in% c("Research Engineer", "Research Scientist") ~ "Research",
    TRUE ~ "Other"
  ))

data_cost = read.csv("cost-of-living-index-by-state-2024.csv")
data_cost[52, "CostOfLivingIndex2023"] <- 100
new_column_names <- c("state", "CostOfLivingIndex", "GroceryIndex", 
                      "HealthcareIndex", "HousingIndex", "MiscIndex", 
                      "TransportationIndex", "UtilityIndex") 
colnames(data_cost) <- new_column_names

for (col in c("CostOfLivingIndex", "GroceryIndex", "HealthcareIndex", 
              "HousingIndex", "MiscIndex", "TransportationIndex", "UtilityIndex")) {
  data_cost[[col]] <- as.numeric(data_cost[[col]])
}

data_cost <- data_cost %>% 
  select(which(colSums(is.na(.)) == 0)) %>% 
  filter(state != "United States")

ui <- dashboardPage(
  dashboardHeader(title = "Data Science Jobs and Salaries"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cost of Living Analysis", tabName = "cost_of_living", icon = icon("globe")),
      menuItem("Average Salary by Experience", tabName = "salary_by_experience", icon = icon("bar-chart")),
      menuItem("Pie Charts", tabName = "pie_charts", icon = icon("chart-pie")),
      menuItem("Job Applicant Analysis", tabName = "job_applicant_analysis", icon = icon("users"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "salary_by_experience",
              fluidRow(
                box(
                  title = "Average Salary by Experience Level",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  selectInput("job_field",
                              "Job Field",
                              choices = unique(datajob$category),
                              multiple = TRUE,
                              selected = unique(datajob$category)),
                  
                  selectInput("experience",
                              "Experience Level:",
                              choices = c("Entry Level" = "EN",
                                          "Mid Level" = "MI",
                                          "Senior Level" = "SE",
                                          "Executive Level" = "EX"),
                              multiple = TRUE,
                              selected = c("EN", "MI", "SE", "EX")),
                  
                  checkboxGroupInput("company_size", 
                                     "Company Size", 
                                     choices = c("Small (<50 Employees)" = "S",
                                                 "Medium (50 to 250 Employees)" = "M",
                                                 "Large (>250 Employees)" = "L"),
                                     selected = unique(datajob$company_size)),
                  plotOutput("experienceBar")
                )
              )
      ),
      
      tabItem(tabName = "cost_of_living",
              fluidRow(
                tags$div(
                  style = "text-align: left; font-size: 20px; margin-bottom: 20px;",
                  "Compare how the cost of living as well as how six main contributing factors to the cost of living differ in each state across the U.S. 
                  When comparing costs across states, the average cost of living in the United States is used as the baseline set at 100. 
                  States are then measured against this baseline. 
                  For example, a state with a cost of living index of 200 is twice as expensive as the national average. 
                  Likewise, living in a state with an index of 50 will cost about half the national average."
                ),
                tags$div(
                  style = "text-align: left; font-size: 20px; margin-bottom: 20px;",
                  HTML("<b>Choose state(s) to consider and then one factor to compare, and optionally choose an index range for a more restrictive search</b>")
                ),
                box(
                  title = "Cost of Living Indexes in the U.S. 2024",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  pickerInput("state", 
                              "Select state(s):", 
                              choices = unique(data_cost$state), 
                              selected = unique(data_cost$state), 
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE, `live-search` = TRUE)
                  ),
                  
                  selectInput("factors", 
                              "Select what factor to consider:",
                              choices = c("Cost of Living Overall" = "CostOfLivingIndex",
                                          "Grocery Costs" = "GroceryIndex",
                                          "Healthcare Costs" = "HealthcareIndex",
                                          "Housing Costs" = "HousingIndex",
                                          "Miscellaneous Costs" = "MiscIndex",
                                          "Transportation Costs" = "TransportationIndex",
                                          "Utility Costs" = "UtilityIndex"),
                              selected = "CostOfLivingIndex"),
                  
                  sliderInput("indexRange", 
                              "Index Range:", 
                              min = 50, 
                              max = 350, 
                              value = c(50, 350)),
                  plotOutput("stateMap")
                )
              )
      ),
      
      tabItem(tabName = "job_applicant_analysis",
              fluidRow(
                box(
                  title = "Job Applicant Analysis",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  checkboxGroupInput("companyType", "Select Company Type:",
                                     choices = unique(data_applicants$company_type),
                                     selected = unique(data_applicants$company_type)),
                  checkboxGroupInput("companySize", "Select Company Size:",
                                     choices = unique(data_applicants$company_size),
                                     selected = unique(data_applicants$company_size)),
                  plotOutput("plotMajorDiscipline"),
                  plotOutput("plotEducationLevel"),
                  plotOutput("plotGender")
                )
              )
      ),
      
      tabItem(tabName = "pie_charts",
              fluidRow(
                box(
                  title = "Experience Level Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  plotOutput("pie_chart_experience")
                ),
                box(
                  title = "Year Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  plotOutput("pie_chart_year")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Tab 1: Average Salary by Experience
  filteredDataSalary <- reactive({
    datajob %>% 
      filter(category %in% input$job_field) %>%
      filter(experience_level %in% input$experience) %>%
      filter(company_size %in% input$company_size) %>%
      group_by(category, experience_level) %>%
      summarize(avg_salary = mean(salary_in_usd), .groups = 'drop') %>%
      arrange(avg_salary)
  })
  
  output$experienceBar <- renderPlot({
    filtered <- filteredDataSalary()
    
    if(nrow(filtered) == 0) {
      return(NULL)
    }
    
    ggplot(filtered, aes(x = reorder(category, avg_salary), y = avg_salary, fill = experience_level)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Job Field", y = "Average Salary (USD)", fill = "Experience Level") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = c("EN" = "blue", "MI" = "green", "SE" = "orange", "EX" = "red"),
                        labels = c("EN" = "Entry Level", "MI" = "Mid Level", "SE" = "Senior Level", "EX" = "Executive Level"))
  })
  
  # Tab 2: Cost of Living 
  filteredDataLiving <- reactive({
    if (length(input$state) == 0) {
      return(NULL)
    }
    
    filtered = data_cost %>% 
      filter(state %in% input$state) %>%
      filter(between(.data[[input$factors]], input$indexRange[1], input$indexRange[2]))
    
    if (nrow(filtered) == 0) {
      return(NULL)
    }
    
    return(filtered)
  })
  
  output$stateMap <- renderPlot({
    if (is.null(filteredDataLiving())) {
      plot_usmap(regions = "states", labels = TRUE, fill = "grey") 
    } else {
      plot_usmap(data = filteredDataLiving(), values = input$factors, regions = "states", labels = TRUE) +
        scale_fill_continuous(low = "beige", high = "blue", na.value = "grey", name = input$factors) +
        theme(legend.position = "right") +
        labs(fill = "Index Value")
    }
  })
  
  # Tab 3: Pie Charts
  output$pie_chart_experience <- renderPlot({
    print(pie_chart_experience)
  })

  output$pie_chart_year <- renderPlot({
    print(pie_chart_year)
  })
  
  # Tab 4: Job Applicant Analysis
  filteredDataApplicant <- reactive({
    data_applicants %>%
      filter(company_type %in% input$companyType, company_size %in% input$companySize)
  })
  
  output$plotMajorDiscipline <- renderPlot({
    ggplot(filteredDataApplicant(), aes(x = "", fill = major_discipline)) +
      geom_bar(width = 1, stat = "count") +
      coord_polar(theta = "y") +
      labs(fill = "Major Discipline", title = "Distribution of Major Disciplines")
  })
  
  output$plotEducationLevel <- renderPlot({
    ggplot(filteredDataApplicant(), aes(x = "", fill = education_level)) +
      geom_bar(width = 1, stat = "count") +
      coord_polar(theta = "y") +
      labs(fill = "Education Level", title = "Distribution of Education Levels")
  })
  
  output$plotGender <- renderPlot({
    ggplot(filteredDataApplicant(), aes(x = "", fill = gender)) +
      geom_bar(width = 1, stat = "count") +
      coord_polar(theta = "y") +
      labs(fill = "Gender", title = "Gender Distribution")
  })
}

shinyApp(ui = ui, server = server)

  
  




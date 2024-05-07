library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)


datajob <- read_csv("https://uwmadison.box.com/shared/static/50z80zegvymqwmjqu8jd7h87pd9tiak0.csv")

pie_chart_year <- ggplot(data = NULL, aes(x = "", y = year_percentages, fill = names(year_percentages))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      theme(legend.position = "bottom") +
      labs(title = "Percentage Distribution by Year") +
      geom_text(aes(label = paste0(round(year_percentages, 1), "%")), position = position_stack(vjust = 0.5))

pie_chart_experience <- ggplot(data = NULL, aes(x = "", y = experience_percentages, fill = names(experience_percentages))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      theme(legend.position = "bottom") +
      labs(title = "Percentage Distribution by Experience Level") +
      geom_text(aes(label = paste0(round(experience_percentages, 1), "%")), position = position_stack(vjust = 0.5))

```



```{r}

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

# Define UI for the experience bar page
experienceBarUI <- function() {
  fluidPage(
    titlePanel("Data Science Fields and Salaries by Experience Level (2023)"),
    sidebarLayout(
      sidebarPanel(
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
                    multiple = TRUE),
        
        checkboxGroupInput("company_size", 
                           "Company Size", 
                           choices = c("Small (<50 Employees)" = "S",
                                       "Medium (50 to 250 Employees)" = "M",
                                       "Large (>250 Employees)" = "L"),
                           selected = unique(datajob$company_size))
      ),
      
      mainPanel(
        plotOutput("experienceBar")
      )
    )
  )
}

# Define UI for the pie charts page
pieChartsUI <- function() {
  fluidPage(
    titlePanel("Data Science Fields and Salaries by Experience Level (2023)"),
    tabsetPanel(
      tabPanel("Pie Charts",
        plotOutput("pie_chart_experience"),
        plotOutput("pie_chart_year")
      )
    )
  )
}


# Define server logic
server <- function(input, output) {
  
  filteredData <- reactive({
    datajob %>% 
      filter(category %in% input$job_field) %>%
      filter(experience_level %in% input$experience) %>%
      filter(company_size %in% input$company_size) %>%
      group_by(category, experience_level) %>%
      summarize(avg_salary = mean(salary_in_usd), .groups = 'drop') %>% 
      arrange(avg_salary)
  })
  
  output$experienceBar <- renderPlot({
    filtered <- filteredData()
    
    if(nrow(filtered) == 0) {
      return(NULL)
    }
    
    ggplot(filtered, aes(x = category, y = avg_salary, fill = experience_level)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Job Field", y = "Average Salary (USD)", fill = "Experience Level") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_fill_manual(values = c("EN" = "blue", "MI" = "green", "SE" = "orange", "EX" = "red"),
                        labels = c("EN" = "Entry Level", "MI" = "Mid Level", "SE" = "Senior Level", "EX" = "Executive Level"))
  })
  
  # Render pie chart for experience level distribution
  output$pie_chart_experience <- renderPlot({
    geom_text(aes(label = paste0(round(experience_percentages, 1), "%")), position = position_stack(vjust = 0.5))
    pie(experience_percentages, main = "Experience Level Distribution")
  })
  
  # Render pie chart for year distribution
  output$pie_chart_year <- renderPlot({
    geom_text(aes(label = paste0(round(year_percentages, 1), "%")), position = position_stack(vjust = 0.5))
    pie(year_percentages, main = "Year Distribution")
  })
}

# Run the application 
shinyApp(ui = navbarPage("Data Science Dashboard",
                          tabPanel("Experience Bar", experienceBarUI()),
                          tabPanel("Pie Charts", pieChartsUI())),
         server = server)

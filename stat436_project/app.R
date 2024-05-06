library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)


datajob <- read_csv("https://uwmadison.box.com/shared/static/50z80zegvymqwmjqu8jd7h87pd9tiak0.csv")

datajob <- datajob %>%
  filter(company_location == "US")

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

ui <- fluidPage(
  
  titlePanel("Data Science Jobs and Salaries by Experience Level"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("experience",
                  "Experience Level:",
                  choices = c("Entry Level" = "EN",
                              "Mid Level" = "MI",
                              "Senior Level" = "SE",
                              "Executive Level" = "EX"),
                  multiple = TRUE),
    ),
    
    selectInput("year",
    )
    
    mainPanel(
      plotOutput("experienceBar")
    )
  )
)

server <- function(input, output) {
  
  
}


shinyApp(ui = ui, server = server)

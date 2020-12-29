# Exploring the time series median income from US Census (Summary Table):
# Table P-8: https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-income-people.html

# Data cleaned and formatted in Excel
# 2013 & 2017 have different series. As such we're selecting the one with the most number of observations
# for the 15+ (All Ages) group

library(shiny)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(zoo)
library(plotly)
library(shinythemes)
#library(shinycssloaders)

# Load data:
app_data <- read_csv("./med_inc_data.csv")
gap_data <- read_csv("./gender_gap_data.csv")

# Define UI 
ui <- fluidPage(theme = shinytheme("paper"),

    # Application title
    titlePanel("Comparing Median Incomes by Age Group and Sex"),
    
    # Short Introduction
    p("Developed by Lucas De Oliveira | ", a(href = "https://lbdeoliveira.com", "Website")),
    br(),
    p("Visualize trends in real median income (2019 USD) by sex and age group. Track changes in gender pay gaps by age group as a difference and ratio. For a summary of trends, ", a(href = "https://deoliveiralb.medium.com", "click here!")),
    p("Data from the ", a(href = "https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-income-people.html", "US Census Bureau.")),
    p("Find the source code for this app on", a(href = "https://github.com/lbdeoliveira/app_median_income_sex_age", "my Github.")),
    br(),
    # Tab Panel
    tabsetPanel(
        tabPanel(title = "Comparing Income across Age Groups",
            br(),
            sidebarPanel(
                br(),
                checkboxGroupInput("age_gps", "Select Age Groups for Comparison:", choices = unique(app_data$Age), selected = "15+"),
                checkboxInput("norm_to_last", "Normalize Incomes to Last Observation (2019)?")
            ),
            mainPanel(
                br(),
                #h2("Male Real Median Incomes (in 2019 USD) by Age Group"),
                plotlyOutput("male_age_plot"),
                br(),
                #h2("Female Real Median Incomes (in 2019 USD) by Age Group"),
                plotlyOutput("female_age_plot"),
                br()
            )
        ),
        tabPanel(title = "Comparing Income across Sexes",
            br(),
            fluidRow(
                sidebarPanel(
                    selectInput("gap_measure", "Measure Gender Gap as a Difference or Ratio?", choices = c("Difference", "Ratio")),
                    br(),
                    checkboxGroupInput("gap_age_gps", "Select Age Groups", choices = unique(gap_data$Age), selected = "15+")
                ),
                mainPanel(
                    plotlyOutput("gap_plot")
                ) 
            ),
            fluidRow(
                sidebarPanel(
                    selectInput("sex_tab_age_groups", "Select Age Group:", choices = unique(app_data$Age)), 
                    br(),
                    checkboxInput("sexes_norm_to_last", "Normalize Incomes to Last Observation (2019)?"),
                ),
                mainPanel(
                    plotlyOutput("plot_sex") 
                )
            )
        )
    )
)

# Define server logic 
server <- function(input, output) {
    
    # Server Logic for Age Comparison Tab ------------------------------------- 
    plot_male_age <- reactive(
        
        if (input$norm_to_last == TRUE) {
            
            ggplot(filter(app_data, Sex == "Male", Age %in% input$age_gps),
                   aes(x = Year,
                       y = norm_last,
                       color = Age))+
                geom_line()+
                ggtitle("Male Median Real Wages by Age Group")+
                ylab("Normalized Wage (100 = 2019 Wage)")
            
            
        } else {
            
            ggplot(filter(app_data, Sex == "Male", Age %in% input$age_gps),
                   aes(x = Year,
                       y = median_2019,
                       color = Age))+
                geom_line()+
                ggtitle("Male Median Real Wages by Age Group")+
                ylab("Real Median Wage (2019)")
            
        }
        
    )
    
    plot_female_age <- reactive(
        
        if (input$norm_to_last == TRUE) {
            
            ggplot(filter(app_data, Sex == "Female", Age %in% input$age_gps),
                   aes(x = Year,
                       y = norm_last,
                       color = Age))+
                geom_line()+
                ggtitle("Female Median Real Wages by Age Group")+
                ylab("Normalized Wage (100 = 2019 Wage)")
            
            
        } else {
            
            ggplot(filter(app_data, Sex == "Female", Age %in% input$age_gps),
                   aes(x = Year,
                       y = median_2019,
                       color = Age))+
                geom_line()+
                ggtitle("Female Median Real Wages by Age Group")+
                ylab("Real Median Wage (2019 USD)")
            
        }
        
    )
    
    output$male_age_plot <- renderPlotly({
        plot_male_age()
    })
    
    output$female_age_plot <- renderPlotly({
        plot_female_age()
    })
    
    # Server Logic for Sex Comparison Tab -------------------------------------
    plot_sex <- reactive(
        if (input$sexes_norm_to_last == TRUE) {
            ggplot(filter(app_data, Age == input$sex_tab_age_groups),
                   aes(x = Year,
                       y = norm_last,
                       color = Sex))+
                geom_line()+
                ggtitle("Male and Female Real Median Wages")+
                ylab("Normalized Wage (100 = 2019 Wage)")
        } else {
            ggplot(filter(app_data, Age == input$sex_tab_age_groups),
                   aes(x = Year,
                       y = median_2019,
                       color = Sex))+
                geom_line()+
                ggtitle("Male and Female Real Median Wages")+
                ylab("Real Median Wage (2019 USD)")
        }
    )
    
    output$plot_sex <- renderPlotly({
        plot_sex()
    })
    
    # Server Logic for Gender Gap ---------------------------------------------
    plot_gender_gap <- reactive(
        if (input$gap_measure == "Difference") {
            ggplot(filter(gap_data, Age %in% input$gap_age_gps),
                   aes(x = Year,
                       y = M_minus_F,
                       color = Age))+
                geom_line()+
                ggtitle("Male-Female Real Median Wage Gap (Difference)")+
                ylab("Male Wages - Female Wages")
        } else {
            ggplot(filter(gap_data, Age %in% input$gap_age_gps),
                   aes(x = Year,
                       y = MF_ratio,
                       color = Age))+
                geom_line()+
                ggtitle("Male-Female Real Median Wage Gap (Ratio)")+
                ylab("Male Wages / Female Wages")
        }
    )
    
    output$gap_plot <- renderPlotly({
        plot_gender_gap()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

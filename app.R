#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(here)
library(dplyr)
library(stringr)


source('Data_cleaning_2902.r')

num_list <- list(
  age = cleaned_age,
  weetbix_count = cleaned_weetbix_count,
  weekly_food_spend = cleaned_food_spend,
  weekly_alcohol = cleaned_weekly_alcohol,
  weekly_study_hours = cleaned_weekly_study_hours,
  height = cleaned_height,
  sleep_duration = sleep_duration,
  random_number = cleaned_random_number,
  sleep_schedule = cleaned_sleep_schedule,
  favourite_number = cleaned_favourite_number,
  weekly_exercise_hours = cleaned_weekly_exercise_hours,
  weekly_paid_work_hours = cleaned_weekly_paid_work_hours,
  daily_water_intake_l = cleaned_daily_water_intake_l,
  perceived_old_age = cleaned_perceived_old_age,
  wam = cleaned_wam
)

# Create a list of categorical variables
cat_list <- list(
  target_grade = cleaned_target_grade,
  assignment = cleaned_assignment,
  trimester_or_semester = cleaned_trimester_or_semester,
  tendency_yes_or_no = cleaned_tendency_yes_or_no,
  pay_rent = cleaned_pay_rent,
  stall_choice = cleaned_stall_choice,
  living_arrangements = cleaned_living_arrangements,
  believe_in_aliens = cleaned_believe_in_aliens,
  daily_anxiety_frequency = cleaned_daily_anxiety_frequency,
  gender = cleaned_gender,
  usual_bedtime = cleaned_usual_bedtime,
  sibling_count = cleaned_sibling_count,
  allergy_count = cleaned_allergy_count,
  diet_style = cleaned_diet_style,
  drivers_license = cleaned_drivers_license,
  relationship_status = cleaned_relationship_status,
  steak_preference = cleaned_steak_preference,
  dominant_hand = cleaned_dominant_hand,
  enrolled_unit = cleaned_enrolled_unit,
  assignments_on_time = cleaned_assignments_on_time,
  used_r_before = cleaned_used_r_before,
  team_role_type = cleaned_team_role_type,
  university_year = cleaned_university_year,
  fluent_languages = cleaned_fluent_languages,
  readable_languages = cleaned_readable_languages
)

cleaned_data_list <- list(
  numeric = num_list,
  categorical = cat_list
)

combined_var_names <- c(names(cleaned_data_list$numeric), names(cleaned_data_list$categorical))






ui <- fluidPage(
  titlePanel("Variable Selector"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown for selecting any variable (numeric or categorical)
      selectInput("var1", 
                  "Select any Variable (Numeric or Categorical):", 
                  choices = combined_var_names,
                  selected = combined_var_names[1]),
      
      # Dropdown for selecting a categorical variable only
      selectInput("var2",
                  "Select a Categorical Variable:",
                  choices = names(cleaned_data_list$categorical),
                  selected = names(cleaned_data_list$categorical)[1])
    ),
    mainPanel(
      h3("Selected Variables"),
      verbatimTextOutput("selected_vars")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  output$selected_vars <- renderPrint({
    list(
      Selected_Any_Variable = input$var1,
      Selected_Categorical_Variable = input$var2
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
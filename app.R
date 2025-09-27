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

# Define UI
ui <- fluidPage(
  titlePanel("Variable Selector and Statistical Tests"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select any variable (numeric or categorical)
      selectInput("var1", 
                  "Select any Variable (Numeric or Categorical):", 
                  choices = c("Select a variable" = "", combined_var_names),
                  selected = ""),
      
      # Dropdown to select a categorical variable only
      selectInput("var2",
                  "Select a Categorical Variable:",
                  choices = c("Select a variable" = "", names(cleaned_data_list$categorical)),
                  selected = "")
    ),
    mainPanel(
      h3("Current Selections"),
      verbatimTextOutput("selected_vars"),
      
      # Show analysis results
      uiOutput("analysis_output")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Print the selected variable names
  output$selected_vars <- renderPrint({
    list(
      Selected_Any_Variable = if(input$var1 == "") "None selected" else input$var1,
      Selected_Categorical_Variable = if(input$var2 == "") "None selected" else input$var2
    )
  })
  
  # Determine which test to show and generate appropriate output
  output$analysis_output <- renderUI({
    
    # Check if both variables are selected
    if (input$var1 == "" || input$var2 == "") {
      return(tagList(
        h3("No Test Available"),
        p("Please select both variables to perform statistical analysis.")
      ))
    }
    
    # Check if first variable is categorical (Chi-square test)
    if (input$var1 %in% names(cleaned_data_list$categorical)) {
      return(tagList(
        h3("Chi-Square Test Summary"),
        verbatimTextOutput("chiSummary"),
        h3("Mosaic Plot Visualization"),
        plotOutput("chiPlot")
      ))
    }
    
    # Check if first variable is numeric (T-test)
    if (input$var1 %in% names(cleaned_data_list$numeric)) {
      return(tagList(
        h3("Two-Sample T-Test Summary"),
        verbatimTextOutput("tTestSummary"),
        h3("Box Plot Visualization"),
        plotOutput("tTestPlot")
      ))
    }
    
    # Default case
    return(tagList(
      h3("No Test Available"),
      p("No appropriate statistical test available for the selected variable combination.")
    ))
  })
  
  # CHI-SQUARE TEST COMPONENTS
  
  # Reactive: Retrieve the first variable if it is categorical
  selected_cat1 <- reactive({
    if (input$var1 %in% names(cleaned_data_list$categorical)) {
      return(cleaned_data_list$categorical[[input$var1]])
    } else {
      return(NULL)
    }
  })
  
  # Since var2 is always categorical, simply retrieve it.
  selected_cat2 <- reactive({
    if (input$var2 != "") {
      return(cleaned_data_list$categorical[[input$var2]])
    } else {
      return(NULL)
    }
  })
  
  # Combine the two categorical variables into a data frame
  df_pair_cat <- reactive({
    vec1 <- selected_cat1()
    vec2 <- selected_cat2()
    
    if (is.null(vec1) || is.null(vec2)) return(NULL)
    
    n_min <- min(length(vec1), length(vec2))
    if(length(vec1) != length(vec2)) {
      message("Lengths differ: using first ", n_min, " observations from each variable.")
    }
    data.frame(cat1 = vec1[1:n_min],
               cat2 = vec2[1:n_min],
               stringsAsFactors = TRUE)
  })
  
  # Create a contingency table from the data frame
  contingency_table <- reactive({
    df <- df_pair_cat()
    if (is.null(df)) return(NULL)
    table(df$cat1, df$cat2)
  })
  
  # Perform a chi-square test on the contingency table if valid
  chi_test <- reactive({
    tbl <- contingency_table()
    if (is.null(tbl)) return(NULL)
    if (all(dim(tbl) > 1)) {
      test <- chisq.test(tbl)
      return(test)
    } else {
      return(NULL)
    }
  })
  
  # Output the chi-square test summary
  output$chiSummary <- renderPrint({
    if (is.null(chi_test())) {
      cat("Chi-Square Test cannot be performed.\n")
      cat("Ensure that both selected variables are categorical and that there are at least 2 levels per variable.\n")
    } else {
      print(chi_test())
    }
  })
  
  # Output the mosaic plot of the contingency table
  output$chiPlot <- renderPlot({
    tbl <- contingency_table()
    if (is.null(tbl)) {
      plot.new()
      text(0.5, 0.5, "No mosaic plot available.\nSelect two categorical variables.")
    } else {
      mosaicplot(tbl, main = "Mosaic Plot", color = TRUE)
    }
  })
  
  # T-TEST COMPONENTS
  
  # Get numeric variable
  selected_num <- reactive({
    if (input$var1 %in% names(cleaned_data_list$numeric)) {
      return(cleaned_data_list$numeric[[input$var1]])
    } else {
      return(NULL)
    }
  })
  
  # Combine numeric and categorical variables into a data frame
  df_pair_t <- reactive({
    num_vec <- selected_num()
    cat_vec <- selected_cat2()
    
    if (is.null(num_vec) || is.null(cat_vec)) return(NULL)
    
    n_min <- min(length(num_vec), length(cat_vec))
    if(length(num_vec) != length(cat_vec)) {
      message("Lengths differ: using first ", n_min, " observations from each variable.")
    }
    
    data.frame(numeric_var = num_vec[1:n_min],
               categorical_var = as.factor(cat_vec[1:n_min]),
               stringsAsFactors = TRUE)
  })
  
  # Perform t-test
  t_test_result <- reactive({
    df <- df_pair_t()
    if (is.null(df)) return(NULL)
    
    # Remove NA values
    df_complete <- df[complete.cases(df), ]
    if (nrow(df_complete) == 0) return(NULL)
    
    # Check if categorical variable has exactly 2 levels
    if (length(levels(df_complete$categorical_var)) != 2) {
      return("error_not_two_levels")
    }
    
    # Perform t-test
    tryCatch({
      t.test(numeric_var ~ categorical_var, data = df_complete)
    }, error = function(e) {
      return(paste("Error in t-test:", e$message))
    })
  })
  
  # Output t-test summary
  output$tTestSummary <- renderPrint({
    result <- t_test_result()
    if (is.null(result)) {
      cat("T-test cannot be performed.\n")
      cat("Please ensure both variables are selected and contain valid data.\n")
    } else if (is.character(result) && result == "error_not_two_levels") {
      cat("T-test cannot be performed.\n")
      cat("The categorical variable must have exactly 2 levels for a two-sample t-test.\n")
      df <- df_pair_t()
      if (!is.null(df)) {
        cat("Current levels:", paste(levels(df$categorical_var), collapse = ", "), "\n")
        cat("Number of levels:", length(levels(df$categorical_var)), "\n")
      }
    } else if (is.character(result)) {
      cat(result, "\n")
    } else {
      print(result)
      
      # Add interpretation
      cat("\n--- Interpretation ---\n")
      if (result$p.value < 0.05) {
        cat("Result: Statistically significant difference (p < 0.05)\n")
        cat("Conclusion: There is evidence of a difference in means between the two groups.\n")
      } else {
        cat("Result: No statistically significant difference (p >= 0.05)\n")
        cat("Conclusion: There is insufficient evidence of a difference in means between the two groups.\n")
      }
    }
  })
  
  # Output box plot for t-test
  output$tTestPlot <- renderPlot({
    df <- df_pair_t()
    if (is.null(df)) {
      plot.new()
      text(0.5, 0.5, "No box plot available.\nSelect a numeric and categorical variable.")
      return()
    }
    
    # Remove NA values
    df_complete <- df[complete.cases(df), ]
    if (nrow(df_complete) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available after removing missing values.")
      return()
    }
    
    if (length(levels(df_complete$categorical_var)) != 2) {
      plot.new()
      text(0.5, 0.5, paste("Box plot not available.\nCategorical variable must have exactly 2 levels.\nCurrent levels:", 
                           length(levels(df_complete$categorical_var))))
    } else {
      boxplot(numeric_var ~ categorical_var, 
              data = df_complete,
              main = paste("Box Plot:", input$var1, "by", input$var2),
              xlab = input$var2,
              ylab = input$var1,
              col = c("lightblue", "lightcoral"))
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
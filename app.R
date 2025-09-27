#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here:
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(here)
library(dplyr)
library(stringr)

source('Data_cleaning_2902.r')

# Define the list of numeric variables
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

# Define the list of categorical variables
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

combined_var_names <- c(names(cleaned_data_list$numeric),
                        names(cleaned_data_list$categorical))

ui <- fluidPage(
  titlePanel("Variable Selector and Statistical Tests"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select any variable (numeric or categorical)
      selectInput("var1", 
                  "Select any Variable (Numeric or Categorical):", 
                  choices = c("Select a variable" = "", combined_var_names),
                  selected = ""),
      
      # Dropdown to select a categorical variable (grouping variable)
      selectInput("var2",
                  "Select a Grouping (Categorical) Variable:",
                  choices = c("Select a variable" = "", names(cleaned_data_list$categorical)),
                  selected = ""),
      
      # Dynamic UI for level selection when the grouping variable has >2 levels.
      uiOutput("levelSelectors")
    ),
    mainPanel(
      h3("Current Selections"),
      verbatimTextOutput("selected_vars"),
      uiOutput("analysis_output")
    )
  )
)

server <- function(input, output, session) {
  
  # Print the currently selected variables
  output$selected_vars <- renderPrint({
    list(
      Selected_Var1 = if(input$var1 == "") "None selected" else input$var1,
      Selected_Grouping_Variable = if(input$var2 == "") "None selected" else input$var2
    )
  })
  
  # Dynamically create level selectors for a grouping variable with >2 levels
  output$levelSelectors <- renderUI({
    # Only applicable when var1 (numeric) and var2 (categorical) are selected
    if (input$var1 != "" && input$var2 != "" && (input$var1 %in% names(cleaned_data_list$numeric))) {
      grp_vec <- cleaned_data_list$categorical[[input$var2]]
      if (!is.null(grp_vec)) {
        df_temp <- data.frame(group = as.factor(grp_vec))
        lvl <- levels(df_temp$group)
        if (length(lvl) > 2) {
          tagList(
            selectInput("selectedLevel1", "Select First Level:",
                        choices = lvl, selected = lvl[1]),
            selectInput("selectedLevel2", "Select Second Level:",
                        choices = lvl, selected = lvl[2])
          )
        }
      }
    }
  })
  
  # analysis_output selects which test to perform and what UI to show
  output$analysis_output <- renderUI({
    
    # Check if both variables are selected
    if (input$var1 == "" || input$var2 == "") {
      return(tagList(
        h3("No Test Available"),
        p("Please select both variables to perform statistical analysis.")
      ))
    }
    
    # If var1 is categorical --> Chi-square test components as before
    if (input$var1 %in% names(cleaned_data_list$categorical)) {
      return(tagList(
        h3("Chi-Square Test Summary"),
        verbatimTextOutput("chiSummary"),
        h3("Mosaic Plot Visualization"),
        plotOutput("chiPlot")
      ))
    }
    
    # If var1 is numeric --> Independent two-sample test (t-test or ANOVA)
    if (input$var1 %in% names(cleaned_data_list$numeric)) {
      return(tagList(
        h3("Two-Sample T-Test / ANOVA Summary"),
        verbatimTextOutput("tTestSummary"),
        h3("Boxplot Visualization"),
        plotOutput("tTestPlot")
      ))
    }
    
    # Fallback if none of the above
    return(tagList(
      h3("No Test Available"),
      p("No appropriate statistical test available for the selected variable combination.")
    ))
  })
  
  # ========================
  # CHI-SQUARE TEST COMPONENTS
  # ========================
  
  # Get the first variable from categorical list if var1 is categorical.
  selected_cat1 <- reactive({
    if (input$var1 %in% names(cleaned_data_list$categorical)) {
      return(cleaned_data_list$categorical[[input$var1]])
    } else {
      return(NULL)
    }
  })
  
  # Since var2 is always categorical, retrieve it.
  selected_cat2 <- reactive({
    if (input$var2 != "") {
      return(cleaned_data_list$categorical[[input$var2]])
    } else {
      return(NULL)
    }
  })
  
  # Build a data frame for two categorical variables
  df_pair_cat <- reactive({
    vec1 <- selected_cat1()
    vec2 <- selected_cat2()
    
    if (is.null(vec1) || is.null(vec2)) return(NULL)
    n_common <- min(length(vec1), length(vec2))
    data.frame(cat1 = vec1[1:n_common],
               cat2 = vec2[1:n_common],
               stringsAsFactors = TRUE)
  })
  
  # Contingency table for chi-square test
  contingency_table <- reactive({
    df <- df_pair_cat()
    if (is.null(df)) return(NULL)
    table(df$cat1, df$cat2)
  })
  
  # Chi-square test
  chi_test <- reactive({
    tbl <- contingency_table()
    if (is.null(tbl)) return(NULL)
    if (all(dim(tbl) > 1)) {
      chisq.test(tbl)
    } else {
      NULL
    }
  })
  
  output$chiSummary <- renderPrint({
    if (is.null(chi_test())) {
      cat("Chi-Square Test cannot be performed.\n")
      cat("Ensure that both selected variables are categorical and each has at least 2 levels.\n")
    } else {
      print(chi_test())
    }
  })
  
  output$chiPlot <- renderPlot({
    tbl <- contingency_table()
    if (is.null(tbl)) {
      plot.new()
      text(0.5, 0.5, "No mosaic plot available.\nSelect two categorical variables with at least 2 levels each.")
    } else {
      mosaicplot(tbl, main = "Mosaic Plot", color = TRUE)
    }
  })
  
  # ========================
  # T-TEST / ANOVA COMPONENTS (Numeric vs. Categorical)
  # ========================
  
  # Get numeric variable for var1
  selected_num <- reactive({
    if (input$var1 %in% names(cleaned_data_list$numeric)) {
      return(cleaned_data_list$numeric[[input$var1]])
    } else {
      return(NULL)
    }
  })
  
  # Build a data frame that combines the numeric variable and the grouping variable (var2)
  df_pair_t <- reactive({
    num_vec <- selected_num()
    cat_vec <- cleaned_data_list$categorical[[input$var2]]
    
    if (is.null(num_vec) || is.null(cat_vec)) return(NULL)
    
    n_common <- min(length(num_vec), length(cat_vec))
    df <- data.frame(numeric_var = num_vec[1:n_common],
                     categorical_var = as.factor(cat_vec[1:n_common]),
                     stringsAsFactors = TRUE)
    df <- df[complete.cases(df), ]
    
    # If there are more than 2 groups, check if dynamic level selection is available;
    # if yes, filter to only these two levels.
    if(nlevels(df$categorical_var) > 2) {
      if(!is.null(input$selectedLevel1) && !is.null(input$selectedLevel2)) {
        df <- df %>% filter(categorical_var %in% c(input$selectedLevel1, input$selectedLevel2))
        df$categorical_var <- droplevels(df$categorical_var)
      }
    }
    return(df)
  })
  
  # Run t-test (or ANOVA if more than 2 groups remain)
  t_test_result <- reactive({
    df <- df_pair_t()
    if (is.null(df) || nrow(df) < 2) return("Not enough data to perform the test.")
    
    groups <- levels(df$categorical_var)
    if(length(groups) < 2) {
      return("At least two groups are needed for comparison.")
    } else if(length(groups) == 2) {
      # Independent two-sample t-test (Welch's by default)
      tryCatch({
        t.test(numeric_var ~ categorical_var, data = df)
      }, error = function(e) {
        paste("Error in t-test:", e$message)
      })
    } else {
      # More than 2 groups: perform ANOVA.
      fit <- aov(numeric_var ~ categorical_var, data = df)
      summary(fit)
    }
  })
  
  output$tTestSummary <- renderPrint({
    res <- t_test_result()
    if (is.null(res)) {
      cat("Test cannot be performed.\nEnsure that both variables contain valid data.")
    } else {
      if (is.character(res)) {
        cat(res)
      } else {
        print(res)
        # Simple interpretation
        if(is.list(res) && !is.null(res$p.value)) {
          cat("\n--- Interpretation ---\n")
          if (res$p.value < 0.05) {
            cat("Statistically significant difference (p < 0.05).\n")
          } else {
            cat("No statistically significant difference (p >= 0.05).\n")
          }
        }
      }
    }
  })
  
  output$tTestPlot <- renderPlot({
    df <- df_pair_t()
    if (is.null(df) || nrow(df) < 1) {
      plot.new()
      text(0.5, 0.5, "No box plot available.\nSelect an appropriate numeric and categorical variable.")
    } else {
      boxplot(numeric_var ~ categorical_var, data = df,
              main = paste("Box Plot:", input$var1, "by", input$var2),
              xlab = input$var2,
              ylab = input$var1,
              col = c("lightblue", "lightcoral"))
    }
  })
}

shinyApp(ui = ui, server = server)
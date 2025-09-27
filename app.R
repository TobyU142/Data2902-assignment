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
  trimester_or_semester = cleaned_trimester_or_semester,
  target_grade = cleaned_target_grade,
  assignment_preference = cleaned_assignment,
  work_status = cleaned_work_status,
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
      
      # Dynamic assumptions checker
      uiOutput("assumptions_check"),
      
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
  
  # Dynamic assumption checker
  output$assumptions_check <- renderUI({
    if (input$var1 == "" || input$var2 == "") {
      return(NULL)
    }
    
    # Show assumptions for both chi-square and t-test
    if (input$var1 %in% names(cleaned_data_list$categorical)) {
      tagList(
        h3("Chi-Square Test Assumptions"),
        verbatimTextOutput("chi_assumptions")
      )
    } else if (input$var1 %in% names(cleaned_data_list$numeric)) {
      tagList(
        h3("T-Test Assumptions"),
        verbatimTextOutput("t_test_assumptions")
      )
    }
  })
  
  # Chi-square assumptions checker - (ALL frequencies ≥ 5)
  output$chi_assumptions <- renderText({
    # Use the same chi-square result that the actual test uses
    chi_result <- chi_test()
    if (is.null(chi_result)) {
      return("Cannot check assumptions - chi-square test cannot be performed.")
    }
    
    expected_freq <- chi_result$expected
    
    min_expected <- min(expected_freq)
    violations <- sum(expected_freq < 5)
    total_cells <- length(expected_freq)
    
    assumption_text <- paste(
      "Chi-Square Test Assumptions:",
      paste("- ALL expected frequencies must be ≥ 5 (np ≥ 5 and n(1-p) ≥ 5)"),
      paste("- This ensures the chi-square statistic follows the chi-square distribution"),
      "",
      "Results:",
      paste("- Minimum expected frequency:", round(min_expected, 2)),
      paste("- Cells with expected frequency < 5:", violations, "out of", total_cells),
      sep = "\n"
    )
    
    # Show individual expected frequencies for transparency
    assumption_text <- paste(assumption_text,
                             "\nExpected frequencies table:",
                             sep = "\n")
    
    # Add expected frequencies display
    expected_display <- capture.output(print(round(expected_freq, 2)))
    assumption_text <- paste(assumption_text,
                             paste(expected_display, collapse = "\n"),
                             sep = "\n")
    
    # Evaluation - must have ALL cells ≥ 5
    if (violations == 0) {
      assumption_text <- paste(assumption_text, 
                               "\n✓ ASSUMPTION MET: All expected frequencies ≥ 5", 
                               "\nChi-square test is valid and reliable.",
                               sep = "\n")
    } else {
      assumption_text <- paste(assumption_text, 
                               "\n✗ ASSUMPTION VIOLATED: Some expected frequencies < 5", 
                               paste("\nNumber of violating cells:", violations),
                               "\nRecommendations:",
                               "- Use Fisher's exact test instead",
                               "- Combine categories if theoretically justified",
                               "- Collect more data",
                               "\n⚠ WARNING: Chi-square results may be unreliable!",
                               sep = "\n")
    }
    
    return(assumption_text)
  })
  
  # T-test assumptions checker 
  output$t_test_assumptions <- renderText({
    df <- df_pair_t()
    if (is.null(df) || nrow(df) < 2) {
      return("Cannot check assumptions - insufficient data available.")
    }
    
    groups <- levels(df$categorical_var)
    if (length(groups) != 2) {
      return("Strict assumption checking only available for two-group comparisons.")
    }
    
    # Sample sizes for each group
    group_sizes <- table(df$categorical_var)
    total_n <- sum(group_sizes)
    
    assumption_text <- paste(
      "Two-Sample T-Test Assumptions:",
      "",
      "1. NORMALITY: Data in each group must be approximately normally distributed",
      "2. INDEPENDENCE: Observations must be independent",
      "3. EQUAL VARIANCES: Groups must have equal population variances (for pooled t-test)",
      "",
      "Sample Information:",
      paste("- Group sizes:", paste(names(group_sizes), "=", group_sizes, collapse = ", ")),
      paste("- Total sample size:", total_n),
      "",
      sep = "\n"
    )
    
    assumption_text <- paste(assumption_text,
                             "NORMALITY CHECK (Shapiro-Wilk test, p > 0.05 indicates normality):",
                             sep = "\n")
    
    normality_violations <- 0
    for (group_name in names(group_sizes)) {
      group_data <- df$numeric_var[df$categorical_var == group_name]
      group_data <- group_data[!is.na(group_data)]
      
      if (length(group_data) >= 3 && length(group_data) <= 5000) {
        shapiro_result <- shapiro.test(group_data)
        p_val <- shapiro_result$p.value
        w_stat <- shapiro_result$statistic
        
        if (p_val > 0.05) {
          status <- "✓ Normal"
        } else {
          status <- "✗ Non-normal"
          normality_violations <- normality_violations + 1
        }
        
        # Better formatting for very small p-values
        p_display <- if (p_val < 0.001) {
          paste("< 0.001")
        } else {
          paste("=", round(p_val, 3))
        }
        
        assumption_text <- paste(assumption_text,
                                 paste("  ", group_name, ": W =", round(w_stat, 4),
                                       ", p", p_display, "-", status),
                                 paste("    (n =", length(group_data), "observations)"),
                                 sep = "\n")
      } else if (length(group_data) > 5000) {
        assumption_text <- paste(assumption_text,
                                 paste("  ", group_name, ": Sample too large for Shapiro-Wilk test",
                                       "(n =", length(group_data), ") - use other normality tests"),
                                 sep = "\n")
      } else {
        assumption_text <- paste(assumption_text,
                                 paste("  ", group_name, ": Sample size too small for reliable test",
                                       "(n =", length(group_data), ")"),
                                 sep = "\n")
      }
    }
    
    
    # Equal variances check (F-test)
    tryCatch({
      var_test <- var.test(numeric_var ~ categorical_var, data = df)
      var_p_value <- var_test$p.value
      
      assumption_text <- paste(assumption_text,
                               "",
                               "EQUAL VARIANCES CHECK (F-test, p > 0.05 indicates equal variances):",
                               paste("  F-statistic =", round(var_test$statistic, 4)),
                               paste("  p-value =", round(var_p_value, 4)),
                               if (var_p_value > 0.05) "  ✓ Equal variances" else "  ✗ Unequal variances",
                               sep = "\n")
    }, error = function(e) {
      assumption_text <- paste(assumption_text,
                               "",
                               "EQUAL VARIANCES CHECK: Cannot perform F-test",
                               sep = "\n")
      var_p_value <- NA
    })
    
    # Sample size adequacy (n ≥ 30 per group for robustness)
    small_sample_groups <- sum(group_sizes < 30)
    assumption_text <- paste(assumption_text,
                             "",
                             "SAMPLE SIZE ADEQUACY (n ≥ 30 per group for robustness to non-normality):",
                             paste("  Groups with n < 30:", small_sample_groups, "out of", length(group_sizes)),
                             sep = "\n")
    
    
    assumptions_met <- TRUE
    warning_messages <- c()
    
    if (normality_violations > 0) {
      assumptions_met <- FALSE
      warning_messages <- c(warning_messages, "Non-normal data detected")
    }
    
    if (!is.na(var_p_value) && var_p_value <= 0.05) {
      assumptions_met <- FALSE
      warning_messages <- c(warning_messages, "Unequal variances detected")
    }
    
    if (small_sample_groups > 0) {
      assumptions_met <- FALSE
      warning_messages <- c(warning_messages, "Small sample sizes detected")
    }
    
    assumption_text <- paste(assumption_text,
                             "",
                             "====== ASSESSMENT ======",
                             sep = "\n")
    
    if (assumptions_met) {
      assumption_text <- paste(assumption_text,
                               "✓ ALL ASSUMPTIONS MET: T-test is appropriate and reliable",
                               sep = "\n")
    } else {
      assumption_text <- paste(assumption_text,
                               "✗ ASSUMPTIONS VIOLATED:",
                               paste("  -", warning_messages, collapse = "\n"),
                               "",
                               "RECOMMENDATIONS:",
                               "- Use Welch's t-test for unequal variances (R default)",
                               "- Consider Mann-Whitney U test for non-normal data",
                               "- Bootstrap or permutation tests for small samples",
                               "- Transform data if appropriate",
                               "",
                               "⚠ WARNING: Standard t-test results may be unreliable!",
                               sep = "\n")
    }
    
    return(assumption_text)
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
      tryCatch({
        chisq.test(tbl)
      }, error = function(e) {
        return(NULL)
      })
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
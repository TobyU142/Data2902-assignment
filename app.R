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
  'sleep_schedule_(10_point_scale)' = cleaned_sleep_schedule,
  random_number = cleaned_random_number,
  'daily_anxiety_frequency_(10_point_scale)' = cleaned_daily_anxiety_frequency,
  'team_role_type_(Passive_to_active)' = cleaned_team_role_type,
  weekly_exercise_hours = cleaned_weekly_exercise_hours,
  weekly_paid_work_hours = cleaned_weekly_paid_work_hours,
  daily_water_intake_l = cleaned_daily_water_intake_l,
  perceived_old_age = cleaned_perceived_old_age,
  sibling_count = cleaned_sibling_count,
  allergy_count = cleaned_allergy_count,
  usual_bedtime = cleaned_usual_bedtime,
  fluent_languages = cleaned_fluent_languages,
  readable_languages = cleaned_readable_languages,
  wam = cleaned_wam
)

# Define the list of categorical variables
cat_list <- list(
  favourite_number = cleaned_favourite_number,
  trimester_or_semester = cleaned_trimester_or_semester,
  target_grade = cleaned_target_grade,
  work_status = cleaned_work_status,
  assignment_preference = cleaned_assignment,
  tendency_yes_or_no = cleaned_tendency_yes_or_no,
  pay_rent = cleaned_pay_rent,
  stall_choice = cleaned_stall_choice,
  living_arrangements = cleaned_living_arrangements,
  believe_in_aliens = cleaned_believe_in_aliens,
  gender = cleaned_gender,
  diet_style = cleaned_diet_style,
  drivers_license = cleaned_drivers_license,
  relationship_status = cleaned_relationship_status,
  steak_preference = cleaned_steak_preference,
  dominant_hand = cleaned_dominant_hand,
  enrolled_unit = cleaned_enrolled_unit,
  assignments_on_time = cleaned_assignments_on_time,
  used_r_before = cleaned_used_r_before,
  university_year = cleaned_university_year
)

cleaned_data_list <- list(
  numeric = num_list,
  categorical = cat_list
)

combined_var_names <- c(names(cleaned_data_list$numeric),
                        names(cleaned_data_list$categorical))

# Custom CSS for better styling
css <- "
  .content-wrapper, .right-side {
    background-color: #f8f9fa;
  }
  
  body {
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    background-color: #f8f9fa;
  }
  
  .main-header {
    background: linear-gradient(135deg, #2c3e50 0%, #3498db 100%);
    color: white;
    padding: 25px;
    margin-bottom: 25px;
    border-radius: 8px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.15);
  }
  
  .main-title {
    margin: 0;
    font-weight: 300;
    font-size: 2.2em;
  }
  
  .subtitle {
    margin: 8px 0 0 0;
    opacity: 0.9;
    font-weight: 300;
    font-size: 1.1em;
  }
  
  .card {
    background: white;
    border-radius: 10px;
    padding: 20px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.1);
    margin-bottom: 20px;
    border: none;
  }
  
  .card-header {
    color: #2c3e50;
    margin-top: 0;
    margin-bottom: 15px;
    font-weight: 600;
    font-size: 1.3em;
    border-bottom: 2px solid #e9ecef;
    padding-bottom: 8px;
  }
  
  .guide-box {
    background-color: #e8f4f8;
    padding: 15px;
    border-radius: 8px;
    border-left: 4px solid #3498db;
    margin-top: 15px;
  }
  
  .guide-title {
    margin-top: 0;
    color: #2c3e50;
    font-weight: 600;
  }
  
  .guide-list {
    margin-bottom: 0;
    color: #34495e;
    font-size: 14px;
  }
  
  .selection-display {
    background-color: #f8f9fa;
    border: 1px solid #dee2e6;
    border-radius: 6px;
    padding: 12px;
    font-family: 'Courier New', monospace;
    font-size: 13px;
    color: #495057;
  }
  
  .level-selector {
    margin-top: 15px;
    padding: 15px;
    background-color: #fff3cd;
    border-radius: 8px;
    border-left: 4px solid #ffc107;
  }
  
  .level-selector-title {
    margin-top: 0;
    color: #856404;
    font-weight: 600;
  }
  
  .level-selector-text {
    font-size: 13px;
    color: #856404;
    margin-bottom: 12px;
  }
  
  .welcome-card {
    text-align: center;
    padding: 50px 30px;
    color: #6c757d;
  }
  
  .welcome-title {
    color: #495057;
    margin-bottom: 15px;
  }
  
  .assumption-section {
    margin: 15px 0;
    padding: 15px;
    border-radius: 8px;
    border-left: 4px solid;
  }
  
  .assumption-met {
    background-color: #d4edda;
    border-left-color: #28a745;
    color: #155724;
  }
  
  .assumption-warning {
    background-color: #fff3cd;
    border-left-color: #ffc107;
    color: #856404;
  }
  
  .assumption-violated {
    background-color: #f8d7da;
    border-left-color: #dc3545;
    color: #721c24;
  }
  
  .plot-container {
    text-align: center;
    margin-top: 20px;
  }
  
  .empty-plot {
    color: #6c757d;
    font-size: 16px;
    font-weight: 300;
  }
  
  .form-control {
    border-radius: 6px;
    border: 1px solid #ced4da;
    transition: border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out;
  }
  
  .form-control:focus {
    border-color: #3498db;
    box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
  }
  
  .btn {
    border-radius: 6px;
    font-weight: 500;
    transition: all 0.15s ease-in-out;
  }
"

ui <- fluidPage(
  tags$head(
    tags$style(HTML(css)),
    tags$title("Statistical Analysis Dashboard")
  ),
  
  # Header
  div(class = "main-header",
      h1("Statistical Analysis Dashboard", class = "main-title"),
      h4("Interactive Variable Analysis & Assumption Checking", class = "subtitle")
  ),
  
  fluidRow(
    # Sidebar
    column(4,
           div(class = "card",
               h4("Variable Selection", class = "card-header"),
               
               selectInput("var1", 
                           "Primary Variable:", 
                           choices = c("Choose a variable..." = "", combined_var_names),
                           selected = "",
                           width = "100%"),
               
               selectInput("var2",
                           "Grouping Variable (Categorical):",
                           choices = c("Choose a variable..." = "", names(cleaned_data_list$categorical)),
                           selected = "",
                           width = "100%"),
               
               uiOutput("levelSelectors"),
               
               div(class = "guide-box",
                   h5("Quick Guide", class = "guide-title"),
                   tags$ul(
                     tags$li("Select any variable type for primary analysis"),
                     tags$li("Choose a categorical variable for grouping"),
                     tags$li("Review assumptions before interpreting results"),
                     class = "guide-list"
                   )
               )
           ),
           
           # Current selections display
           div(class = "card",
               h5("Current Analysis", class = "card-header"),
               div(class = "selection-display",
                   verbatimTextOutput("selected_vars", placeholder = TRUE)
               )
           )
    ),
    
    # Main panel
    column(8,
           # Assumptions checker
           uiOutput("assumptions_check"),
           
           # Analysis output
           uiOutput("analysis_output")
    )
  )
)

server <- function(input, output, session) {
  
  # Print the currently selected variables with better formatting
  output$selected_vars <- renderText({
    var1_text <- if(input$var1 == "") "None selected" else input$var1
    var2_text <- if(input$var2 == "") "None selected" else input$var2
    
    paste(
      "Primary Variable:", var1_text,
      "Grouping Variable:", var2_text,
      sep = "\n"
    )
  })
  
  # Dynamically create level selectors
  output$levelSelectors <- renderUI({
    if (input$var1 != "" && input$var2 != "" && (input$var1 %in% names(cleaned_data_list$numeric))) {
      grp_vec <- cleaned_data_list$categorical[[input$var2]]
      if (!is.null(grp_vec)) {
        df_temp <- data.frame(group = as.factor(grp_vec))
        lvl <- levels(df_temp$group)
        if (length(lvl) > 2) {
          div(class = "level-selector",
              h6("Group Selection", class = "level-selector-title"),
              p("Multiple groups detected. Select two for comparison:", class = "level-selector-text"),
              
              fluidRow(
                column(6, selectInput("selectedLevel1", "First Group:",
                                      choices = lvl, selected = lvl[1], width = "100%")),
                column(6, selectInput("selectedLevel2", "Second Group:",
                                      choices = lvl, selected = lvl[2], width = "100%"))
              )
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
    
    if (input$var1 %in% names(cleaned_data_list$categorical)) {
      div(class = "card",
          h4("Chi-Square Test Assumptions", class = "card-header"),
          verbatimTextOutput("chi_assumptions")
      )
    } else if (input$var1 %in% names(cleaned_data_list$numeric)) {
      div(class = "card",
          h4("T-Test Assumptions", class = "card-header"),
          verbatimTextOutput("t_test_assumptions")
      )
    }
  })
  
  # Enhanced Chi-square assumptions checker
  output$chi_assumptions <- renderText({
    chi_result <- chi_test()
    if (is.null(chi_result)) {
      return("Cannot check assumptions - chi-square test cannot be performed.")
    }
    
    expected_freq <- chi_result$expected
    min_expected <- min(expected_freq)
    violations <- sum(expected_freq < 5)
    total_cells <- length(expected_freq)
    
    assumption_text <- paste(
      "REQUIREMENTS:",
      "• All expected frequencies must be ≥ 5 (ensures χ² distribution validity)",
      "• Independence of observations",
      "",
      "RESULTS:",
      paste("• Minimum expected frequency:", round(min_expected, 2)),
      paste("• Cells with frequency < 5:", violations, "out of", total_cells),
      "",
      "Expected Frequencies Table:",
      sep = "\n"
    )
    
    expected_display <- capture.output(print(round(expected_freq, 2)))
    assumption_text <- paste(assumption_text,
                             paste(expected_display, collapse = "\n"),
                             sep = "\n")
    
    if (violations == 0) {
      assumption_text <- paste(assumption_text, 
                               "",
                               "✓ VERDICT: All assumptions satisfied!",
                               "Chi-square test results are reliable and valid.",
                               sep = "\n")
    } else {
      assumption_text <- paste(assumption_text, 
                               "",
                               "✗ VERDICT: Assumptions violated!",
                               paste("Number of problematic cells:", violations),
                               "",
                               "RECOMMENDATIONS:",
                               "• Use Fisher's exact test instead",
                               "• Combine categories if theoretically justified", 
                               "• Collect additional data",
                               "",
                               "⚠ WARNING: Chi-square results may be unreliable!",
                               sep = "\n")
    }
    
    return(assumption_text)
  })
  
  # Enhanced T-test assumptions checker
  output$t_test_assumptions <- renderText({
    df <- df_pair_t()
    if (is.null(df) || nrow(df) < 2) {
      return("Cannot check assumptions - insufficient data available.")
    }
    
    groups <- levels(df$categorical_var)
    if (length(groups) != 2) {
      return("Assumption checking only available for two-group comparisons.")
    }
    
    group_sizes <- table(df$categorical_var)
    total_n <- sum(group_sizes)
    
    assumption_text <- paste(
      "T-TEST REQUIREMENTS:",
      "1. Normality: Data normally distributed in each group",
      "2. Independence: Observations must be independent", 
      "3. Equal variances: Similar variability between groups",
      "",
      "SAMPLE INFORMATION:",
      paste("• Group sizes:", paste(names(group_sizes), "=", group_sizes, collapse = ", ")),
      paste("• Total sample:", total_n, "observations"),
      "",
      "NORMALITY TEST (Shapiro-Wilk, p > 0.05 = normal):",
      sep = "\n"
    )
    
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
        
        p_display <- if (p_val < 0.001) "< 0.001" else paste("=", round(p_val, 3))
        
        assumption_text <- paste(assumption_text,
                                 paste("  ", group_name, ": W =", round(w_stat, 4),
                                       ", p", p_display, "-", status),
                                 paste("    (n =", length(group_data), "observations)"),
                                 sep = "\n")
      } else {
        assumption_text <- paste(assumption_text,
                                 paste("  ", group_name, ": Sample size not suitable for test (n =", length(group_data), ")"),
                                 sep = "\n")
      }
    }
    
    # Variance test
    tryCatch({
      var_test <- var.test(numeric_var ~ categorical_var, data = df)
      var_p_value <- var_test$p.value
      var_status <- if (var_p_value > 0.05) "✓ Equal variances" else "✗ Unequal variances"
      
      assumption_text <- paste(assumption_text,
                               "",
                               "EQUAL VARIANCES TEST (F-test, p > 0.05 = equal):",
                               paste("  F =", round(var_test$statistic, 4), ", p =", round(var_p_value, 4)),
                               paste("  ", var_status),
                               sep = "\n")
    }, error = function(e) {
      var_p_value <- NA
      assumption_text <- paste(assumption_text,
                               "",
                               "EQUAL VARIANCES TEST: Cannot perform F-test",
                               sep = "\n")
    })
    
    # Sample size check
    small_sample_groups <- sum(group_sizes < 30)
    assumption_text <- paste(assumption_text,
                             "",
                             "SAMPLE SIZE CHECK (n ≥ 30 recommended per group):",
                             paste("  Groups with small samples (n < 30):", small_sample_groups, "out of", length(group_sizes)),
                             sep = "\n")
    
    # Final assessment
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
                             "FINAL VERDICT:",
                             sep = "\n")
    
    if (assumptions_met) {
      assumption_text <- paste(assumption_text,
                               "✓ All assumptions satisfied! T-test results are reliable.",
                               sep = "\n")
    } else {
      assumption_text <- paste(assumption_text,
                               "✗ Some assumptions violated:",
                               paste("  •", warning_messages, collapse = "\n"),
                               "",
                               "RECOMMENDATIONS:",
                               "  • Welch's t-test for unequal variances (default in R)",
                               "  • Mann-Whitney U test for non-normal data",
                               "  • Bootstrap methods for small/problematic samples",
                               "",
                               "⚠ Interpret standard t-test results with caution!",
                               sep = "\n")
    }
    
    return(assumption_text)
  })
  
  # Enhanced analysis output
  output$analysis_output <- renderUI({
    if (input$var1 == "" || input$var2 == "") {
      return(
        div(class = "card welcome-card",
            h4("Welcome to Statistical Analysis", class = "welcome-title"),
            p("Please select both variables above to begin your statistical analysis.", 
              style = "color: #6c757d; font-size: 16px;")
        )
      )
    }
    
    if (input$var1 %in% names(cleaned_data_list$categorical)) {
      div(class = "card",
          h4("Chi-Square Independence Test", class = "card-header"),
          verbatimTextOutput("chiSummary"),
          
          h5("Mosaic Plot Visualization", style = "color: #2c3e50; margin-top: 25px; margin-bottom: 15px;"),
          div(class = "plot-container",
              plotOutput("chiPlot", height = "400px")
          )
      )
    } else if (input$var1 %in% names(cleaned_data_list$numeric)) {
      div(class = "card",
          h4("Two-Sample Comparison", class = "card-header"),
          verbatimTextOutput("tTestSummary"),
          
          h5("Distribution Comparison", style = "color: #2c3e50; margin-top: 25px; margin-bottom: 15px;"),
          div(class = "plot-container",
              plotOutput("tTestPlot", height = "400px")
          )
      )
    }
  })
  
  # ========================
  # CHI-SQUARE TEST COMPONENTS
  # ========================
  
  selected_cat1 <- reactive({
    if (input$var1 %in% names(cleaned_data_list$categorical)) {
      return(cleaned_data_list$categorical[[input$var1]])
    } else {
      return(NULL)
    }
  })
  
  selected_cat2 <- reactive({
    if (input$var2 != "") {
      return(cleaned_data_list$categorical[[input$var2]])
    } else {
      return(NULL)
    }
  })
  
  df_pair_cat <- reactive({
    vec1 <- selected_cat1()
    vec2 <- selected_cat2()
    
    if (is.null(vec1) || is.null(vec2)) return(NULL)
    n_common <- min(length(vec1), length(vec2))
    data.frame(cat1 = vec1[1:n_common],
               cat2 = vec2[1:n_common],
               stringsAsFactors = TRUE)
  })
  
  contingency_table <- reactive({
    df <- df_pair_cat()
    if (is.null(df)) return(NULL)
    table(df$cat1, df$cat2)
  })
  
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
      cat("Please ensure both variables are categorical with at least 2 levels each.\n")
    } else {
      print(chi_test())
    }
  })
  
  output$chiPlot <- renderPlot({
    tbl <- contingency_table()
    if (is.null(tbl)) {
      plot.new()
      text(0.5, 0.5, "Select two categorical variables\nto view the mosaic plot", 
           cex = 1.2, col = "#6c757d", family = "sans")
    } else {
      # Enhanced mosaic plot
      mosaicplot(tbl, 
                 main = paste("Mosaic Plot:", input$var1, "vs", input$var2),
                 color = c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c"),
                 cex.axis = 0.9,
                 main.cex = 1.1,
                 las = 1)
    }
  }, bg = "white")
  
  # ========================
  # T-TEST COMPONENTS
  # ========================
  
  selected_num <- reactive({
    if (input$var1 %in% names(cleaned_data_list$numeric)) {
      return(cleaned_data_list$numeric[[input$var1]])
    } else {
      return(NULL)
    }
  })
  
  df_pair_t <- reactive({
    num_vec <- selected_num()
    cat_vec <- cleaned_data_list$categorical[[input$var2]]
    
    if (is.null(num_vec) || is.null(cat_vec)) return(NULL)
    
    n_common <- min(length(num_vec), length(cat_vec))
    df <- data.frame(numeric_var = num_vec[1:n_common],
                     categorical_var = as.factor(cat_vec[1:n_common]),
                     stringsAsFactors = TRUE)
    df <- df[complete.cases(df), ]
    
    if(nlevels(df$categorical_var) > 2) {
      if(!is.null(input$selectedLevel1) && !is.null(input$selectedLevel2)) {
        df <- df %>% filter(categorical_var %in% c(input$selectedLevel1, input$selectedLevel2))
        df$categorical_var <- droplevels(df$categorical_var)
      }
    }
    return(df)
  })
  
  t_test_result <- reactive({
    df <- df_pair_t()
    if (is.null(df) || nrow(df) < 2) return("Not enough data to perform the test.")
    
    groups <- levels(df$categorical_var)
    if(length(groups) < 2) {
      return("At least two groups are needed for comparison.")
    } else if(length(groups) == 2) {
      tryCatch({
        t.test(numeric_var ~ categorical_var, data = df)
      }, error = function(e) {
        paste("Error in t-test:", e$message)
      })
    } else {
      fit <- aov(numeric_var ~ categorical_var, data = df)
      summary(fit)
    }
  })
  
  output$tTestSummary <- renderPrint({
    res <- t_test_result()
    if (is.null(res)) {
      cat("Test cannot be performed.\nPlease ensure both variables contain valid data.\n")
    } else {
      if (is.character(res)) {
        cat(res)
      } else {
        print(res)
        if(is.list(res) && !is.null(res$p.value)) {
          cat("\nINTERPRETATION:\n")
          if (res$p.value < 0.05) {
            cat("✓ Statistically significant difference detected (p < 0.05)\n")
          } else {
            cat("✗ No statistically significant difference found (p ≥ 0.05)\n")
          }
        }
      }
    }
  })
  
  output$tTestPlot <- renderPlot({
    df <- df_pair_t()
    if (is.null(df) || nrow(df) < 1) {
      plot.new()
      text(0.5, 0.5, "Select a numeric and categorical variable\nto view the comparison plot", 
           cex = 1.2, col = "#6c757d", family = "sans")
    } else {
      # Enhanced boxplot with professional styling
      par(bg = "white", family = "sans")
      boxplot(numeric_var ~ categorical_var, data = df,
              main = paste("Distribution Comparison:", input$var1, "by", input$var2),
              xlab = input$var2,
              ylab = input$var1,
              col = c("#3498db", "#e74c3c"),
              border = c("#2980b9", "#c0392b"),
              notch = TRUE,
              cex.main = 1.1,
              cex.lab = 1.0,
              cex.axis = 0.9,
              las = 1)
      
      # Add sample sizes
      group_sizes <- table(df$categorical_var)
      for(i in 1:length(group_sizes)) {
        text(i, par("usr")[3] - 0.02 * diff(par("usr")[3:4]), 
             paste("n =", group_sizes[i]), 
             pos = 1, cex = 0.8, col = "#7f8c8d")
      }
      
      # Add grid for better readability
      grid(col = "#f0f0f0", lty = 1, nx = NA, ny = NULL)
      
      # Redraw the boxplot on top of grid
      boxplot(numeric_var ~ categorical_var, data = df,
              col = c("#3498db", "#e74c3c"),
              border = c("#2980b9", "#c0392b"),
              notch = TRUE,
              add = TRUE,
              axes = FALSE)
    }
  }, bg = "white")
}

shinyApp(ui = ui, server = server)
library(tidyverse)
library(here)
library(dplyr)
library(stringr)
x = readxl::read_excel(here("/Users/tobyurban/Downloads", "DATA2x02_survey_2025_Responses.xlsx"))
clean_data <- function(x) {

  old_names = colnames(x)
  new_names <- c(
    "timestamp",
    "target_grade",
    "assignment_preference",
    "trimester_or_semester",
    "age",
    "tendency_yes_or_no",
    "pay_rent",
    "stall_choice",
    "weetbix_count",
    "weekly_food_spend",
    "living_arrangements",
    "weekly_alcohol",
    "believe_in_aliens",
    "height",
    "commute",
    "daily_anxiety_frequency",
    "weekly_study_hours",
    "work_status",
    "social_media",
    "gender",
    "average_daily_sleep",
    "usual_bedtime",
    "sleep_schedule",
    "sibling_count",
    "allergy_count",
    "diet_style",
    "random_number",
    "favourite_number",
    "favourite_letter",
    "drivers_license",
    "relationship_status",
    "daily_short_video_time",
    "computer_os",
    "steak_preference",
    "dominant_hand",
    "enrolled_unit",
    "weekly_exercise_hours",
    "weekly_paid_work_hours",
    "assignments_on_time",
    "used_r_before",
    "team_role_type",
    "university_year",
    "favourite_anime",
    "fluent_languages",
    "readable_languages",
    "country_of_birth",
    "wam",
    "shoe_size",
    "books_read_highschool",
    "daily_water_intake_l",
    "perceived_old_age",
    "study_music_preference"
  )
  colnames(x) = new_names
  name_combo = bind_cols(New = new_names, Old = old_names)
  name_combo %>% gt::gt()
  
  
  
  cleaned_assignment = x$assignment_preference[!is.na(x$assignment_preference)]
  cleaned_assignment
  
  target = x$target_grade[!is.na(x$target_grade)]
  
  
  sleep_duration = x$average_daily_sleep[!is.na(x$average_daily_sleep)]
  sleep_duration[sleep_duration == '28800 seconds'] <- 8
  sleep_duration[sleep_duration == 'Eight Hours'] <- 8
  sleep_duration <- sleep_duration[sleep_duration != 'good']
  sleep_duration <- sleep_duration[sleep_duration != 25.0]
  sleep_duration <- str_replace_all(sleep_duration, "1/2", ".5")
  sleep_duration <- str_replace_all(sleep_duration, "6-7", "6.5")
  sleep_duration <- str_replace_all(sleep_duration, "7-8", "7.5")
  sleep_duration <- str_remove_all(sleep_duration, "\\s")
  sleep_duration <- gsub("[^0-9.]", "", sleep_duration)
  
  
  
  
  
  cleaned_wam = x$wam[!is.na(x$wam)]
  
  recommended_wam = x$wam[!is.na(x$weekly_study_hours) & x$weekly_study_hours == 40]
  
  
  non_recommended_wam = x$wam[!is.na(x$weekly_study_hours) & x$weekly_study_hours != 40]
  non_recommended_wam = non_recommended_wam[!is.na(non_recommended_wam)]
  
  
  cram <- recode(cleaned_assignment,"cram at the last second" = 1, "do them immediately" = 0,"draw up a schedule and work through it in planned stages" = 0)
  
  cram_count = sum(cram) #Observed cramming in sample
  cram_total_responses = length(cram) 
  
  
}
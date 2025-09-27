library(tidyverse)
library(here)
library(dplyr)
library(stringr)
library(janitor)
library(gendercoder)
library(readr)
x = readxl::read_excel("DATA2x02_survey_2025_Responses.xlsx")



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
  
  cleaned_target_grade = x$target_grade[!is.na(x$target_grade)]
  
  cleaned_assignment = x$assignment_preference[!is.na(x$assignment_preference)]
  cleaned_assignment <- recode(cleaned_assignment,"cram at the last second" = "cram", "do them immediately" = "immediate","draw up a schedule and work through it in planned stages" = "schedule")
  
  

  temp_trimester <- as.character(x$trimester_or_semester)
  
  cleaned_trimester_or_semester <- ifelse(
    grepl("trimester", temp_trimester, ignore.case = TRUE), "Trimester",
    ifelse(grepl("semester", temp_trimester, ignore.case = TRUE), "Semester", NA_character_)
  )
  
  
  cleaned_trimester_or_semester <- cleaned_trimester_or_semester[!is.na(cleaned_trimester_or_semester)]
  
  


  cleaned_age <- x$age[!is.na(x$age)]
  cleaned_age <- cleaned_age[cleaned_age != 2000]
  
  cleaned_tendency_yes_or_no <- x$tendency_yes_or_no[!is.na(x$tendency_yes_or_no)]
  cleaned_tendency_yes_or_no[cleaned_tendency_yes_or_no == 'More \"Yes\"'] <- 'Yes'
  cleaned_tendency_yes_or_no[cleaned_tendency_yes_or_no == 'More \"No\"'] <- 'No'
  
  cleaned_pay_rent <- x$pay_rent[!is.na(x$pay_rent)]
  cleaned_pay_rent[cleaned_pay_rent == "Mortgage"] <- 'Yes'
  cleaned_pay_rent <- cleaned_pay_rent[cleaned_pay_rent %in% c('Yes', 'No')]
  
  cleaned_stall_choice <- x$stall_choice[!is.na(x$stall_choice)]
  
  cleaned_weetbix_count <- x$weetbix_count[!is.na(x$weetbix_count)]
  cleaned_weetbix_count <- cleaned_weetbix_count[cleaned_weetbix_count <= 10]
  
  cleaned_food_spend <- x$weekly_food_spend[!is.na(x$weekly_food_spend)]
  
  cleaned_living_arrangements <- x$living_arrangements[!is.na(x$living_arrangements)]
  
  temp_living_arrangements <- as.character(x$living_arrangements[ !is.na(x$living_arrangements) ])
  living_allowed_values <- c("With parent(s) and/or sibling(s)",
                             "With partner",
                             "College or student accomodation",
                             "Alone",
                             "Share house")
  
  cleaned_living_arrangements <- ifelse(temp_living_arrangements %in% living_allowed_values,
                                        temp_living_arrangements,
                                        NA_character_)
  
  cleaned_living_arrangements <- cleaned_living_arrangements[ !is.na(cleaned_living_arrangements) ]
  
  
  cleaned_weekly_alcohol <- x$weekly_alcohol[!is.na(x$weekly_alcohol)]
  
  cleaned_believe_in_aliens <- x$believe_in_aliens[!is.na(x$believe_in_aliens)]
  
  cleaned_height <- x$height[!is.na(x$height)] ##
  fi_match <- str_match(cleaned_height, "^(\\d+)[\\s]*[\'’\"][\\s]*(\\d+)")
  is_ft_in <- !is.na(fi_match[, 1])
  height_cm <- numeric(length(cleaned_height))
  if(any(is_ft_in)) {
    height_cm[is_ft_in] <- as.numeric(fi_match[is_ft_in, 2]) * 30.48 +
      as.numeric(fi_match[is_ft_in, 3]) * 2.54
  }
  non_ft_in <- !is_ft_in
  if(any(non_ft_in)) {
    num_val <- parse_number(cleaned_height[non_ft_in])
    height_cm[non_ft_in] <- ifelse(num_val <= 2.5, num_val * 100,
                                   ifelse(num_val > 2.5 & num_val < 100, NA_real_, num_val))
  }
  height_cm
  
  cleaned_height <- height_cm[!is.na(height_cm)]
  
  
  cleaned_commute <- x$commute[!is.na(x$commute)] ## dont do
  
  
  cleaned_daily_anxiety_frequency <- x$daily_anxiety_frequency[!is.na(x$daily_anxiety_frequency)]
  
  cleaned_weekly_study_hours <- x$weekly_study_hours[!is.na(x$weekly_study_hours)]
  
  cleaned_work_status <- x$work_status[!is.na(x$work_status)] 
  cleaned_work_status <- recode(cleaned_work_status, "I don't currently work" = "Unemployed")
  accepted_status <- c("Full time", "Part time", "Casual", "Self employed", "Contractor", "Unemployed")
  cleaned_work_status <- cleaned_work_status[cleaned_work_status %in% accepted_status]
  
  
  cleaned_social_media <- x$social_media[!is.na(x$social_media)] ##dont use this one
  
   
  cleaned_gender <- x$gender[!is.na(x$gender)] 
  std_gender <- tolower(str_trim(cleaned_gender))
  female_pattern <- "^(f(emale)?)$"
  male_pattern   <- "^(m(ale)?|man)$"
  std_gender_cleaned <- ifelse(str_detect(std_gender, female_pattern), "Female",
                               ifelse(str_detect(std_gender, male_pattern), "Male", NA))
  cleaned_gender <- std_gender_cleaned[!is.na(std_gender_cleaned)]
  
  print(cleaned_gender)
  
  
  
  sleep_duration = x$average_daily_sleep[!is.na(x$average_daily_sleep)]
  sleep_duration[sleep_duration == '28800 seconds'] <- 8
  sleep_duration[sleep_duration == 'Eight Hours'] <- 8
  sleep_duration <- sleep_duration[sleep_duration != 'good']
  sleep_duration <- sleep_duration[sleep_duration != 25.0]
  sleep_duration <- str_replace_all(sleep_duration, "1/2", ".5")
  sleep_duration <- str_replace_all(sleep_duration, "6-7", "6.5")
  sleep_duration <- str_replace_all(sleep_duration, "7-8", "7.5")
  sleep_duration <- str_remove_all(sleep_duration, "\\s")
  cleaned_sleep_duration <- gsub("[^0-9.]", "", sleep_duration)
  
  cleaned_usual_bedtime <- x$usual_bedtime[!is.na(x$usual_bedtime)] 
  cleaned_usual_bedtime <- as.POSIXct(cleaned_usual_bedtime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  cleaned_usual_bedtime <- format(cleaned_usual_bedtime, "%H:%M:%S")
  
  
  
  cleaned_sleep_schedule <- x$sleep_schedule[!is.na(x$sleep_schedule)] 
  
  cleaned_sibling_count <- x$sibling_count[!is.na(x$sibling_count)] 
  cleaned_sibling_count <- str_replace_all(cleaned_sibling_count, regex("one", ignore_case = TRUE), '1')
  cleaned_sibling_count <- str_replace_all(cleaned_sibling_count, regex("two", ignore_case = TRUE), '2')
  cleaned_sibling_count <- cleaned_sibling_count[cleaned_sibling_count <= 8]
  cleaned_sibling_count <- gsub("[^0-9.]", "", cleaned_sibling_count)
  cleaned_sibling_count <- as.integer(cleaned_sibling_count)
  
 
  
  cleaned_allergy_count <- x$allergy_count 
  allergy_replacements <- c(
    "(?i)\\b(zero|none|nothing|no|i don't have|n/a)\\b" = "0",
    "(?i)\\b(one|shellfish)\\b" = "1",
    "(?i)\\btwo\\b" = "2",
    "(?i)\\bthree\\b" = "3"
  )
  cleaned_allergy_count <- str_replace_all(cleaned_allergy_count, allergy_replacements)
  cleaned_allergy_count <- as.numeric(cleaned_allergy_count)
  cleaned_allergy_count <- cleaned_allergy_count[cleaned_allergy_count <= 20]
  cleaned_allergy_count <- cleaned_allergy_count[!is.na(cleaned_allergy_count)]
  cleaned_allergy_count
  
  
  
  cleaned_diet_style <- x$diet_style[!is.na(x$diet_style)] 
  accepted_diets <- c("Vegan", "Vegetarian", "Pescatarian", "Omnivorous")
  cleaned_diet_style <- cleaned_diet_style[ cleaned_diet_style %in% accepted_diets ]
  cleaned_diet_style

  
  
  
  
  cleaned_random_number <- x$random_number[!is.na(x$random_number)]
  
  cleaned_favourite_number <- x$favourite_number[!is.na(x$favourite_number)]

  cleaned_favourite_letter <- x$favourite_letter[!is.na(x$favourite_letter)] ## Dont do
  
  cleaned_drivers_license <- x$drivers_license[!is.na(x$drivers_license)] 
  cleaned_drivers_license <- tolower(cleaned_drivers_license)
  cleaned_drivers_license <- ifelse(str_detect(cleaned_drivers_license, "^yes"), "Yes",
  ifelse(str_detect(cleaned_drivers_license, "^no"), "No", NA_character_))
  cleaned_drivers_license <- cleaned_drivers_license[!is.na(cleaned_drivers_license)]
  cleaned_drivers_license
  
  
  cleaned_relationship_status <- x$relationship_status[!is.na(x$relationship_status)]
  cleaned_relationship_status[cleaned_relationship_status == "it's complicated"] <- "Yes"
  cleaned_relationship_status[cleaned_relationship_status == "I have 2 girlfriends"] <- "Yes"
  cleaned_relationship_status <- ifelse(str_detect(cleaned_relationship_status, "^Yes"), "Yes",
  ifelse(str_detect(cleaned_relationship_status, "^No"), "No", NA_character_))
  cleaned_relationship_status <- cleaned_relationship_status[!is.na(cleaned_relationship_status)]
  cleaned_relationship_status <- cleaned_relationship_status[cleaned_relationship_status %in% c("Yes", "No")]
  cleaned_relationship_status <- ifelse(cleaned_relationship_status == "Yes", 
                                        "Taken", 
                                        "single")
  
  
  
  
  
  cleaned_daily_short_video_time <- x$daily_short_video_time[!is.na(x$daily_short_video_time)]##Don't use
  cleaned_daily_short_video_time
  
  
  cleaned_computer_os <- x$computer_os[!is.na(x$computer_os)]## Don't use
  cleaned_computer_os

  
  cleaned_steak_preference <- x$steak_preference[!is.na(x$steak_preference)]
  accepted_steak_preferences <- c("Rare", "Medium-rare", "Medium", "Medium-well done", "Well done")
  cleaned_steak_preference <- cleaned_steak_preference[cleaned_steak_preference %in% accepted_steak_preferences]
  cleaned_steak_preference
  
  
  
  
  cleaned_dominant_hand <- x$dominant_hand[!is.na(x$dominant_hand)]
  
  cleaned_enrolled_unit <- x$enrolled_unit[!is.na(x$enrolled_unit)]
  
  cleaned_weekly_exercise_hours <- x$weekly_exercise_hours[!is.na(x$weekly_exercise_hours)] 
  cleaned_weekly_exercise_hours <- cleaned_weekly_exercise_hours[cleaned_weekly_exercise_hours <= 100]
  
  cleaned_weekly_paid_work_hours <- x$weekly_paid_work_hours[!is.na(x$weekly_paid_work_hours)]##
  cleaned_weekly_paid_work_hours <- cleaned_weekly_paid_work_hours[cleaned_weekly_paid_work_hours != 0]
  cleaned_weekly_paid_work_hours <- cleaned_weekly_paid_work_hours[cleaned_weekly_paid_work_hours <= 100]
  cleaned_weekly_paid_work_hours
  
  
  
  
  
  cleaned_assignments_on_time <- x$assignments_on_time[!is.na(x$assignments_on_time)]
  
  cleaned_used_r_before <- x$used_r_before[!is.na(x$used_r_before)]
  
  cleaned_team_role_type <- x$team_role_type[!is.na(x$team_role_type)]
  
  cleaned_university_year <- x$university_year[!is.na(x$university_year)]
  
  cleaned_favourite_anime <- x$favourite_anime[!is.na(x$favourite_anime)]## Dont do

  cleaned_fluent_languages <- x$fluent_languages[!is.na(x$fluent_languages)]
  cleaned_fluent_languages <- str_replace_all(cleaned_fluent_languages, regex('one', ignore_case = TRUE), '1')
  cleaned_fluent_languages <- str_replace_all(cleaned_fluent_languages, regex("two", ignore_case = TRUE), '2')
  cleaned_fluent_languages[cleaned_fluent_languages == '1 fluently, 6 conversationally'] <- '1'
  cleaned_fluent_languages[cleaned_fluent_languages == '1.5'] <- 'NA'
  cleaned_fluent_languages <- as.numeric(cleaned_fluent_languages)
  cleaned_fluent_languages <- cleaned_fluent_languages[!is.na(cleaned_fluent_languages)]
  cleaned_fluent_languages
  
  cleaned_readable_languages <- x$readable_languages[!is.na(x$readable_languages)]
  cleaned_readable_languages <- str_replace_all(cleaned_readable_languages, regex('one', ignore_case = TRUE), '1')
  cleaned_readable_languages <- str_replace_all(cleaned_readable_languages, regex("two", ignore_case = TRUE), '2')
  cleaned_readable_languages <- gsub("[^0-9.]", "", cleaned_readable_languages)
  cleaned_readable_languages <- as.integer(cleaned_readable_languages)
  cleaned_readable_languages <- cleaned_readable_languages[cleaned_readable_languages != 0]
  cleaned_readable_languages <- cleaned_readable_languages[cleaned_readable_languages <= 15]
  cleaned_readable_languages <- cleaned_readable_languages[!is.na(cleaned_readable_languages)]
  cleaned_readable_languages
  
  
 
  
  cleaned_country_of_birth <- x$country_of_birth[!is.na(x$country_of_birth)]## Dont do
  cleaned_country_of_birth
  
  
  cleaned_wam <- x$wam[!is.na(x$wam)]
  cleaned_wam <- gsub("[^0-9.]", "", cleaned_wam)
  cleaned_wam <- as.numeric(cleaned_wam)
  cleaned_wam <- cleaned_wam[cleaned_wam >=10]
  cleaned_wam
  
  
  
  cleaned_shoe_size <- x$shoe_size[!is.na(x$shoe_size)]## Don't use
  cleaned_shoe_size
  
  
  
  
  
  cleaned_books_read_highschool <- x$books_read_highschool[!is.na(x$books_read_highschool)]## Don't do
  
  
  
  cleaned_daily_water_intake_l <- x$daily_water_intake_l[!is.na(x$daily_water_intake_l)]
  cleaned_daily_water_intake_l <- gsub("[^0-9.]", "", cleaned_daily_water_intake_l)
  cleaned_daily_water_intake_l <- as.numeric(cleaned_daily_water_intake_l)
  cleaned_daily_water_intake_l <- cleaned_daily_water_intake_l[!is.na(cleaned_daily_water_intake_l)]
  cleaned_daily_water_intake_l[cleaned_daily_water_intake_l > 500] <- 
    cleaned_daily_water_intake_l[cleaned_daily_water_intake_l > 500] / 1000
  
  cleaned_daily_water_intake_l <- cleaned_daily_water_intake_l[
    !(cleaned_daily_water_intake_l >= 10 & cleaned_daily_water_intake_l <= 500)
  ]
  
  cleaned_daily_water_intake_l
  
  cleaned_perceived_old_age <- x$perceived_old_age[!is.na(x$perceived_old_age)]
  cleaned_perceived_old_age <- gsub("[^0-9.]", "", cleaned_perceived_old_age)
  cleaned_perceived_old_age <- as.numeric(cleaned_perceived_old_age)
  cleaned_perceived_old_age <- cleaned_perceived_old_age[!is.na(cleaned_perceived_old_age)]
  
  
  
  
  cleaned_study_music_preference <- x$study_music_preference[!is.na(x$study_music_preference)]## Don't do
  
  
  
 
  

  
  


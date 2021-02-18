#---------------------------------
# Education Sector - Care Givers
# Data Cleaning Script
#---------------------------------
# Ben.Smith@reach.initiative.org
# 06/12/2020
#-----------


# Preamble ----------------------------------------------------------------

# For manipulating inputs
library(dplyr)
library(openxlsx)
library(stringr)
library(openxlsx)
library(butteR)
library(tidyr)

# Choose whether you would like to write outputs:
write_output <-c("yes","no")[1]

# Set the directory:
setwd("C:/Users/Ben SMITH/Documents/Bangladesh/REACH Projects/REACH-Education_Sector_Analysis-2020/")

# Read in the raw data ----------------------------------------------------

# Household Response Data:
hh <- read.csv("02 - Inputs/Household Surveys - BGD2006_Education_Caregivers_Final - all_versions_-_False_-_2021-01-31-10-10-59.csv", 
               check.names =FALSE,
               na.strings = c(""," ","NA","N/A"), 
               stringsAsFactors = F) %>% rename("X_uuid" = "_uuid")

# Check that the "access_to_electricity" and "access_to_internet" are as hrs, not dates (auto conversion in excel):
table(hh[,c("access_to_electricity", "access_to_internet_connection")])
  # Assuming these are read in correctly, change the '-' to '_to_' so that excel doesn't auto-format the outputs:
  hh[,"access_to_electricity"] = gsub("-", "_to_", hh[,"access_to_electricity"])
  hh[,"access_to_internet_connection"] = gsub("-", "_to_", hh[,"access_to_internet_connection"])
  
# Individual Response Data:
indv <- read.csv("02 - Inputs/Individual Surveys - BGD2006_Education_Caregivers_Final - all_versions_-_False_-_2021-01-31-10-10-59.csv", 
                 check.names =FALSE,
                 na.strings = c("", " ", "NA", "N/A"), 
                 stringsAsFactors = F) %>% rename("X_uuid" = "_submission__uuid")


# The Cleaning log:
cleaning_log <- read.xlsx("02 - Inputs/20210302_Data_cleaning_logbook_v7.xlsx",
                          na.strings = c(""," ","NA","N/A"))

# # hh uuid that need removing from indv:
# indv_remove = c("5f70fe4f-dd06-49a5-9ff5-e1dc592d9409",
#                 "f13fff68-9c18-47be-bab6-6d49926a09d9",
#                 "1f4ceb38-e7d7-4459-842d-d7b847eb0ea1",
#                 "694a95ad-90ea-4b79-a6c8-9b2ea113556d",
#                 "90a788a8-d4f8-48e5-a1f3-1044408769fa",
#                 "baaca48e-950a-4156-8522-cc1d5bfb06e9",
#                 "872a0731-cb29-4587-919e-d8fc67d086df",
#                 "0f17c571-ae3e-4d3c-802a-6726b19d15f1",
#                 "da2cbd72-b118-4158-89de-9f3231ff753f")
# 
# # Print a list of these for manual editing of the cleaning log (now saved in V3)
# for(i in 1:length(indv_remove)){
#   print(indv_remove[i])
#   print(indv$'_index'[indv$X_uuid %in% indv_remove[i]])
#   print("_____________")
# }


# Data cleaning ------------------------------------------------------------

# Clean the Household responses:
# Filter the cleaning data to only household:
cleaning_log_hh <- filter(cleaning_log, dataset_loop == "household" & change_type != "no_action")

# # Check the cleaning log for potential issues:
# View(check_cleaning_log(df = hh,
#                         df_uuid = "X_uuid",
#                         cl = cleaning_log_hh,
#                         cl_change_type_col ="change_type",
#                         cl_change_col ="question",
#                         cl_uuid = "uuid",
#                         cl_new_val = "new_value"))

# Change values according to the cleaning log:
hh_clean_data <- implement_cleaning_log(df = hh,
                                        df_uuid = "X_uuid",
                                        cl = cleaning_log_hh,
                                        cl_change_type_col ="change_type",
                                        cl_change_col ="question",
                                        cl_uuid = "uuid",
                                        cl_new_val = "new_value")


# Clean the individual responses:

# Remove the "attend_education_camp/non_religious_learning_centre" response (as it does not really exist).
indv$`attend_education_camp/non_religious_learning_centre` = NULL

# Filter the data to only individual responses:
cleaning_log_indv  <- filter(cleaning_log, dataset_loop== "individual" & change_type != "no_action")

# # Check the cleaning log for potential issues:
# View(check_cleaning_log(df = indv, 
#                         df_uuid = "_index", 
#                         cl = cleaning_log_indv, 
#                         cl_change_type_col ="change_type", 
#                         cl_change_col ="question", 
#                         cl_uuid = "index_number/instance_name",
#                         cl_new_val = "new_value"))

# Change values according to the cleaning log:
indv_clean_data <- implement_cleaning_log(df = indv,
                                          df_uuid = "_index",
                                          cl = cleaning_log_indv,
                                          cl_change_type_col ="change_type",
                                          cl_change_col ="question",
                                          cl_uuid = "index_number/instance_name",
                                          cl_new_val = "new_value")


# Filter the data to only household surveys that were removed:
# Added following second round of surveys to remove the individual responses related to the household surveys that were removed.
# This was not used in the first round of scripts as the individual surveys were manually added to the cleaning log.
cleaning_log_indv  <- filter(cleaning_log, dataset_loop== "household" & change_type == "remove_survey"
                             & spotted_by == "Daria" & question == "survey duration")

# Change values according to the cleaning log:
indv_clean_data <- implement_cleaning_log(df = indv_clean_data,
                                          df_uuid = "X_uuid",
                                          cl = cleaning_log_indv,
                                          cl_change_type_col ="change_type",
                                          cl_change_col ="question",
                                          cl_uuid = "uuid",
                                          cl_new_val = "new_value")

# Make level_of_education_camp uniform by removing " " and capitals:
indv_clean_data$level_of_education_camp = gsub(pattern = " ",
                                               replacement = "_",
                                               x = tolower(indv_clean_data$level_of_education_camp))

# Remove all responses that do not have consent:
hh_clean_consent <- hh_clean_data %>% dplyr::filter(informed_consent == "yes")

# Write cleaned data:
if(write_output == "yes"){
  
  write.csv(hh_clean_consent, 
            paste0("03 - Outputs/02_clean_data/",
                   str_replace_all(Sys.Date(),"-","_"),
                   "_hh_cleaned_data_with_consent.csv"))
  
  write.csv(hh_clean_data, 
            paste0("03 - Outputs/02_clean_data/",
                   str_replace_all(Sys.Date(),"-","_"),
                   "_hh_cleaned_data_all.csv"))
  
  write.csv(indv_clean_data, 
            paste0("03 - Outputs/02_clean_data/",
                   str_replace_all(Sys.Date(),"-","_"),
                   "_indv_cleaned_data.csv"))

}


# Create Composite Variables (individual data only) ------------------------

# Set up to add columns:
indv_clean_w_comp <- indv_clean_data %>% dplyr::mutate(
  
  # Add column of age group:
  age_group = case_when(age_of_child %in% 3:10 ~ "age_group_3_10",
                        age_of_child %in% 11:14 ~ "age_group_11_14",
                        age_of_child %in% 15:18 ~ "age_group_15_18",
                        age_of_child %in% 19:24 ~ "age_group_19_24"),
  
  # Add an age & gender column:
  gender_age_group = case_when(girl_3_10 >0 ~ "girl_3_10",
                               girl_11_14 >0 ~ "girl_11_14",
                               girl_15_18 >0 ~ "girl_15_18",
                               boy_3_10 >0 ~ "boy_3_10",
                               boy_11_14 >0 ~ "boy_11_14",
                               boy_15_18 >0 ~ "boy_15_18",
                               girl_19_24 >0 ~ "girl_19_24",
                               boy_19_24 >0 ~ "boy_19_24"),
  
  # Add a disabled seeing column:
  disabled_seeing = if_else(
    difficulties_with_seeing %in% c("lot_of_difficulty", "cannot_do_at_all"),
    true = "yes", false = "no", missing = NULL),
  
  # Add a disabled hearing column:
  disabled_hearing = if_else(
    difficulties_with_hearing %in% c("lot_of_difficulty", "cannot_do_at_all"),
    true = "yes", false = "no", missing = NULL),
  
  # Add a disabled climbing column:
  disabled_climbing = if_else(
    difficulties_with_climbing %in% c("lot_of_difficulty", "cannot_do_at_all"),
    true = "yes", false = "no", missing = NULL),
  
  # Add a disabled remembering column:
  disabled_remembering = if_else(
    difficulity_remembering %in% c("lot_of_difficulty", "cannot_do_at_all"),
    true = "yes", false = "no", missing = NULL),
  
  # Add a disabled self care column:
  disabled_self_care = if_else(
    difficulty_with_self_care %in% c("lot_of_difficulty", "cannot_do_at_all"),
    true = "yes", false = "no", missing = NULL),
  
  # Add a disabled communication care column:
  disabled_communication = if_else(
    difficulity_communicating %in% c("lot_of_difficulty", "cannot_do_at_all"),
    true = "yes", false = "no", missing = NULL),
  
  # Add a columns for when education level does not change:
  education_level_promotion_unchanged = if_else(
    education_receive_in_2019_camp == level_of_education_camp,
    true = "yes", false = "no", missing = NULL)
  
)

# Add in columns for the education level promotions:
prom_catagories = paste0(indv_clean_w_comp[,"education_receive_in_2019_camp"], "_to_", indv_clean_w_comp[,"level_of_education_camp"])
for(p in unlist(unique(prom_catagories))){indv_clean_w_comp[[paste0("education_level_promotion_",p)]] = c("no","yes")[(prom_catagories %in% p) +1]}


# Add on the Upazila column from the hh data using UUID as a match:
indv_clean_w_comp = 
  left_join(x = indv_clean_w_comp, 
            y = hh_clean_consent[,c("X_uuid", "upazila", "group_of_caregivers")],
            by = "X_uuid")

# Write Composite Indicators ----------------------------------------------

if(write_output == "yes"){
  
  write.csv(x = indv_clean_w_comp,
            file = paste0("03 - Outputs/03_composite_indicators_with_data/",
                          "Individual data - cleaned data with composite indicators - ",
                          str_replace_all(Sys.Date(),"-","_"), ".csv"))
}



# # Tidy the workspace:
# rm(list=ls()[! ls() %in% c("calc_weight", "weighting_hc", "weighting_camp",
#                            "hh_clean_consent", "indv_clean_w_comp", "kat_split_by_conjunction")])

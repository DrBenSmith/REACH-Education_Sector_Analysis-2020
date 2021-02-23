#---------------------------------
# Education Sector - Care Givers
# Analysis Script
#---------------------------------
# Ben.Smith@reach.initiative.org
# 07/12/2020
#-----------


# Preamble ----------------------------------------------------------------

library(openxlsx)
library(dplyr)
library(srvyr)
library(butteR)
library(stringr)

library(illuminate)
library(AMR)
library(tidyr)

# Function: Split a vector of strings by "AND" and "&" and trim white space:
kat_split_by_conjunction = function(string_vector){
  spit_string_vector = gsub(pattern = c("AND"), replacement = ",", x = string_vector)
  spit_string_vector = gsub(pattern = c("&"), replacement = ",", x = spit_string_vector)
  spit_string_vector = gsub(pattern = c(" "), replacement = "", x = spit_string_vector)
  spit_string_vector = unique(strsplit(x = spit_string_vector, split = ","))
  return(spit_string_vector)
}

# Set the directory:
setwd("C:/Users/Ben SMITH/Documents/Bangladesh/REACH Projects/REACH-Education_Sector_Analysis-2020/")

# Either create the cleaned data:
# source("Scripts/Care Givers Cleaning Script v4.R") # This could do with being updated to output CSVs instead of xlxs as loading in data as an xlsx seems to crash the butteR functions. Below, the data has been coppied into csvs manually from the xlsx files.

# Or load it in directly:
hh <- read.csv(
  file = "03 - Outputs/02_clean_data/2021_02_18_hh_cleaned_data_with_consent.csv",
  na.strings = c(""," ","NA","N/A"),
  stringsAsFactors = FALSE)

indv = read.csv(
  file = "03 - Outputs/03_composite_indicators_with_data/Individual data - cleaned data with composite indicators - 2021_02_18.csv",
  na.strings = c(""," ","NA","N/A"),
  stringsAsFactors = FALSE)

# Read in the Data Analysis Plan:
DAP <- read.xlsx(xlsxFile = "07 - Other/CAREGIVER-SURVEY_DA instructions_v3.xlsx",
                 sheet = "Reduced", na.strings = c(""," ","NA","N/A"),
                 colNames = TRUE, rowNames = FALSE)

# Do you want the results to be collated onto a single page?
collate = FALSE # or TRUE # Not yet fully coded.

# Weightings --------------------------------------------------------------
  
  # Households in host community:
  hh_number <- read.csv("02 - Inputs/Weightings - number of households summary.csv",
                        na.strings = c(""," ","NA","N/A"))
  
  # Add a column into the hh data that you can use to join to the weightings data. 
  hh <- hh %>% mutate(strata = paste0(group_of_caregivers ,"_", upazila))
  
  # Get the number of samples for each hh strata: 
  sample_frame <- hh %>% group_by(strata) %>% dplyr::summarise(sample_size = n())
  sample_frame <- left_join(x = sample_frame, y = hh_number)
  
  # Create the weighting variables for host communities:
  sf_with_weights_host <- sample_frame %>% dplyr::filter(Care_Giver == "host_communities")
  sf_with_weights_host <- sf_with_weights_host %>% dplyr::mutate(
    sample_global = sum(sample_size),
    pop_global = sum(Total_HH),
    survey_weight = (Total_HH/pop_global)/(sample_size/sample_global)
  )
  
  # Create the weighting variables for camps:
  sf_with_weights_camp <- sample_frame %>% dplyr::filter(Care_Giver == "camps")
  sf_with_weights_camp <- sf_with_weights_camp %>% dplyr::mutate(
    sample_global = sum(sample_size),
    pop_global = sum(Total_HH),
    survey_weight = (Total_HH/pop_global)/(sample_size/sample_global)
  )
  
  # Join the two above tables together:
  sample_frame <- bind_rows(sf_with_weights_host, sf_with_weights_camp) 
  
  # Write the weightings for reference:
  write.csv(sample_frame, "03 - Outputs/02_clean_data/weights.csv")
  
  

# Add weightings to the data: ---------------------------------------------

  # Crop down the weightings so that they only join what is needed:
  sample_frame <- sample_frame%>% select(strata, survey_weight)
  
  # Join the household data to their weightings:
  hh_with_weights <- hh %>% left_join(sample_frame)
  
  # Join the individual data to weightings using the household data, as this is simplest:
  indv_with_weights <- left_join(x = indv, y = (hh_with_weights %>% select(X_uuid, strata, survey_weight)), by = "X_uuid")
  
  

# Create survey objects ---------------------------------------------------

  hh_svy <- as_survey(hh_with_weights, strata = strata, weights = survey_weight)
  indv_svy <- as_survey(indv_with_weights, strata=strata, weights = survey_weight)
  

# Define which columns should and should not be analysed ------------------

  hh_cols_not_to_analyse <- c("survey_date", "survey_start", "deviceid", 
                              "end_survey", "instance_name","end_note", "end",
                              "audit", "enumerator_id", "kii_id", "int_note", 
                              "informed_consent", "start","gps_reading",
                              "X_gps_reading_longitude", "X_gps_reading_altitude", 
                              "X_gps_reading_precision", "X_gps_reading_latitude",
                              "X_id", "X_uuid", "X_submission_time", 
                              "X_validation_status","X_index", "strata", 
                              "survey_weight")
  
  hh_cols_to_analyse <- hh_with_weights %>% select(-ends_with("_other"),
                                                   -starts_with("other_"),
                                                   -hh_cols_not_to_analyse) %>% names() 
  
  
  indv_cols_not_to_analyse <- c("strata", "survey_weight","X_index", 
                                "X_parent_table_name", "X_parent_index", 
                                "X_submission__id",  "X_uuid", 
                                "X_submission__submission_time",
                                "X_submission__validation_status")
  
  
  indv_cols_to_analyse <- indv_with_weights %>% select(-ends_with("_other"),
                                                   -starts_with("other_"),
                                                   -indv_cols_not_to_analyse) %>% names() 


# Calculate the proportions and means -------------------------------------

  # Create empty lists to take the analysis results if needed:
  hh_results = list()
  indv_results = list()

  # Household Analysis:
  
  # Find the groups to analyse with:
  group_names = unique(trimws(unlist(strsplit(x = DAP$Analysis_Level[DAP$Group=="household"],
                                              split = ","))))
  
  # Convert analysis levels that contain "AND"s and "&"s into a list of vectors: 
  groups = kat_split_by_conjunction(group_names)
  
  # Set up a loop to run through the data and fill the list:
  for(i in 1:length(group_names)){
    
    print(paste0(i, ". -------- ", group_names[i], " --------"))
    
    if(all(groups[[i]] == "overall")){
      x = mean_prop_working(design = hh_svy,
                            list_of_variables = hh_cols_to_analyse)
      
    } else {
      x = mean_prop_working(design = hh_svy,
                            list_of_variables = hh_cols_to_analyse,
                            aggregation_level = groups[[i]])
    }
    
    x <- as.data.frame(t(x))
    colnames(x) <- rep(group_names[i], ncol(x))

      # Assign the result to a tab. Number the tab as some cropped names are identical)
      hh_results[[paste0(i, "_", substr(x = group_names[i], start=1, stop = 28))]] =  x
      
      # Paste the group names into the sheet:
      if(i == length(group_names)){
        hh_results[["groupings"]] = group_names
        hh_results[["input data"]] = hh_svy$variables
        }
    
    
  }
  
  write.xlsx(hh_results,  paste0("03 - Outputs/04_basic_analysis/", str_replace_all(Sys.Date(),"-","_"), "_hh_analysis.xlsx"), row.names = TRUE)
  
  
  # Individual Analysis:
  
  # Find the groups to analyse with:
  group_names = unique(trimws(unlist(strsplit(x = DAP$Analysis_Level[DAP$Group=="individual"],
                                              split = ","))))
  
  # Convert analysis levels that contain "AND"s and "&"s into a list of vectors: 
  groups = kat_split_by_conjunction(group_names)
  
  # Set up a loop to run through the data and fill the list:
  for(i in 1:length(group_names)){
    
    print(paste0(i, ". -------- ", group_names[i], " --------"))

    if(all(groups[[i]] == "overall")){x = mean_prop_working(design = indv_svy,
                                                            list_of_variables = indv_cols_to_analyse)

    } else {x = mean_prop_working(design = indv_svy,
                                  list_of_variables = indv_cols_to_analyse,
                                  aggregation_level = groups[[i]])}
    
    x <- as.data.frame(t(x))
    colnames(x) <- rep(group_names[i], ncol(x))
    
    # Assign the result to a tab. Number the tab as some cropped names are identical)
    indv_results[[paste0(i, "_", substr(x = group_names[i], start=1, stop = 28))]] =  x
    
    # Paste the group names into the sheet:
    if(i == length(group_names)){
      indv_results[["groupings"]] = group_names
      indv_results[["input data"]] = indv_svy$variables}
  
  }
  
  write.xlsx(indv_results, 
             paste0("03 - Outputs/04_basic_analysis/", str_replace_all(Sys.Date(),"-","_"), "_indv_analysis.xlsx"), 
             row.names = TRUE)

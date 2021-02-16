
# Household Response Data:
hh <- read.csv("02 - Inputs/Household Surveys - BGD2006_Education_Caregivers_Final - all_versions_-_False_-_2021-01-31-10-10-59.csv", 
               check.names =FALSE,
               na.strings = c(""," ","NA","N/A"), 
               stringsAsFactors = F) %>% rename("X_uuid" = "_uuid")

# Individual Response Data:
indv <- read.csv("02 - Inputs/Individual Surveys - BGD2006_Education_Caregivers_Final - all_versions_-_False_-_2021-01-31-10-10-59.csv", 
                 check.names =FALSE,
                 na.strings = c("", " ", "NA", "N/A"), 
                 stringsAsFactors = F) %>% rename("X_uuid" = "_submission__uuid")

# Check that the number of "youth_3_24" matches the number of individual responses:

  # Crop down the hh responses:
  y = hh[,c("X_uuid","youth_3_24")]
  y = y[!is.na(y$youth_3_24),]
  
  # Select the individial columns you're after
  cols_for_counting = colnames(indv)[5:12]
  
  # Add some emplty columns to the hh data to fill with summed indv data:
  y = cbind(y, matrix(ncol = length(cols_for_counting),nrow = nrow(y)))
  colnames(y) = c("X_uuid","youth_3_24",cols_for_counting)
  
  # Run through each UUID and add the individual age/gender counts to the hh data:
  for(r in 1:nrow(y)){
    x = which(indv$X_uuid == y$X_uuid[r])
    z = indv[x,cols_for_counting]
    z = colSums(z)
    y[r,3:ncol(y)] = z
  }
  
  # Make a summary column:
  y = cbind(y,rowSums(y[,3:ncol(y)]))
  
  # Print out all the instances where the indv count != hh response
  y[which(!y$youth_3_24 == y$indv_sum),]

# source("https://bioconductor.org/biocLite.R")
# biocLite("RTCGA.clinical")

# get the data
library(RTCGA.clinical)
LUAD <- LUAD.clinical[,c("patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment",
                 "patient.days_to_death",
                 "patient.days_to_last_followup",
                 "patient.gender")]

# clean colnames
colnames(LUAD) <- c("new_tumor_event", "days_to_death", "days_to_last_followup", "gender")
  
# create new variables
LUAD$event <- ifelse(!is.na(LUAD$new_tumor_event), "new_tumor",
                     ifelse(!is.na(LUAD$days_to_death), "death",
                     "alive"))
LUAD$time <- pmin(as.numeric(LUAD$new_tumor_event), 
                  as.numeric(LUAD$days_to_death), 
                  as.numeric(LUAD$days_to_last_followup), na.rm = TRUE)

# use the gender as grouping variable

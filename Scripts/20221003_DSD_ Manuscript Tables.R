#LIBRARIES ---------------------------------------

library(tidyverse)
library(glamr)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(glue)
library(janitor)
library(ggtext)
library(extrafont)
library(waffle)
library(patchwork)
library(readxl)
library(readr)
library(gt)
library(table1)
library(furniture)


table1


# IMPORT -----------------------------------------------

#KRS
df <- read_csv("Dataout/20220223-DSD-preference-clean.csv")


si_path() %>% 
  return_latest("20220921-DSD-preference-clean") %>% 
  read_msd()

df <- readr::read_csv("Dataout/20220921-DSD-preference-clean.csv")

df <- read_csv("Uganda/Dataout/20220921-DSD-preference-clean.csv")
View(df)

# MAKE TABLES ------------------------------------------

#Recode
df$missed_appt <- 
  factor(df$missed_appt, 
         labels=c("No Missed Appointment", # Reference
                  "Missed Appointment"))
 
df$vls <- 
  factor(df$vls, 
         labels=c("Not Virally Suppressed", # Reference
                  "Virally Suppressed"))

df$is_current_mode_preferred <- 
  factor(df$is_current_mode_preferred,
         labels = c("Not in preferred ARV Mode",
                    "In preferred ARV Mode"))

# ADd labels 
 label(df$vls)        <- "Viral load Suppression"
 label(df$gender)          <- "Gender"
 label(df$agecoarse)          <- "Agecoarse"
 label(df$region)          <- "Region"
 label(df$missed_appt)          <- "Missed Apointments in the last 12 months"
 label(df$current_arv_mode)          <- "Current ARV Mode"
 label(df$CHW_present)          <- "Community Health worker Present"
 label(df$preferred_DSD)          <- "Preferred ARV Mode"
 label(df$is_current_mode_preferred)          <- "ARV Mode Preference"
 #Table 1
# 
#  df$gender <- 
#    factor(df$gender,
#           labels = c("Female",
#                      "Male",
#                      "NA"))
#  
# 
#  
#  
#  df$missed_appt <- 
#    factor(df$missed_appt, 
#           labels=c("No Missed Appointment", # Reference
#                    "Missed Appointment"))
#  
#  df$vls <- 
#    factor(df$vls, 
#           labels=c("Not Virally Suppressed", # Reference
#                    "Virally Suppressed"))
#  label(df$missed_appt)          <- "Missed Apointments"
#  label(df$vls)        <- "Viral load Suppression"

#Table 1 -demographics
table1(~ agecoarse + agecoarse + gender + region + missed_appt
+ current_arv_mode + is_current_mode_preferred + vls, data = df, caption = "Table 1: Baseline Cohort Demographics")


# df$vls <- 
#   factor(df$vls, 
#          labels=c("Not Virally Suppressed", # Reference
#                   "Virally Suppressed"))
# 
# df$missed_appt <- 
#   factor(df$missed_appt, 
#          labels=c("No Missed Appontment", # Reference
#                   "Missed Appointment"))

#df[df==0] <- NA

#Table 2 and 3 - outcomes
table1(~ agecoarse + gender  + is_current_mode_preferred + current_arv_mode | vls, data=df, caption = "Table 2: Viral Suppression Outcomes by ...")
table1(~ agecoarse + gender +  is_current_mode_preferred + current_arv_mode | missed_appt, data=df, caption = "Table 3: Missed Appointment (in the last 12 months) Outcomes by ...")
  

# tab1 <-furniture::table1(data = df, ~ agecoarse + gender +  is_current_mode_preferred + current_arv_mode | ,
#        row_wise = TRUE)
# 
# 
#   tab1 <- furniture::table1(df,
#                     agecoarse, gender, is_current_mode_preferred, current_arv_mode,
#                     splitby = ~missed_appt,
#                     row_wise = TRUE)
# kable(tab1)
# print(kable_styling(kable(tab1)))

si_save("table2.svg")

#LIBRARIES ---------------------------------------

library(tidyverse)
library(flextable)
# library(glamr)
# library(glitr)
# library(gophr)
# library(extrafont)
# library(scales)
# library(tidytext)
# library(glue)
# library(janitor)
# library(ggtext)
# library(extrafont)
# library(waffle)
# library(patchwork)
# library(readxl)
# library(readr)
# library(gt)
library(table1)
#library(furniture)



# IMPORT -----------------------------------------------


df <- read_csv("Dataout/20220223-DSD-preference-clean.csv")


# MAKE TABLES ------------------------------------------

#Recode
df$missed_appt <- 
  factor(df$missed_appt, 
         labels=c("No Missed Appointment", # Reference
                  "Missed Appointment"))


#df$region <- gsub("UPMB/LSDA", "UPMB/LSDA(Uganda Protestant Medical Bureau Local Service Delivery for Health and HIV/AIDS)", df$region)


# ADd labels 
label(df$vls)        <- "Viral Load Suppression"
label(df$gender)          <- "Sex"
label(df$agecoarse)          <- "Age Category"
label(df$region)          <- "Region"
label(df$missed_appt)          <- "Missed Apointments in the last 12 months"
label(df$current_arv_mode)          <- "Current ARV Mode"
label(df$CHW_present)          <- "Community Health worker Present"
label(df$preferred_DSD)          <- "Preferred ARV Mode"
label(df$is_current_mode_preferred)          <- "ARV Mode Preference"
label(df$current_age)          <- "Age"

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
tbl1 <- table1(~  current_age + agecoarse + gender + region + missed_appt
               + current_arv_mode + is_current_mode_preferred + vls, data = df,
               footnote = "UPMB/LSDA refers to Uganda Protestant Medical Bureau Local Service Delivery for Health and HIV/AIDS Activity, a partner organization that was analyzed as a region because they work with all private not-for-profit institutions.",
               #"CCLAD =Community Client-Led ART delivery,
               #        CDDP = Community Drug Distribution Points,
               #        FBG = Facility Based Group,
               #        FBIM = Facility Based Individual Management,
               #        FTDR = Fast-Track Drug Refills",
               caption = "Table 1: Baseline Cohort Demographics")


# Convert to flextable
t1flex(tbl1) %>% 
  save_as_docx(path="Images/table1.docx")





#filter out missing values
df1 <- df %>% 
  filter(!is.na(vls),
         !is.na(missed_appt))

#Table 2 - outcomes by vls
tbl2 <- table1(~ agecoarse + gender  + is_current_mode_preferred + current_arv_mode | vls, data=df1,
               row_wise = FALSE,
               # footnote = "CCLAD =Community Client-Led ART delivery,
               # CDDP = Community Drug Distribution Points,
               # FBG = Facility Based Group,
               # FBIM = Facility Based Individual Management,
               # FTDR = Fast-Track Drug Refills",
               caption = "Table 2: Viral Suppression Outcomes by by Demographic and Clinical Characteristics")

# Convert to flextable
t1flex(tbl2) %>% 
  save_as_docx(path="Images/table2.docx")

#Table 3 - outcomes by missed appts
tbl3 <- table1(~ agecoarse + gender +  is_current_mode_preferred + current_arv_mode | missed_appt, data=df1,
               # footnote = "CCLAD =Community Client-Led ART delivery,
               # CDDP = Community Drug Distribution Points,
               # FBG = Facility Based Group,
               # FBIM = Facility Based Individual Management,
               # FTDR = Fast-Track Drug Refills",
               caption = "Table 3: Missed Appointment Outcomes (in the last 12 months) by Demographic and Clinical Characteristics")


# Convert to flextable
t1flex(tbl3) %>% 
  save_as_docx(path="Images/table3.docx")

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


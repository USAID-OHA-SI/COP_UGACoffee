## PROJECT: COP_UGA
## AUTHOR:  K. Srikanth | USAID
## PURPOSE: DSD Analysis - descriptive analysis viz
## LICENSE: MIT
## DATE:    2022-02-23


# DEPENDENCIES ------------------------------------------------------------

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

# IMPORT AND GLOBALS  ------------------------------------------------------------

df <- si_path() %>%
  return_latest("DSD_patient_reference_export 16th Feb 22") %>% 
  read_csv(skip =2) %>% 
  clean_names()

#rename
  names(df) <- c('organisation', "client_id", "current_age", "gender", "yearHIVdiag", "tx_duration", "updated_vl", "vls", "missed_appt",
    'person_administered', "MMDfreq_last_visit", "preferred_MMDfreq", "current_arv_mode", "is_current_mode_preferred", "preferred_DSD",
    'other_preferred_method', "CHW_present", "CHW_followup", "contact_followup")
  
#current year
  curr_year <- 2022
  
  str(df)
  
  df %>% 
    select(yearHIVdiag, tx_duration)

#TIDY -------------------------------------------------------------------------------
 
#separate into region/snu/psnu/facility 
  df <- df %>% 
    separate(organisation, sep = " / ", into = c("region", "snu", "psnu", "facility"))
  
  table(df$region)
  
#recode as factors
  factor_vars <- c(7, 9:18, 20:22) #everything but region-facility, client_id, age, yearHIVdiag and other_preferred_method
  
  df[,factor_vars] <- lapply(df[,factor_vars] , factor) 
  str(df)
  
  
#change to lowercase
  df[, factor_vars] <- apply(df[, factor_vars], 2, str_to_title)
  
#recode names
 df_clean <- df %>% 
    mutate(tx_duration = recode(tx_duration, "< 3mo" = "Less than 3 months",
                               "3mo <> 6mo" = "Between 3 and 6 months",
                               "6mo <> 1yr" = "Between 6 months and a year",
                               "> 1yr" = "More than a year"),
           MMDfreq_last_visit = recode(MMDfreq_last_visit, "< 1mo" = "Less than 1 month",
                                "1mo <> 2mo" = "1-2 months",
                                "3mo <> 5mo" = "3-5 months",
                                "> 6mo" = "More than 6 months"),
           preferred_MMDfreq = recode(preferred_MMDfreq, "< 1mo" = "Less than 1 month",
                                       "1mo <> 2mo" = "1-2 months",
                                       "3mo <> 5mo" = "3-5 months",
                                       "> 6mo" = "More than 6 months"),
           current_arv_mode = recode(current_arv_mode, "Cclad" = "Community client-led ART delivery (CCLAD)",
                                      "Cddp" = "Community Drug Distribution Point (CDDP)",
                                      "Cph" = "Community Pharmacy",
                                      "Fbg" = "Facility-based group (FBG)",
                                      "Fbim" = "Facility-based individual management (FBIM)",
                                      "Ftdr" = "Fast track refill (FTDR)"),
           preferred_DSD = recode(preferred_DSD, "Cclad" = "Community client-led ART delivery (CCLAD)",
                                     "Cddp" = "Community Drug Distribution Point (CDDP)",
                                     "Cph" = "Community Pharmacy",
                                     "Fbg" = "Facility-based group (FBG)",
                                     "Fbim" = "Facility-based individual management (FBIM)",
                                     "Ftdr" = "Fast track refill (FTDR)"))
#relevel
 df_clean <-  df_clean %>% 
    mutate(tx_duration = fct_relevel(tx_duration, c("Less than 3 months", "Between 3 and 6 months",
                                                    "Between 6 months and a year", "More than a year")),
           MMDfreq_last_visit = fct_relevel(MMDfreq_last_visit, c("Less than 1 month", "1-2 months",
                                                   "3-5 months", "More than 6 months")),
           preferred_MMDfreq = fct_relevel(MMDfreq_last_visit, c("Less than 1 month", "1-2 months",
                                                                  "3-5 months", "More than 6 months")),
           current_arv_mode = fct_relevel(current_arv_mode, c("Facility-based individual management (FBIM)", "Facility-based group (FBG)",
                                                              "Fast track refill (FTDR)", "Community client-led ART delivery (CCLAD)",
                                                              "Community Drug Distribution Point (CDDP)", "Community Pharmacy")),
           preferred_DSD = fct_relevel(preferred_DSD, c("Facility-based individual management (FBIM)", "Facility-based group (FBG)",
                                                              "Fast track refill (FTDR)", "Community client-led ART delivery (CCLAD)",
                                                              "Community Drug Distribution Point (CDDP)", "Community Pharmacy"))) 
 
 
#use agecoarse age bands and create age by 5-year age bands 
 
 df_clean <- df_clean %>% 
   mutate(agecoarse = ifelse(current_age <15, "<15", "15+"),
          age_group = cut(df$current_age,
                          breaks = c(0,4,9,14,19,24,29,34,39,44,49,100),
                          labels = c("0-04","05-09","10-14","15-19", "20-24", "25-29", "30-34", "35-39","40-44", "45-49","50+"))) 

#create var of HIV diagnosis time based on diagnosis year
 
 df_clean <- df_clean %>% 
   mutate(yrs_with_HIV = curr_year - yearHIVdiag) %>% 
   mutate(yrs_with_HIV_grouped = cut(yrs_with_HIV,
                                     breaks = c(0,10,20,40),
                                     labels = c("01-10","11-20","20+")))
 
 #DESCRIPTIVE ANALYTICS ---------------------------------------------------
 
 #  What is the frequency of survey participants ?
 
 ggplot(df_clean) +
   geom_bar(mapping = aes(x = gender))
 
 df_clean %>% count(gender, age_group) %>%
   drop_na() %>% 
   #filter(!is.na(gender) & !is.na(age_group)) %>% 
   mutate(fill_color = ifelse(gender == "Male", genoa, moody_blue)) %>% 
   ggplot(aes(x = age_group, y = ifelse(gender == "Male", -n, n), fill = fill_color), na.rm = TRUE) +
   geom_bar(stat = "identity") +
   geom_text(aes(label = n, hjust = ifelse(gender == "Male", 1.2, -.4)),
             family = "Source Sans Pro", size = 4, color = "#505050") +
   coord_flip() + 
   scale_fill_identity() +
   si_style_nolines() +
     labs(x = NULL, y = NULL,
          title = "The patient cohort comprises of more <span style = 'color:#8980cb'>women</span> than  <span style = 'color:#287c6f'>men</span> and mostly patients over the age of 50",
          subtitle = "Uganda COP22 DSD Analysis | USAID",
          caption = "Source: DSD Patient Reference Export, 16 Feb 2022") + 
   theme(legend.position = "none",
         panel.grid.major.y = element_blank(),
         axis.text.x = element_blank(),
         plot.title = element_markdown()
   )
 
 si_save("COP22_UGA_survey-participants-age-sex.png", path = "Images")
 
 #  What is ITT, VLC, VLS results/% for ped vs Adult 
 
 #removed missing values
 df_iit_prct <- df_clean %>% 
   count(gender, age_group, missed_appt) %>% drop_na() %>% 
   pivot_wider(names_from = missed_appt, values_from = n) %>% 
   mutate(total = No + Yes,
          IIT = Yes / total,
          fill_color = ifelse(gender == "Male", genoa, moody_blue))
 
 df_iit_prct %>% 
   ggplot(aes(IIT, age_group)) +
   geom_path(color = "gray50") +
   geom_point(aes(size = total), fill = "white") +
   geom_point(aes(size = total, fill = gender, color = "white"), shape = 21) +
   geom_text(aes(label = percent(IIT, 1)), family = "Source Sans Pro", 
             color = trolley_grey, hjust = -0.8, na.rm = TRUE) +
   scale_fill_manual(values = c("Male" = genoa, "Female" = moody_blue)) +
   scale_color_identity() +
   scale_x_continuous(label = percent) +
   scale_size_continuous(range = c(3, 10)) +
   #expand_limits(x = 1) +
   si_style_xgrid() +
   labs(y = NULL,
        title = "Share of patients with missed appointments in the last 12 months, across age and sex",
        subtitle = "Uganda COP22 DSD Analysis | USAID",
        caption = "Estimated IIT Proxy = Missed appointment in the last 12 months
           Source: DSD Patient Reference Export, 16 Feb 2022"
   ) +
   theme(
         panel.grid.major.y = element_blank(),
         axis.text.x = element_blank(),
         plot.title = element_markdown()
   )
 
 si_save("COP22_UGA_itt-share1.svg", path = "Graphics")
 
   
 
 x#  What is ITT result by factors ( region, sex. age group etc )?
 #  What is  VLC result by factors ( region, sex. age group etc )?
 #  What is VLS result by factors ( region, sex. age group etc )?
 #  What is the most preferred method of ARV dispensation?
 #  How many patients are receiving  their ARVs  through their most prefered method of ARV dispensation?
 
 ggplot(df_clean) +
   geom_bar(mapping = aes(x = preferred_DSD))
 
 df_clean %>% 
   filter(is_current_mode_preferred == "No") %>% 
   ggplot() +
   geom_bar(mapping = aes(x = preferred_DSD))
 
 #  How do the clinical health outcomes differ by age group( Peds and Adults)
 
 mosaicplot(age_group ~ vls ,data = df_clean)
 mosaicplot(agecoarse ~ vls ,data = df_clean)
 
 #  How do clinical outcomes differ by current method of ARV dispensation?
 #  how does IIT differ by region and by age?
 
 df_clean %>% 
   ggplot(aes(region, missed_appt)) + 
   geom_col() + 
   scale_y_continuous(labels = label_number_si())
 
 ggplot(df_clean) +
   geom_bar(mapping = aes(x = region, y = missed_appt))
 
 
 df_clean %>% 
   count(gender, MMDfreq_last_visit)
 
 
 
 

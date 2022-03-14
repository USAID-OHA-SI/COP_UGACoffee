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
library(extrafont)
library(patchwork)
library(waffle)

#IMPORT ------------------------------------------------------------------

#Run tidying script and save to /Dataout folder

df_clean <- read_csv("Dataout/20220223-DSD-preference-clean.csv")

#DESCRIPTIVE ANALYTICS ---------------------------------------------------

#  What is the frequency of survey participants ? -------------

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
       title = "The participant cohort (n = 6,376) comprises of more <span style = 'color:#8980cb'>women</span> than  <span style = 'color:#287c6f'>men</span> and mostly patients over the age of 50",
       subtitle = "Uganda COP22 Patient Preference Analysis | USAID",
       caption = "Source: DSD Patient Reference Export, 16 Feb 2022") + 
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_markdown()
  )

si_save("COP22_UGA_survey-participants-age-sex.png", path = "Images")

#  IIT by age/sex ---------

df_iit_prct <- df_clean %>% 
  count(gender, age_group, missed_appt) %>% drop_na() %>% 
  pivot_wider(names_from = missed_appt, values_from = n) %>% 
  mutate(total = No + Yes,
         IIT = Yes / total,
         fill_color = ifelse(gender == "Male", genoa, moody_blue))

df_iit_prct %>% 
  #drop_na() %>% 
  summarise(Yes = sum(Yes, na.rm = TRUE), .groups = "drop") 

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
       title = "Share of patients (n = 6215) with missed appointments in the last 12 months, across age and sex",
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


#  VLC by age/sex? -------------

df_vlc <- df_clean %>% 
  count(gender, age_group, updated_vl) %>% drop_na() %>% 
  pivot_wider(names_from = updated_vl, values_from = n) %>% 
  mutate(total = No + Yes,
         VLC = Yes / total,
         fill_color = ifelse(gender == "Male", genoa, moody_blue))

df_vlc %>% 
  #drop_na() %>% 
  summarise(Yes = sum(Yes, na.rm = TRUE), .groups = "drop") 

df_vlc %>% 
  ggplot(aes(VLC, age_group)) +
  geom_path(color = "gray50") +
  geom_point(aes(size = total), fill = "white") +
  geom_point(aes(size = total, fill = gender, color = "white"), shape = 21) +
  geom_text(aes(label = percent(VLC, 1)), family = "Source Sans Pro", 
            color = trolley_grey, hjust = -0.8, na.rm = TRUE) +
  scale_fill_manual(values = c("Male" = genoa, "Female" = moody_blue)) +
  scale_color_identity() +
  scale_x_continuous(label = percent) +
  scale_size_continuous(range = c(3, 10)) +
  #expand_limits(x = 1) +
  si_style_xgrid() +
  labs(
    title = "Share of patients (n=6246) with updated viral loads, across age and sex",
    subtitle = "Uganda COP22 Patient Preference Analysis | USAID",
    caption = "Estimated IIT Proxy = Missed appointment in the last 12 months
           Source: DSD Patient Reference Export, 16 Feb 2022"
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_markdown()
  )

si_save("COP22_UGA_vlc-share1.svg", path = "Graphics")


#  VLS by age/sex?

#removed missing values

df_vls <- df_clean %>% 
  count(gender, age_group, vls) %>% drop_na() %>% 
  pivot_wider(names_from = vls, values_from = n) %>% 
  mutate(total = No + Yes,
         VLS = Yes / total,
         fill_color = ifelse(gender == "Male", genoa, moody_blue))

df_vls %>% 
  #drop_na() %>% 
  summarise(Yes = sum(Yes, na.rm = TRUE), .groups = "drop") 

df_vls %>%
  ggplot(aes(VLS, age_group)) +
  geom_path(color = "gray50") +
  geom_point(aes(size = total), fill = "white") +
  geom_point(aes(size = total, fill = gender, color = "white"), shape = 21) +
  geom_text(aes(label = percent(VLS, 1)), family = "Source Sans Pro",
            color = trolley_grey, hjust = -0.8, na.rm = TRUE) +
  scale_fill_manual(values = c("Male" = genoa, "Female" = moody_blue)) +
  scale_color_identity() +
  scale_x_continuous(label = percent) +
  scale_size_continuous(range = c(3, 10)) +
  #expand_limits(x = 1) +
  si_style_xgrid() +
  labs(y = NULL,
       title = "Share of participants (n = 6112) with suppressed viral loads, across age and sex",
       subtitle = "Uganda COP22 Patient Preference Analysis | USAID",
       caption = "Estimated VLS Proxy = Missed appointment in the last 12 months
           Source: DSD Patient Reference Export, 16 Feb 2022"
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_markdown()
  )

si_save("COP22_UGA_vls-share1.svg", path = "Graphics")


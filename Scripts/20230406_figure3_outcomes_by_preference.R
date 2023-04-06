## PROJECT: COP_UGA
## AUTHOR:  K. Srikanth | USAID
## PURPOSE: DSD Analysis - figure 3 manuscript; outcomes by preference
## LICENSE: MIT
## DATE:    2022-004-06


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

# MUNGE-----------------------------------------------------------------------

preference_vls <- df_clean %>% 
  filter(!is.na(vls),
         !is.na(missed_appt)) %>% 
  count(is_current_mode_preferred, vls) %>%
  pivot_wider(names_from = vls, values_from = n) %>% 
  mutate(total = No + Yes,
         pct = Yes / total) %>% 
  mutate(prct_round_vls = round(pct*100),
         is_current_mode_preferred = recode(is_current_mode_preferred,
                                            "No" = "Not in preferred DSD model",
                                            "Yes" = "In preferred DSD model")) %>% 
  pivot_longer(c(prct_round_vls), names_to = "status") %>% 
  mutate(fill_color =ifelse(is_current_mode_preferred == "Not in preferred DSD model",
                            trolley_grey_light, denim),
         indicator = "Had suppressed viral load")

df_viz_iit <-  df_clean %>% 
  filter(!is.na(vls),
         !is.na(missed_appt)) %>% 
  #filter(gender == "Female") %>% 
  count(is_current_mode_preferred, missed_appt) %>%
  pivot_wider(names_from = missed_appt, values_from = n) %>% 
  mutate(total = No + Yes,
         pct = Yes / total) %>%
  mutate(prct_round_iit = round(pct*100),
         is_current_mode_preferred = recode(is_current_mode_preferred,
                                            "No" = "Not in preferred DSD model",
                                            "Yes" = "In preferred DSD model")) %>% 
 # pivot_wider(names_from = "is_current_mode_preferred", values_from = "prct_round_iit")
  pivot_longer(c(prct_round_iit), names_to = "status") %>% 
  mutate(fill_color =ifelse(is_current_mode_preferred == "Not in preferred DSD model",
                            trolley_grey_light, denim),
         indicator = "Had missed appointment in the past 12 months")

# VIZ ------------------------------------------------------------------------

nudge_space <- 0.125 #set nudge for offset bars


viz_all <- df_viz_iit %>% 
  bind_rows(preference_vls) %>% 
  mutate(num_label = str_c("(", `Yes`, "/", total, ")"))
  
  viz_all %>% 
  ggplot(aes(x = indicator, y = value)) +
  # geom_col(aes(y = current_n),fill = trolley_grey_light, alpha = 1, position = position_nudge(x = -nudge_space)) +
    geom_col(aes(y = value, fill = fill_color), position = "dodge") +
    # geom_label(aes(y = 0, label = percent(share, 1), fill = share, 
    #                color = ifelse(share > 0.3, "white", grey90k)), 
    #            vjust = 1.3, 
    #            size = 10/.pt, 
    #            label.size = NA, family = "Source Sans Pro") +
    geom_text(aes(y = value, label = percent(pct, 1)), family = "Source Sans Pro",
              hjust = 1, vjust = -1, na.rm = TRUE) +
    geom_text(aes(y = value, label = num_label), family = "Source Sans Pro",
              hjust = 1, vjust = 2, na.rm = TRUE) +
    # geom_text(aes(y = preferred_n, label = percent(preferred_share, 1)), family = "Source Sans Pro",
    #           hjust = 0, vjust = -1, na.rm = TRUE) +
    #scale_y_continuous(labels = percent) +
    scale_fill_identity() +
    scale_color_identity() +
    si_style_ygrid() +
    labs(x = NULL, y = NULL, title = "VIRAL SUPPRESSION AND MISSED APPOINTMENT OUTCOMES BY DART MODEL PREFERENCE",
         subtitle = "Grey bars represent clients not in preferred DART model",
         caption = "Source: DSD Patient Reference Export, 16 Feb 2022
         Note: Filtered out missing values for viral load suppression and missed appointments") +
    theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))
  
  si_save("Graphics/figure2_manuscript_20220406.svg")
  
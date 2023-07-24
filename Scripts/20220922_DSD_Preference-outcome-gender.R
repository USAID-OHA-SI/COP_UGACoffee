# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Additional DSD preference plots requested by Han
# REF ID:   9acb8460 
# LICENSE:  MIT
# DATE:     2022-09-22
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)


# GLOBAL VARIABLES --------------------------------------------------------

ref_id <- "9acb8460"
today <- lubridate::today()


# IMPORT ------------------------------------------------------------------
#Run tidying script and save to /Dataout folder

df_clean <- read_csv("Dataout/20220223-DSD-preference-clean.csv")


# MUNGE -------------------------------------------------------------------

#  how does IIT differ by region and by age? 

#Regional analysis

library(waffle)

df_preference_iit <- df_clean %>% 
  filter(!is.na(vls),
         !is.na(missed_appt)) %>% 
  #filter(gender == "Female") %>% 
  count(is_current_mode_preferred, missed_appt) %>%
  pivot_wider(names_from = missed_appt, values_from = n) %>% 
  mutate(total = No + Yes,
         IIT = Yes / total) %>%
  mutate(prct_round_iit = round(IIT*100),
         gap = 100-prct_round_iit,
         is_current_mode_preferred = recode(is_current_mode_preferred,
                                            "No" = "Not in preferred DSD model",
                                            "Yes" = "In preferred DSD model")) %>% 
  pivot_longer(c(prct_round_iit, gap), names_to = "status") %>% 
  mutate(fill_color = ifelse(status == "gap", trolley_grey_light, scooter),
         val_lab = glue("IIT (n) - {(Yes)}\nTotal -{(total)}"), 
         full_lab = glue("{is_current_mode_preferred}\n\n{val_lab}"))

viz_preference_itt <- df_preference_iit %>%
  #filter(fundingagency == "USAID") %>% 
  ggplot(aes(fill = fill_color, values = value)) +
  geom_waffle(color = "white", size = 1, n_rows = 10, flip = TRUE, na.rm = TRUE) +
  geom_text(aes(x = 5, y  = 12, label = percent(IIT, 1), color = scooter),
            family = "Source Sans Pro SemiBold", size = 14/.pt) +
  #  facet_wrap(~region, nrow = 1, strip.position = "bottom") +
  facet_wrap(~fct_reorder(full_lab, value,min), nrow = 2, strip.position = "bottom") +
  expand_limits(y = 14) +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_alpha_identity() +
  coord_equal() +
  labs(x= NULL, y = NULL,
       subtitle = "Share of participants with missed appointments in the last 12 months") +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        strip.text.x = element_text(hjust = .5),
        panel.spacing = unit(1, "pt")) +
  guides(fill = guide_legend(reverse = TRUE))

#VLS
preference_vls <- df_clean %>% 
  filter(!is.na(vls),
         !is.na(missed_appt)) %>% 
  count(is_current_mode_preferred, vls) %>%
  pivot_wider(names_from = vls, values_from = n) %>% 
  mutate(total = No + Yes,
         VLS = Yes / total) %>% 
  mutate(prct_round_vls = round(VLS*100),
         gap = 100-prct_round_vls,
         is_current_mode_preferred = recode(is_current_mode_preferred,
                                            "No" = "Not in preferred DSD model",
                                            "Yes" = "In preferred DSD model")) %>% 
  pivot_longer(c(prct_round_vls, gap), names_to = "status") %>% 
  mutate(fill_color = ifelse(status == "gap", trolley_grey_light, moody_blue),
         val_lab = glue("VLS (n) - {(Yes)}\nTotal -{(total)}"), 
         full_lab = glue("{is_current_mode_preferred}\n\n{val_lab}"))

viz_preference_vls <- preference_vls %>%
  #filter(fundingagency == "USAID") %>% 
  ggplot(aes(fill = fill_color, values = value)) +
  geom_waffle(color = "white", size = 1, n_rows = 10, flip = TRUE, na.rm = TRUE) +
  geom_text(aes(x = 5, y  = 12, label = percent(VLS, 1), color = moody_blue),
            family = "Source Sans Pro SemiBold", size = 14/.pt) +
  #  facet_wrap(~region, nrow = 1, strip.position = "bottom") +
  facet_wrap(~fct_reorder(full_lab, value,min), nrow = 2, strip.position = "bottom") +
  expand_limits(y = 14) +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_alpha_identity() +
  coord_equal() +
  labs(x= NULL, y = NULL,
       subtitle = "Share of participants with suppressed viral loads"
       #caption = glue("Source: DSD Patient Reference Export, 16 Feb 2022")
  ) +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        strip.text.x = element_text(hjust = .5),
        panel.spacing = unit(1, "pt")) +
  guides(fill = guide_legend(reverse = TRUE))

preference_vlc <- df_clean %>% 
  filter(gender == "Female") %>%
  count(is_current_mode_preferred, updated_vl) %>% drop_na() %>% 
  pivot_wider(names_from = updated_vl, values_from = n) %>% 
  mutate(total = No + Yes,
         VLC = Yes / total) %>% 
  mutate(prct_round_vlc = round(VLC*100),
         gap = 100-prct_round_vlc,
         is_current_mode_preferred = recode(is_current_mode_preferred,
                                            "No" = "Not in preferred DSD model",
                                            "Yes" = "In preferred DSD model")) %>% 
  pivot_longer(c(prct_round_vlc, gap), names_to = "status") %>% 
  mutate(fill_color = ifelse(status == "gap", trolley_grey_light, golden_sand),
         val_lab = glue("VLC (n) - {(Yes)}\nTotal -{(total)}"), 
         full_lab = glue("{is_current_mode_preferred}\n\n{val_lab}"))

viz_preference_vlc <- preference_vlc %>%
  #filter(fundingagency == "USAID") %>% 
  ggplot(aes(fill = fill_color, values = value)) +
  geom_waffle(color = "white", size = 1, n_rows = 10, flip = TRUE, na.rm = TRUE) +
  geom_text(aes(x = 5, y  = 12, label = percent(VLC, 1), color = golden_sand),
            family = "Source Sans Pro SemiBold", size = 14/.pt) +
  #  facet_wrap(~region, nrow = 1, strip.position = "bottom") +
  facet_wrap(~fct_reorder(full_lab, value,min), nrow = 2, strip.position = "bottom") +
  expand_limits(y = 14) +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_alpha_identity() +
  coord_equal() +
  labs(x= NULL, y = NULL,
       subtitle = "Shared of participants with updated viral loads") +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        strip.text.x = element_text(hjust = .5),
        panel.spacing = unit(1, "pt")) +
  guides(fill = guide_legend(reverse = TRUE))


viz_preference_itt + viz_preference_vlc +viz_preference_vls +
  plot_annotation(
    title = "Female participants in their preferred DSD model had fewer missed appointments and more updated viral loads and suppressed viral loads than Female participants not in their preferred DSD model",
    caption = glue("Source: DSD Patient Reference Export, 16 Feb 2022
                   Updated: {today} | {ref_id}"),
    theme = si_style())

si_save(glue("Graphics/dsd-preference-by-outcomes-MEN-{today}.svg"))
si_save(glue("Images/dsd-preference-by-outcomes-{today}.png"))


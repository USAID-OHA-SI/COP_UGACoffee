## PROJECT: COP_UGA
## AUTHOR:  K. Srikanth | USAID
## PURPOSE: DSD Analysis - regional/CHW/MMD viz
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

#  how does IIT differ by region and by age? ------------------------------

#Regional analysis

library(waffle)

df_region_iit <- df_clean %>% 
  filter(region != "Central" & region != "Mid Western") %>% 
  count(region, missed_appt) %>% drop_na() %>% 
  pivot_wider(names_from = missed_appt, values_from = n) %>% 
  mutate(total = No + Yes,
         IIT = Yes / total) %>%
  mutate(prct_round_iit = round(IIT*100),
         gap = 100-prct_round_iit) %>% 
  pivot_longer(c(prct_round_iit, gap), names_to = "status") %>% 
  mutate(fill_color = ifelse(status == "gap", trolley_grey_light, scooter),
         val_lab = glue("IIT (n) - {(Yes)}\nTotal -{(total)}"), 
         full_lab = glue("{region}\n\n{val_lab}"))

viz_region_itt <- df_region_iit %>%
  #filter(fundingagency == "USAID") %>% 
  ggplot(aes(fill = fill_color, values = value)) +
  geom_waffle(color = "white", size = 1, n_rows = 10, flip = TRUE) +
  geom_text(aes(x = 5, y  = 12, label = percent(IIT, 1), color = scooter),
            family = "Source Sans Pro SemiBold", size = 14/.pt) +
  #  facet_wrap(~region, nrow = 1, strip.position = "bottom") +
  facet_wrap(~fct_reorder(full_lab, value,min), nrow = 1, strip.position = "bottom") +
  expand_limits(y = 14) +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_alpha_identity() +
  coord_equal() +
  labs(x= NULL, y = NULL,
       title = "Share of participants with missed appointments in the last 12 months") +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        strip.text.x = element_text(hjust = .5),
        panel.spacing = unit(1, "pt")) +
  guides(fill = guide_legend(reverse = TRUE))

region_vls <- df_clean %>% 
  filter(region != "Central" & region != "Mid Western") %>% 
  count(region, vls) %>% drop_na() %>% 
  pivot_wider(names_from = vls, values_from = n) %>% 
  mutate(total = No + Yes,
         VLS = Yes / total) %>% 
  mutate(prct_round_vls = round(VLS*100),
         gap = 100-prct_round_vls) %>% 
  pivot_longer(c(prct_round_vls, gap), names_to = "status") %>% 
  mutate(fill_color = ifelse(status == "gap", trolley_grey_light, moody_blue),
         val_lab = glue("VLS (n) - {(Yes)}\nTotal -{(total)}"), 
         full_lab = glue("{region}\n\n{val_lab}"))

viz_region_vls <- region_vls %>%
  #filter(fundingagency == "USAID") %>% 
  ggplot(aes(fill = fill_color, values = value)) +
  geom_waffle(color = "white", size = 1, n_rows = 10, flip = TRUE) +
  geom_text(aes(x = 5, y  = 12, label = percent(VLS, 1), color = moody_blue),
            family = "Source Sans Pro SemiBold", size = 14/.pt) +
  #  facet_wrap(~region, nrow = 1, strip.position = "bottom") +
  facet_wrap(~fct_reorder(full_lab, value,min), nrow = 1, strip.position = "bottom") +
  expand_limits(y = 14) +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_alpha_identity() +
  coord_equal() +
  labs(x= NULL, y = NULL,
       title = "Share of participants with suppressed viral loads",
       caption = glue("Source: DSD Patient Reference Export, 16 Feb 2022")) +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        strip.text.x = element_text(hjust = .5),
        panel.spacing = unit(1, "pt")) +
  guides(fill = guide_legend(reverse = TRUE))


region_vlc <- df_clean %>% 
  filter(region != "Central" & region != "Mid Western") %>% 
  count(region, updated_vl) %>% drop_na() %>% 
  pivot_wider(names_from = updated_vl, values_from = n) %>% 
  mutate(total = No + Yes,
         VLC = Yes / total) %>% 
  mutate(prct_round_vlc = round(VLC*100),
         gap = 100-prct_round_vlc) %>% 
  pivot_longer(c(prct_round_vlc, gap), names_to = "status") %>% 
  mutate(fill_color = ifelse(status == "gap", trolley_grey_light, golden_sand),
         val_lab = glue("VLC (n) - {(Yes)}\nTotal -{(total)}"), 
         full_lab = glue("{region}\n\n{val_lab}"))

viz_region_vlc <- region_vlc %>%
  #filter(fundingagency == "USAID") %>% 
  ggplot(aes(fill = fill_color, values = value)) +
  geom_waffle(color = "white", size = 1, n_rows = 10, flip = TRUE) +
  geom_text(aes(x = 5, y  = 12, label = percent(VLC, 1), color = golden_sand),
            family = "Source Sans Pro SemiBold", size = 14/.pt) +
  #  facet_wrap(~region, nrow = 1, strip.position = "bottom") +
  facet_wrap(~fct_reorder(full_lab, value,min), nrow = 1, strip.position = "bottom") +
  expand_limits(y = 14) +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_alpha_identity() +
  coord_equal() +
  labs(x= NULL, y = NULL,
       title = "Shared of participants with updated viral loads") +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        strip.text.x = element_text(hjust = .5),
        panel.spacing = unit(1, "pt")) +
  guides(fill = guide_legend(reverse = TRUE))


viz_region_itt/viz_region_vlc/viz_region_vls + plot_layout(widths = c(0.5, 2)) +
  plot_annotation(
    
    caption = glue("Source: DSD Patient Reference Export, 16 Feb 2022"),
    theme = si_style())

ggsave("COP22_UGA_outcomes-by-regions.png", path = "Images")


#CHW health outcomes - peds/adults ---------------------------

chw_viz <- df_clean %>% 
  #filter(agecoarse == "15+") %>% 
  count(CHW_present, missed_appt) %>% drop_na() %>% 
  pivot_wider(names_from = missed_appt, values_from = n) %>% 
  mutate(total = No + Yes,
         IIT = Yes / total) %>% select(CHW_present, IIT)

chw_vls <- df_clean %>% 
  # filter(agecoarse == "15+") %>% 
  count(CHW_present, vls) %>% drop_na() %>% 
  pivot_wider(names_from = vls, values_from = n) %>% 
  mutate(total = No + Yes,
         VLS = Yes / total)  %>% select(CHW_present, VLS)

chw_vlc <- df_clean %>% 
  #filter(agecoarse == "15+") %>% 
  count(CHW_present, updated_vl) %>% drop_na() %>% 
  pivot_wider(names_from = updated_vl, values_from = n) %>% 
  mutate(total = No + Yes,
         VLC = Yes / total)  %>% select(CHW_present, VLC)

chw_viz <- chw_viz %>% 
  left_join(chw_vls, by = c("CHW_present")) %>% 
  left_join(chw_vlc, by = c("CHW_present")) %>% 
  pivot_longer(cols = c(IIT: VLC), names_to = "outcomes") %>% 
  pivot_wider(names_from = CHW_present, values_from = value) %>% 
  rename(no_CHW = No,
         yes_CHW = Yes)

chw_viz %>% 
  mutate(fill_color = case_when(outcomes == "IIT" ~ scooter,
                                outcomes == "VLS" ~ moody_blue,
                                outcomes == "VLC" ~ golden_sand)) %>% 
  ggplot(aes(x = outcomes)) +
  geom_col(aes(y = no_CHW),fill = trolley_grey_light, alpha = 1, position = position_nudge(x = -nudge_space)) +
  geom_col(aes(y = yes_CHW, fill = fill_color), position = position_nudge(x = nudge_space)) +
  # geom_label(aes(y = 0, label = percent(share, 1), fill = share, 
  #                color = ifelse(share > 0.3, "white", grey90k)), 
  #            vjust = 1.3, 
  #            size = 10/.pt, 
  #            label.size = NA, family = "Source Sans Pro") +
  geom_text(aes(y = no_CHW, label = percent(no_CHW, 1)), family = "Source Sans Pro",
            hjust = 1.5, vjust = -1, na.rm = TRUE) +
  geom_text(aes(y = yes_CHW, label = percent(yes_CHW, 1)), family = "Source Sans Pro",
            hjust = 0, vjust = -1, na.rm = TRUE) +
  scale_y_continuous(labels = percent) +
  scale_fill_identity() +
  scale_color_identity() +
  si_style_ygrid() +
  labs(x = NULL, y = NULL, title = "Attachment to a community health worker is associated with fewer missed appointments,
   more participants with updated viral loads, and more participants achieving viral suppression",
       subtitle = "Uganda COP22 Patient Preference Analysis | USAID",
       caption = "Source: DSD Patient Reference Export, 16 Feb 2022") +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))

si_save("COP22_UGA_CHW.svg", path = "Graphics")

#86% of pediatric patients are attached to CHW
df_clean %>% 
  # filter(agecoarse == "<15") %>% 
  count(agecoarse, CHW_present) %>% drop_na() %>% 
  # pivot_longer(cols = c(IIT: VLC), names_to = "outcomes") %>% 
  pivot_wider(names_from = CHW_present, values_from = n) %>% 
  mutate(total = No + Yes,
         VLC = Yes / total) %>% 
  rename(no_CHW = No,
         yes_CHW = Yes)


#MMD -------------------------

df_mmd <- df_clean %>% 
  count(MMDfreq_last_visit, preferred_MMDfreq) %>%drop_na()

df_mmd$total <- sum(df_mmd$n)

df_mmd <- df_mmd %>% 
  mutate(share = n/total)
# rename(dsd = current_arv_mode, current_n = n, total = total, current_share = share) %>% select(current_n, current_share)

df_mmd %>% 
  # filter(current_share != "Other") %>% 
  mutate(fill_color = burnt_sienna) %>% 
  ggplot(aes(x = MMDfreq_last_visit)) +
  #geom_col(aes(y = current_n),fill = trolley_grey_light, alpha = 1, position = position_nudge(x = -nudge_space)) +
  geom_col(aes(y = n, fill = fill_color)) +
  # geom_label(aes(y = 0, label = percent(share, 1), fill = share, 
  #                color = ifelse(share > 0.3, "white", grey90k)), 
  #            vjust = 1.3, 
  #            size = 10/.pt, 
  #            label.size = NA, family = "Source Sans Pro") +
  geom_text(aes(y = n, label = percent(share, 1)), family = "Source Sans Pro",
            hjust = .5, vjust = -1, na.rm = TRUE) +
  scale_y_continuous(labels = label_number_si()) +
  scale_fill_identity() +
  scale_color_identity() +
  si_style_ygrid() +
  labs(x = NULL, y = NULL, title = "Over half of the participants in the sample were on and preferred 3-5 month of ART dispensing",
       subtitle = "No difference between last month MMD frequency and preferred MMD frequency",
       caption = "Source: DSD Patient Reference Export, 16 Feb 2022") +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))

si_save("Graphics/mmd-preference.svg")